#!/usr/bin/env python3
"""Durable same-filesystem publication transactions for the packaged macOS app."""

from __future__ import annotations

import ctypes
import errno
import json
import os
import pathlib
import stat
import sys


JOURNAL_VERSION = 1
VALID_OPERATIONS = frozenset({"swapped", "created"})
VALID_PHASES = frozenset({
    "prepared",
    "published",
    "rollback_started",
    "rolled_back",
})


def fail(message: str) -> "NoReturn":
    raise SystemExit(message)


def require_real_directory(path: pathlib.Path, label: str) -> None:
    try:
        mode = os.lstat(path).st_mode
    except FileNotFoundError:
        fail(f"{label} is missing: {path}")
    if not stat.S_ISDIR(mode) or stat.S_ISLNK(mode):
        fail(f"{label} is not a real directory: {path}")


def require_managed_path(
    root: pathlib.Path,
    path: pathlib.Path,
    label: str,
) -> None:
    if not path.is_absolute():
        fail(f"{label} is not absolute: {path}")
    if any(part in (".", "..") for part in path.parts):
        fail(f"{label} contains a traversal component: {path}")
    try:
        relative = path.relative_to(root)
    except ValueError:
        fail(f"{label} escapes repository root: {path}")
    if not relative.parts:
        fail(f"{label} unexpectedly equals the repository root")

    cursor = root
    for part in relative.parts:
        cursor = cursor / part
        try:
            mode = os.lstat(cursor).st_mode
        except FileNotFoundError:
            continue
        if stat.S_ISLNK(mode):
            fail(f"{label} contains a symlink component: {cursor}")


def fsync_directory(path: pathlib.Path) -> None:
    descriptor = os.open(path, os.O_RDONLY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def write_all(descriptor: int, payload: bytes) -> None:
    offset = 0
    while offset < len(payload):
        try:
            written = os.write(descriptor, payload[offset:])
        except InterruptedError:
            continue
        if written <= 0 or written > len(payload) - offset:
            raise OSError(errno.EIO, "journal write made invalid progress")
        offset += written


def fsync_regular_file(path: pathlib.Path) -> None:
    descriptor = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    try:
        mode = os.fstat(descriptor).st_mode
        if not stat.S_ISREG(mode):
            fail(f"staged bundle entry is not a regular file: {path}")
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def fsync_bundle_tree(root: pathlib.Path) -> None:
    """Make every staged bundle entry durable before publication begins."""
    require_real_directory(root, "staged bundle")

    def visit(directory: pathlib.Path) -> None:
        # scandir failures are intentionally not caught: an unreadable subtree must abort
        # publication instead of being silently omitted from the durability walk.
        with os.scandir(directory) as iterator:
            entries = sorted(iterator, key=lambda entry: entry.name)
        for entry in entries:
            path = pathlib.Path(entry.path)
            mode = entry.stat(follow_symlinks=False).st_mode
            if stat.S_ISLNK(mode):
                fail(f"staged bundle entry is a symbolic link: {path}")
            if stat.S_ISDIR(mode):
                visit(path)
            elif stat.S_ISREG(mode):
                fsync_regular_file(path)
            else:
                fail(f"staged bundle entry is not a regular file or directory: {path}")
        fsync_directory(directory)

    visit(root)
    fsync_directory(root.parent)


def testing_faults() -> frozenset[str]:
    if os.environ.get("STPD_BUNDLE_TRANSACTION_TESTING") != "1":
        return frozenset()
    return frozenset(
        item.strip()
        for item in os.environ.get(
            "STPD_BUNDLE_TRANSACTION_FAULTS",
            "",
        ).split(",")
        if item.strip()
    )


def inject_fault(name: str, faults: frozenset[str]) -> None:
    if name in faults:
        raise OSError(f"injected transaction fault: {name}")


def main(arguments: list[str]) -> int:
    if len(arguments) != 7:
        fail(
            "usage: bundle_transaction.py "
            "ROOT STAGED DESTINATION ACTION PRIOR_STATE JOURNAL"
        )

    root = pathlib.Path(arguments[1]).resolve(strict=True)
    staged = pathlib.Path(arguments[2])
    destination = pathlib.Path(arguments[3])
    action = arguments[4]
    prior_state = arguments[5]
    journal = pathlib.Path(arguments[6])
    faults = testing_faults()

    for path, label in (
        (staged, "staged bundle"),
        (destination, "destination bundle"),
        (journal, "transaction journal"),
    ):
        require_managed_path(root, path, label)
    require_real_directory(staged.parent, "staged parent")
    require_real_directory(destination.parent, "destination parent")
    require_real_directory(journal.parent, "journal parent")
    if os.stat(staged.parent).st_dev != os.stat(destination.parent).st_dev:
        fail("staged and destination bundles are not on the same filesystem")

    libc = ctypes.CDLL(None, use_errno=True)
    renameatx_np = libc.renameatx_np
    renameatx_np.argtypes = [
        ctypes.c_int,
        ctypes.c_char_p,
        ctypes.c_int,
        ctypes.c_char_p,
        ctypes.c_uint,
    ]
    renameatx_np.restype = ctypes.c_int
    at_fdcwd = -2
    rename_swap = 0x00000002
    rename_excl = 0x00000004

    def rename_with_flag(
        source: pathlib.Path,
        target: pathlib.Path,
        flag: int,
    ) -> None:
        result = renameatx_np(
            at_fdcwd,
            os.fsencode(source),
            at_fdcwd,
            os.fsencode(target),
            flag,
        )
        if result != 0:
            error_number = ctypes.get_errno()
            raise OSError(error_number, os.strerror(error_number))

    def bundle_identity(path: pathlib.Path) -> str | None:
        try:
            mode = os.lstat(path).st_mode
        except FileNotFoundError:
            return None
        if not stat.S_ISDIR(mode) or stat.S_ISLNK(mode):
            fail(f"managed bundle is not a real directory: {path}")
        identity = os.stat(path, follow_symlinks=False)
        return f"{identity.st_dev}:{identity.st_ino}"

    def read_journal() -> dict[str, object] | None:
        try:
            mode = os.lstat(journal).st_mode
        except FileNotFoundError:
            return None
        if not stat.S_ISREG(mode) or stat.S_ISLNK(mode):
            fail(f"transaction journal is not a real file: {journal}")
        try:
            record = json.loads(journal.read_text(encoding="utf-8"))
        except (json.JSONDecodeError, UnicodeDecodeError) as error:
            fail(f"invalid transaction journal JSON: {error}")
        if not isinstance(record, dict):
            fail("transaction journal root is not an object")
        expected_keys = {
            "version",
            "operation",
            "phase",
            "new_identity",
            "old_identity",
        }
        if set(record) != expected_keys:
            fail("transaction journal fields do not match the contract")
        if record["version"] != JOURNAL_VERSION:
            fail(f"unsupported transaction journal version: {record['version']!r}")
        if record["operation"] not in VALID_OPERATIONS:
            fail(f"invalid transaction operation: {record['operation']!r}")
        if record["phase"] not in VALID_PHASES:
            fail(f"invalid transaction phase: {record['phase']!r}")
        if not isinstance(record["new_identity"], str) or not record["new_identity"]:
            fail("transaction journal lacks the staged bundle identity")
        old_identity = record["old_identity"]
        if old_identity is not None and (
            not isinstance(old_identity, str) or not old_identity
        ):
            fail("transaction journal has an invalid prior bundle identity")
        if (
            record["operation"] == "swapped"
            and old_identity is None
        ) or (
            record["operation"] == "created"
            and old_identity is not None
        ):
            fail("transaction journal operation contradicts its prior bundle identity")
        return record

    def write_journal(record: dict[str, object]) -> None:
        if record["operation"] not in VALID_OPERATIONS:
            fail(f"invalid transaction operation: {record['operation']!r}")
        if record["phase"] not in VALID_PHASES:
            fail(f"invalid transaction phase: {record['phase']!r}")
        temporary = journal.with_name(f"{journal.name}.pending")
        if os.path.lexists(temporary):
            mode = os.lstat(temporary).st_mode
            if stat.S_ISLNK(mode) or not stat.S_ISREG(mode):
                fail(f"journal temporary path is not a real file: {temporary}")
            temporary.unlink()
        descriptor = os.open(
            temporary,
            os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
            0o600,
        )
        try:
            payload = json.dumps(
                record,
                ensure_ascii=True,
                sort_keys=True,
                separators=(",", ":"),
            )
            write_all(descriptor, f"{payload}\n".encode("utf-8"))
            os.fsync(descriptor)
        finally:
            os.close(descriptor)
        os.replace(temporary, journal)
        fsync_directory(journal.parent)

    def with_phase(
        record: dict[str, object],
        phase: str,
    ) -> dict[str, object]:
        updated = dict(record)
        updated["phase"] = phase
        return updated

    def topology(record: dict[str, object]) -> str:
        staged_identity = bundle_identity(staged)
        destination_identity = bundle_identity(destination)
        new_identity = record["new_identity"]
        old_identity = record["old_identity"]
        if record["operation"] == "swapped":
            if (
                staged_identity == new_identity
                and destination_identity == old_identity
            ):
                return "restored"
            if (
                staged_identity == old_identity
                and destination_identity == new_identity
            ):
                return "published"
        else:
            if staged_identity == new_identity and destination_identity is None:
                return "restored"
            if staged_identity is None and destination_identity == new_identity:
                return "published"
        return "invalid"

    def clear_journal() -> None:
        if os.path.lexists(journal):
            mode = os.lstat(journal).st_mode
            if stat.S_ISLNK(mode) or not stat.S_ISREG(mode):
                fail(f"transaction journal is not a real file: {journal}")
            journal.unlink()
            fsync_directory(journal.parent)

    def reverse_publication(record: dict[str, object]) -> None:
        current_topology = topology(record)
        if current_topology == "restored":
            fsync_directory(destination.parent)
            write_journal(with_phase(record, "rolled_back"))
            inject_fault("before_journal_clear", faults)
            clear_journal()
            return
        if current_topology != "published":
            fail("transaction paths do not match either recorded topology")

        record = with_phase(record, "rollback_started")
        write_journal(record)
        inject_fault("reverse_rename", faults)
        if record["operation"] == "swapped":
            require_real_directory(staged, "retained prior bundle")
            require_real_directory(destination, "new published bundle")
            rename_with_flag(staged, destination, rename_swap)
        else:
            require_real_directory(destination, "new published bundle")
            if os.path.lexists(staged):
                fail(f"rollback target unexpectedly exists: {staged}")
            rename_with_flag(destination, staged, rename_excl)
        inject_fault("after_reverse_rename", faults)
        fsync_directory(destination.parent)
        write_journal(with_phase(record, "rolled_back"))
        inject_fault("before_journal_clear", faults)
        clear_journal()

    if action == "state":
        record = read_journal()
        if record is not None:
            print(record["operation"])
        return 0

    if action == "clear":
        record = read_journal()
        if record is None:
            fail("cannot commit publication without a transaction journal")
        if record["phase"] != "published":
            fail(
                "cannot commit publication whose transaction phase is not published"
            )
        if topology(record) != "published":
            fail("cannot commit publication whose bundle topology is not published")
        fsync_directory(destination.parent)
        inject_fault("before_journal_clear", faults)
        clear_journal()
        print("cleared")
        return 0

    if action == "publish":
        require_real_directory(staged, "staged bundle")
        if staged.parent.parent != destination.parent:
            fail(
                "staged bundle must live in a transaction directory directly under "
                "the destination parent"
            )
        if journal.parent != staged.parent:
            fail("transaction journal must live beside the staged bundle")
        if read_journal() is not None:
            fail(f"unfinished publication journal already exists: {journal}")
        fsync_bundle_tree(staged)
        inject_fault("after_staged_tree_fsync", faults)
        new_identity = bundle_identity(staged)
        if new_identity is None:
            fail("staged bundle disappeared before publication")
        if os.path.lexists(destination):
            require_real_directory(destination, "published bundle")
            operation = "swapped"
            old_identity = bundle_identity(destination)
        else:
            operation = "created"
            old_identity = None
        record: dict[str, object] = {
            "version": JOURNAL_VERSION,
            "operation": operation,
            "phase": "prepared",
            "new_identity": new_identity,
            "old_identity": old_identity,
        }
        write_journal(record)
        # The journal and bundle are durable inside the staging directory now. Persist the
        # staging-directory entry in `dist` before the publication rename can make it unreachable.
        fsync_directory(destination.parent)
        inject_fault("after_prepared_parent_fsync", faults)
        inject_fault("before_publish_rename", faults)

        if operation == "swapped":
            rename_with_flag(staged, destination, rename_swap)
        else:
            rename_with_flag(staged, destination, rename_excl)
        inject_fault("after_publish_rename", faults)
        record = with_phase(record, "published")
        write_journal(record)
        inject_fault("after_publish_journal", faults)
        fsync_directory(destination.parent)
        inject_fault("after_publish_parent_fsync", faults)
        print(operation)
        return 0

    if action == "rollback":
        record = read_journal()
        if record is None:
            fail("cannot roll back publication without a transaction journal")
        if prior_state and prior_state != record["operation"]:
            fail("requested rollback operation contradicts the transaction journal")
        reverse_publication(record)
        print("rolled_back")
        return 0

    fail(f"unknown bundle transaction action: {action}")


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
