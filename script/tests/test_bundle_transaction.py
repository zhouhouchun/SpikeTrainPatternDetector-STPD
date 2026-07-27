#!/usr/bin/env python3

from __future__ import annotations

import json
import importlib.util
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "bundle_transaction.py"
REPOSITORY_ROOT = pathlib.Path(__file__).resolve().parents[2]
TRANSACTION_SPEC = importlib.util.spec_from_file_location(
    "stpd_bundle_transaction",
    SCRIPT,
)
if TRANSACTION_SPEC is None or TRANSACTION_SPEC.loader is None:
    raise RuntimeError(f"cannot load bundle transaction module: {SCRIPT}")
TRANSACTION = importlib.util.module_from_spec(TRANSACTION_SPEC)
TRANSACTION_SPEC.loader.exec_module(TRANSACTION)


class BundleTransactionTests(unittest.TestCase):
    def run_transaction(
        self,
        root: pathlib.Path,
        staged: pathlib.Path,
        destination: pathlib.Path,
        action: str,
        journal: pathlib.Path,
        *,
        prior_state: str = "",
        faults: str = "",
    ) -> subprocess.CompletedProcess[str]:
        environment = os.environ.copy()
        if faults:
            environment["STPD_BUNDLE_TRANSACTION_TESTING"] = "1"
            environment["STPD_BUNDLE_TRANSACTION_FAULTS"] = faults
        return subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                str(root),
                str(staged),
                str(destination),
                action,
                prior_state,
                str(journal),
            ],
            check=False,
            capture_output=True,
            text=True,
            env=environment,
        )

    def make_bundle(self, path: pathlib.Path, payload: str) -> None:
        path.mkdir(parents=True)
        (path / "identity.txt").write_text(payload, encoding="utf-8")

    def journal_record(self, path: pathlib.Path) -> dict[str, object]:
        return json.loads(path.read_text(encoding="utf-8"))

    def bundle_payload(self, path: pathlib.Path) -> str:
        return (path / "identity.txt").read_text(encoding="utf-8")

    def assert_swap_topology(
        self,
        staged: pathlib.Path,
        destination: pathlib.Path,
        *,
        staged_payload: str,
        destination_payload: str,
    ) -> None:
        self.assertEqual(self.bundle_payload(staged), staged_payload)
        self.assertEqual(self.bundle_payload(destination), destination_payload)

    def swap_fixture(
        self,
        root: pathlib.Path,
    ) -> tuple[pathlib.Path, pathlib.Path, pathlib.Path]:
        stage_root = root / "dist" / ".stage"
        stage_root.mkdir(parents=True)
        staged = stage_root / "SpikeTrainPatternDetectorMac.app"
        destination = root / "dist" / "SpikeTrainPatternDetectorMac.app"
        journal = stage_root / ".publish_state"
        self.make_bundle(staged, "new")
        self.make_bundle(destination, "old")
        return staged, destination, journal

    def test_successful_swap_keeps_structured_journal_until_explicit_commit(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)

            published = self.run_transaction(
                root,
                staged,
                destination,
                "publish",
                journal,
            )
            self.assertEqual(published.returncode, 0, published.stderr)
            self.assertEqual(published.stdout.strip(), "swapped")
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="old",
                destination_payload="new",
            )
            record = self.journal_record(journal)
            self.assertEqual(record["version"], 1)
            self.assertEqual(record["operation"], "swapped")
            self.assertEqual(record["phase"], "published")
            self.assertIsInstance(record["new_identity"], str)
            self.assertIsInstance(record["old_identity"], str)

            cleared = self.run_transaction(
                root,
                staged,
                destination,
                "clear",
                journal,
            )
            self.assertEqual(cleared.returncode, 0, cleared.stderr)
            self.assertFalse(journal.exists())
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="old",
                destination_payload="new",
            )

    def test_publish_crashes_are_recoverable_from_actual_topology(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            parent = pathlib.Path(directory)
            for fault, expected_phase in (
                ("after_publish_rename", "prepared"),
                ("after_publish_journal", "published"),
                ("after_publish_parent_fsync", "published"),
            ):
                with self.subTest(fault=fault):
                    root = parent / fault
                    root.mkdir()
                    staged, destination, journal = self.swap_fixture(root)

                    failed = self.run_transaction(
                        root,
                        staged,
                        destination,
                        "publish",
                        journal,
                        faults=fault,
                    )
                    self.assertNotEqual(failed.returncode, 0)
                    self.assert_swap_topology(
                        staged,
                        destination,
                        staged_payload="old",
                        destination_payload="new",
                    )
                    self.assertEqual(
                        self.journal_record(journal)["phase"],
                        expected_phase,
                    )

                    recovered = self.run_transaction(
                        root,
                        staged,
                        destination,
                        "rollback",
                        journal,
                    )
                    self.assertEqual(recovered.returncode, 0, recovered.stderr)
                    self.assert_swap_topology(
                        staged,
                        destination,
                        staged_payload="new",
                        destination_payload="old",
                    )
                    self.assertFalse(journal.exists())

    def test_prepublication_parent_fsync_checkpoint_is_rollback_recoverable(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)

            failed = self.run_transaction(
                root,
                staged,
                destination,
                "publish",
                journal,
                faults="after_prepared_parent_fsync",
            )

            self.assertNotEqual(failed.returncode, 0)
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="new",
                destination_payload="old",
            )
            self.assertEqual(
                self.journal_record(journal)["phase"],
                "prepared",
            )

            recovered = self.run_transaction(
                root,
                staged,
                destination,
                "rollback",
                journal,
            )
            self.assertEqual(recovered.returncode, 0, recovered.stderr)
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="new",
                destination_payload="old",
            )
            self.assertFalse(journal.exists())

    def test_prepared_journal_survives_repeated_short_writes(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-short-write-prepared-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)
            original_write = os.write
            write_lengths: list[int] = []

            def short_write(descriptor: int, payload: bytes) -> int:
                written = original_write(descriptor, payload[:7])
                write_lengths.append(written)
                return written

            arguments = [
                str(SCRIPT),
                str(root),
                str(staged),
                str(destination),
                "publish",
                "",
                str(journal),
            ]
            environment = {
                "STPD_BUNDLE_TRANSACTION_TESTING": "1",
                "STPD_BUNDLE_TRANSACTION_FAULTS": "after_prepared_parent_fsync",
            }
            with mock.patch.dict(os.environ, environment, clear=False):
                with mock.patch.object(
                    TRANSACTION.os,
                    "write",
                    side_effect=short_write,
                ):
                    with self.assertRaisesRegex(
                        OSError,
                        "after_prepared_parent_fsync",
                    ):
                        TRANSACTION.main(arguments)

            self.assertGreater(len(write_lengths), 1)
            self.assertTrue(all(0 < length <= 7 for length in write_lengths))
            self.assertEqual(self.journal_record(journal)["phase"], "prepared")
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="new",
                destination_payload="old",
            )

    def test_published_journal_survives_repeated_short_writes(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-short-write-published-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)
            original_write = os.write
            write_lengths: list[int] = []

            def short_write(descriptor: int, payload: bytes) -> int:
                written = original_write(descriptor, payload[:7])
                write_lengths.append(written)
                return written

            arguments = [
                str(SCRIPT),
                str(root),
                str(staged),
                str(destination),
                "publish",
                "",
                str(journal),
            ]
            with mock.patch.object(
                TRANSACTION.os,
                "write",
                side_effect=short_write,
            ):
                result = TRANSACTION.main(arguments)

            self.assertEqual(result, 0)
            self.assertGreater(len(write_lengths), 2)
            self.assertTrue(all(0 < length <= 7 for length in write_lengths))
            self.assertEqual(self.journal_record(journal)["phase"], "published")
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="old",
                destination_payload="new",
            )

    def test_write_all_rejects_zero_byte_progress(self):
        read_descriptor, write_descriptor = os.pipe()
        try:
            with mock.patch.object(TRANSACTION.os, "write", return_value=0):
                with self.assertRaisesRegex(
                    OSError,
                    "journal write made invalid progress",
                ):
                    TRANSACTION.write_all(write_descriptor, b"journal")
        finally:
            os.close(read_descriptor)
            os.close(write_descriptor)

    def test_rollback_after_reverse_rename_is_idempotent(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)
            published = self.run_transaction(
                root, staged, destination, "publish", journal
            )
            self.assertEqual(published.returncode, 0, published.stderr)

            failed = self.run_transaction(
                root,
                staged,
                destination,
                "rollback",
                journal,
                faults="after_reverse_rename",
            )
            self.assertNotEqual(failed.returncode, 0)
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="new",
                destination_payload="old",
            )
            self.assertEqual(
                self.journal_record(journal)["phase"],
                "rollback_started",
            )

            recovered = self.run_transaction(
                root, staged, destination, "rollback", journal
            )
            self.assertEqual(recovered.returncode, 0, recovered.stderr)
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="new",
                destination_payload="old",
            )
            self.assertFalse(journal.exists())

    def test_rollback_before_journal_clear_is_idempotent(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)
            published = self.run_transaction(
                root, staged, destination, "publish", journal
            )
            self.assertEqual(published.returncode, 0, published.stderr)

            failed = self.run_transaction(
                root,
                staged,
                destination,
                "rollback",
                journal,
                faults="before_journal_clear",
            )
            self.assertNotEqual(failed.returncode, 0)
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="new",
                destination_payload="old",
            )
            self.assertEqual(
                self.journal_record(journal)["phase"],
                "rolled_back",
            )

            recovered = self.run_transaction(
                root, staged, destination, "rollback", journal
            )
            self.assertEqual(recovered.returncode, 0, recovered.stderr)
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="new",
                destination_payload="old",
            )
            self.assertFalse(journal.exists())

    def test_commit_before_journal_clear_is_idempotent(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)
            published = self.run_transaction(
                root, staged, destination, "publish", journal
            )
            self.assertEqual(published.returncode, 0, published.stderr)

            failed = self.run_transaction(
                root,
                staged,
                destination,
                "clear",
                journal,
                faults="before_journal_clear",
            )
            self.assertNotEqual(failed.returncode, 0)
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="old",
                destination_payload="new",
            )
            self.assertTrue(journal.exists())

            recovered = self.run_transaction(
                root, staged, destination, "clear", journal
            )
            self.assertEqual(recovered.returncode, 0, recovered.stderr)
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="old",
                destination_payload="new",
            )
            self.assertFalse(journal.exists())

    def test_created_publication_rollback_is_idempotent(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            root = pathlib.Path(directory)
            stage_root = root / "dist" / ".stage"
            stage_root.mkdir(parents=True)
            staged = stage_root / "SpikeTrainPatternDetectorMac.app"
            destination = root / "dist" / "SpikeTrainPatternDetectorMac.app"
            journal = stage_root / ".publish_state"
            self.make_bundle(staged, "new")

            published = self.run_transaction(
                root, staged, destination, "publish", journal
            )
            self.assertEqual(published.returncode, 0, published.stderr)
            self.assertFalse(staged.exists())
            self.assertEqual(self.bundle_payload(destination), "new")
            self.assertEqual(
                self.journal_record(journal)["operation"],
                "created",
            )

            failed = self.run_transaction(
                root,
                staged,
                destination,
                "rollback",
                journal,
                faults="after_reverse_rename",
            )
            self.assertNotEqual(failed.returncode, 0)
            self.assertEqual(self.bundle_payload(staged), "new")
            self.assertFalse(destination.exists())

            recovered = self.run_transaction(
                root, staged, destination, "rollback", journal
            )
            self.assertEqual(recovered.returncode, 0, recovered.stderr)
            self.assertEqual(self.bundle_payload(staged), "new")
            self.assertFalse(destination.exists())
            self.assertFalse(journal.exists())

    def test_publish_fsyncs_staged_tree_before_creating_journal(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)

            failed = self.run_transaction(
                root,
                staged,
                destination,
                "publish",
                journal,
                faults="after_staged_tree_fsync",
            )
            self.assertNotEqual(failed.returncode, 0)
            self.assertFalse(journal.exists())
            self.assert_swap_topology(
                staged,
                destination,
                staged_payload="new",
                destination_payload="old",
            )

    def test_clear_requires_published_journal_phase(self):
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-transaction-test.",
        ) as directory:
            root = pathlib.Path(directory)
            staged, destination, journal = self.swap_fixture(root)

            failed_publish = self.run_transaction(
                root,
                staged,
                destination,
                "publish",
                journal,
                faults="after_publish_rename",
            )
            self.assertNotEqual(failed_publish.returncode, 0)
            self.assertEqual(self.journal_record(journal)["phase"], "prepared")
            rejected = self.run_transaction(
                root,
                staged,
                destination,
                "clear",
                journal,
            )
            self.assertNotEqual(rejected.returncode, 0)
            self.assertIn("phase is not published", rejected.stderr)
            self.assertTrue(journal.exists())

            recovered = self.run_transaction(
                root,
                staged,
                destination,
                "rollback",
                journal,
            )
            self.assertEqual(recovered.returncode, 0, recovered.stderr)

    def test_new_shell_invocation_recovers_abandoned_publication(self):
        build_script = SCRIPT.parent / "build_and_run.sh"
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-restart-test.",
        ) as directory:
            root = pathlib.Path(directory)
            script_directory = root / "script"
            app_directory = (
                root / "macos" / "SpikeTrainPatternDetectorMac"
            )
            (app_directory / "Sources").mkdir(parents=True)
            script_directory.mkdir()
            shutil.copy2(build_script, script_directory / "build_and_run.sh")
            shutil.copy2(SCRIPT, script_directory / "bundle_transaction.py")
            (app_directory / "Package.swift").write_text(
                "// transaction recovery fixture\n",
                encoding="utf-8",
            )
            subprocess.run(
                ["git", "init", "-q"],
                cwd=root,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.email", "transaction@test.invalid"],
                cwd=root,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "Transaction Test"],
                cwd=root,
                check=True,
            )
            subprocess.run(["git", "add", "."], cwd=root, check=True)
            subprocess.run(
                ["git", "commit", "-qm", "fixture"],
                cwd=root,
                check=True,
            )

            stage_root = (
                root
                / "dist"
                / ".SpikeTrainPatternDetectorMac.stage.abandoned"
            )
            stage_root.mkdir(parents=True)
            staged = stage_root / "SpikeTrainPatternDetectorMac.app"
            destination = (
                root / "dist" / "SpikeTrainPatternDetectorMac.app"
            )
            journal = stage_root / ".publish_state"
            self.make_bundle(staged, "new")
            self.make_bundle(destination, "old")
            failed_publish = self.run_transaction(
                root,
                staged,
                destination,
                "publish",
                journal,
                faults="after_publish_rename",
            )
            self.assertNotEqual(failed_publish.returncode, 0)
            self.assertEqual(self.bundle_payload(destination), "new")
            self.assertTrue(journal.exists())

            environment = os.environ.copy()
            environment["STPD_BUNDLE_TRANSACTION_TESTING"] = "1"
            environment["STPD_BUILD_AND_RUN_RECOVERY_ONLY"] = "1"
            recovered = subprocess.run(
                [
                    "/bin/bash",
                    str(script_directory / "build_and_run.sh"),
                    "--verify",
                ],
                cwd=root,
                check=False,
                capture_output=True,
                text=True,
                env=environment,
            )
            self.assertEqual(recovered.returncode, 0, recovered.stderr)
            self.assertIn(
                "recovered abandoned packaged-app publication",
                recovered.stdout,
            )
            self.assertEqual(self.bundle_payload(destination), "old")
            self.assertFalse(stage_root.exists())

    def test_recovery_preserves_transaction_when_process_stop_cannot_be_confirmed(
        self,
    ):
        build_script = SCRIPT.parent / "build_and_run.sh"
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-stop-failure-test.",
        ) as directory:
            root = pathlib.Path(directory)
            script_directory = root / "script"
            app_directory = (
                root / "macos" / "SpikeTrainPatternDetectorMac"
            )
            (app_directory / "Sources").mkdir(parents=True)
            script_directory.mkdir()
            shutil.copy2(build_script, script_directory / "build_and_run.sh")
            shutil.copy2(SCRIPT, script_directory / "bundle_transaction.py")
            (app_directory / "Package.swift").write_text(
                "// stop-failure recovery fixture\n",
                encoding="utf-8",
            )
            subprocess.run(["git", "init", "-q"], cwd=root, check=True)
            subprocess.run(
                ["git", "config", "user.email", "transaction@test.invalid"],
                cwd=root,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "Transaction Test"],
                cwd=root,
                check=True,
            )
            subprocess.run(["git", "add", "."], cwd=root, check=True)
            subprocess.run(
                ["git", "commit", "-qm", "fixture"],
                cwd=root,
                check=True,
            )

            stage_root = (
                root
                / "dist"
                / ".SpikeTrainPatternDetectorMac.stage.stop-failure"
            )
            stage_root.mkdir(parents=True)
            staged = stage_root / "SpikeTrainPatternDetectorMac.app"
            destination = (
                root / "dist" / "SpikeTrainPatternDetectorMac.app"
            )
            journal = stage_root / ".publish_state"
            self.make_bundle(staged, "new")
            self.make_bundle(destination, "old")
            failed_publish = self.run_transaction(
                root,
                staged,
                destination,
                "publish",
                journal,
                faults="after_publish_rename",
            )
            self.assertNotEqual(failed_publish.returncode, 0)
            self.assertEqual(self.bundle_payload(destination), "new")
            self.assertEqual(self.bundle_payload(staged), "old")
            self.assertTrue(journal.exists())

            environment = os.environ.copy()
            environment["STPD_BUNDLE_TRANSACTION_TESTING"] = "1"
            environment["STPD_BUILD_AND_RUN_RECOVERY_ONLY"] = "1"
            environment[
                "STPD_BUILD_AND_RUN_TEST_PROCESS_ENUMERATION_FAILURE"
            ] = "1"
            recovered = subprocess.run(
                [
                    "/bin/bash",
                    str(script_directory / "build_and_run.sh"),
                    "--verify",
                ],
                cwd=root,
                check=False,
                capture_output=True,
                text=True,
                env=environment,
            )

            self.assertNotEqual(recovered.returncode, 0)
            self.assertIn(
                "could not stop the packaged app before startup recovery",
                recovered.stderr,
            )
            self.assertIn(
                "preserving transaction state",
                recovered.stderr,
            )
            self.assertEqual(self.bundle_payload(destination), "new")
            self.assertEqual(self.bundle_payload(staged), "old")
            self.assertTrue(journal.exists())
            self.assertTrue(stage_root.is_dir())

    def test_new_shell_invocation_without_abandoned_publication_is_noop(self):
        build_script = SCRIPT.parent / "build_and_run.sh"
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-empty-recovery-test.",
        ) as directory:
            root = pathlib.Path(directory)
            script_directory = root / "script"
            app_directory = (
                root / "macos" / "SpikeTrainPatternDetectorMac"
            )
            (app_directory / "Sources").mkdir(parents=True)
            script_directory.mkdir()
            shutil.copy2(build_script, script_directory / "build_and_run.sh")
            shutil.copy2(SCRIPT, script_directory / "bundle_transaction.py")
            (app_directory / "Package.swift").write_text(
                "// empty transaction recovery fixture\n",
                encoding="utf-8",
            )
            subprocess.run(
                ["git", "init", "-q"],
                cwd=root,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.email", "transaction@test.invalid"],
                cwd=root,
                check=True,
            )
            subprocess.run(
                ["git", "config", "user.name", "Transaction Test"],
                cwd=root,
                check=True,
            )
            subprocess.run(["git", "add", "."], cwd=root, check=True)
            subprocess.run(
                ["git", "commit", "-qm", "fixture"],
                cwd=root,
                check=True,
            )

            environment = os.environ.copy()
            environment["STPD_BUNDLE_TRANSACTION_TESTING"] = "1"
            environment["STPD_BUILD_AND_RUN_RECOVERY_ONLY"] = "1"
            recovered = subprocess.run(
                [
                    "/bin/bash",
                    str(script_directory / "build_and_run.sh"),
                    "--verify",
                ],
                cwd=root,
                check=False,
                capture_output=True,
                text=True,
                env=environment,
            )
            self.assertEqual(recovered.returncode, 0, recovered.stderr)
            self.assertNotIn(
                "recovered abandoned packaged-app publication",
                recovered.stdout,
            )
            distribution = root / "dist"
            self.assertTrue(distribution.is_dir())
            self.assertEqual(
                list(
                    distribution.glob(
                        ".SpikeTrainPatternDetectorMac.stage.*"
                    )
                ),
                [],
            )
            self.assertEqual(
                list(distribution.rglob(".publish_state")),
                [],
            )
            self.assertFalse(
                (distribution / "SpikeTrainPatternDetectorMac.app").exists()
            )

    def test_dist_parent_is_synced_before_lock_or_transaction_state(self):
        build_script = SCRIPT.parent / "build_and_run.sh"
        with tempfile.TemporaryDirectory(
            dir=REPOSITORY_ROOT,
            prefix=".bundle-dist-fsync-test.",
        ) as directory:
            parent = pathlib.Path(directory)
            for preexisting_dist in (False, True):
                with self.subTest(preexisting_dist=preexisting_dist):
                    root = parent / (
                        "existing-dist" if preexisting_dist else "new-dist"
                    )
                    root.mkdir()
                    script_directory = root / "script"
                    app_directory = (
                        root / "macos" / "SpikeTrainPatternDetectorMac"
                    )
                    (app_directory / "Sources").mkdir(parents=True)
                    script_directory.mkdir()
                    if preexisting_dist:
                        (root / "dist").mkdir()
                    shutil.copy2(
                        build_script,
                        script_directory / "build_and_run.sh",
                    )
                    shutil.copy2(
                        SCRIPT,
                        script_directory / "bundle_transaction.py",
                    )
                    (app_directory / "Package.swift").write_text(
                        "// dist-parent durability fixture\n",
                        encoding="utf-8",
                    )
                    subprocess.run(
                        ["git", "init", "-q"],
                        cwd=root,
                        check=True,
                    )
                    subprocess.run(
                        [
                            "git",
                            "config",
                            "user.email",
                            "transaction@test.invalid",
                        ],
                        cwd=root,
                        check=True,
                    )
                    subprocess.run(
                        [
                            "git",
                            "config",
                            "user.name",
                            "Transaction Test",
                        ],
                        cwd=root,
                        check=True,
                    )
                    subprocess.run(
                        ["git", "add", "."],
                        cwd=root,
                        check=True,
                    )
                    subprocess.run(
                        ["git", "commit", "-qm", "fixture"],
                        cwd=root,
                        check=True,
                    )

                    environment = os.environ.copy()
                    environment["STPD_BUNDLE_TRANSACTION_TESTING"] = "1"
                    environment["STPD_BUILD_AND_RUN_RECOVERY_ONLY"] = "1"
                    environment[
                        "STPD_BUILD_AND_RUN_TEST_DIST_PARENT_FSYNC_FAILURE"
                    ] = "1"
                    failed = subprocess.run(
                        [
                            "/bin/bash",
                            str(script_directory / "build_and_run.sh"),
                            "--verify",
                        ],
                        cwd=root,
                        check=False,
                        capture_output=True,
                        text=True,
                        env=environment,
                    )

                    distribution = root / "dist"
                    self.assertEqual(
                        failed.returncode,
                        86,
                        failed.stderr,
                    )
                    self.assertIn(
                        "injected failure after dist parent fsync",
                        failed.stderr,
                    )
                    self.assertTrue(distribution.is_dir())
                    self.assertFalse(
                        (distribution / ".build_and_run.lock").exists()
                    )
                    self.assertEqual(
                        list(
                            distribution.glob(
                                ".SpikeTrainPatternDetectorMac.stage.*"
                            )
                        ),
                        [],
                    )
                    self.assertEqual(
                        list(distribution.rglob(".publish_state")),
                        [],
                    )


if __name__ == "__main__":
    unittest.main()
