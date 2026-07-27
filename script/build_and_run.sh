#!/usr/bin/env bash
set -euo pipefail

MODE="${1:-run}"
APP_NAME="SpikeTrainPatternDetectorMac"
BUNDLE_ID="com.spiketrainpattern.detector.mac"
MIN_SYSTEM_VERSION="14.0"

case "$MODE" in
  run|--debug|debug|--logs|logs|--telemetry|telemetry|--verify|verify)
    ;;
  *)
    echo "usage: $0 [run|--debug|--logs|--telemetry|--verify]" >&2
    exit 2
    ;;
esac

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"
APP_DIR="$ROOT_DIR/macos/SpikeTrainPatternDetectorMac"
DIST_DIR="$ROOT_DIR/dist"
APP_BUNDLE="$DIST_DIR/$APP_NAME.app"
APP_CONTENTS="$APP_BUNDLE/Contents"
APP_MACOS="$APP_CONTENTS/MacOS"
APP_RESOURCES="$APP_CONTENTS/Resources"
APP_BINARY="$APP_MACOS/$APP_NAME"
INFO_PLIST="$APP_CONTENTS/Info.plist"
SAMPLE_CSV="$ROOT_DIR/inst/extdata/Grechishnikova_STN_2017_subset.csv"
GIT_COMMIT="$(git -C "$ROOT_DIR" rev-parse HEAD)"
LOCK_FILE="$DIST_DIR/.build_and_run.lock"
BUNDLE_TRANSACTION_HELPER="$ROOT_DIR/script/bundle_transaction.py"

validate_managed_path() {
  python3 - "$ROOT_DIR" "$1" <<'PY'
import os
import pathlib
import stat
import sys

root = pathlib.Path(sys.argv[1]).resolve(strict=True)
target = pathlib.Path(sys.argv[2])
if not target.is_absolute():
    raise SystemExit(f"managed path is not absolute: {target}")
if any(part in (".", "..") for part in target.parts):
    raise SystemExit(f"managed path contains a traversal component: {target}")

try:
    relative = target.relative_to(root)
except ValueError:
    raise SystemExit(f"managed path escapes repository root: {target}")
if not relative.parts:
    raise SystemExit("refusing to manage the repository root itself")

cursor = root
for part in relative.parts:
    cursor = cursor / part
    try:
        mode = os.lstat(cursor).st_mode
    except FileNotFoundError:
        continue
    if stat.S_ISLNK(mode):
        raise SystemExit(f"managed path contains symlink component: {cursor}")

resolved_parent = target.parent.resolve(strict=False)
if resolved_parent != root and root not in resolved_parent.parents:
    raise SystemExit(f"managed path parent resolves outside repository: {target}")
print(target)
PY
}

sync_directory() {
  python3 - "$1" <<'PY'
import os
import stat
import sys

path = sys.argv[1]
mode = os.lstat(path).st_mode
if stat.S_ISLNK(mode) or not stat.S_ISDIR(mode):
    raise SystemExit(f"directory sync requires a real directory: {path}")
descriptor = os.open(
    path,
    os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW,
)
try:
    os.fsync(descriptor)
finally:
    os.close(descriptor)
PY
}

source_digest() {
  python3 - "$APP_DIR" "$SAMPLE_CSV" <<'PY'
import hashlib
import os
import pathlib
import stat
import sys

root = pathlib.Path(sys.argv[1])
sample_csv = pathlib.Path(sys.argv[2])

def require_real_path(path, *, directory):
    try:
        mode = os.lstat(path).st_mode
    except FileNotFoundError:
        raise SystemExit(f"required source path is missing: {path}")
    if stat.S_ISLNK(mode):
        raise SystemExit(f"source digest refuses symlink path: {path}")
    if directory and not stat.S_ISDIR(mode):
        raise SystemExit(f"required source path is not a directory: {path}")
    if not directory and not stat.S_ISREG(mode):
        raise SystemExit(f"required source path is not a regular file: {path}")

require_real_path(root, directory=True)
package_swift = root / "Package.swift"
sources_root = root / "Sources"
require_real_path(package_swift, directory=False)
require_real_path(sources_root, directory=True)
paths = [package_swift]
package_resolved = root / "Package.resolved"
if os.path.lexists(package_resolved):
    require_real_path(package_resolved, directory=False)
    paths.append(package_resolved)

for directory in (sources_root,):
    for current_root, directory_names, file_names in os.walk(
        directory,
        followlinks=False,
    ):
        current = pathlib.Path(current_root)
        for name in directory_names:
            path = current / name
            if stat.S_ISLNK(os.lstat(path).st_mode):
                raise SystemExit(f"source digest refuses symlink directory: {path}")
        for name in file_names:
            path = current / name
            mode = os.lstat(path).st_mode
            if stat.S_ISLNK(mode):
                raise SystemExit(f"source digest refuses symlink file: {path}")
            if stat.S_ISREG(mode):
                paths.append(path)

if os.path.lexists(sample_csv):
    mode = os.lstat(sample_csv).st_mode
    if stat.S_ISLNK(mode):
        raise SystemExit(f"source digest refuses symlink sample: {sample_csv}")
    if not stat.S_ISREG(mode):
        raise SystemExit(f"sample is not a regular file: {sample_csv}")
    paths.append(sample_csv)

digest = hashlib.sha256()
for path in sorted(paths, key=lambda item: item.as_posix()):
    if path == sample_csv:
        relative_text = "repository/" + sample_csv.name
    else:
        relative_text = path.relative_to(root).as_posix()
    relative = relative_text.encode()
    data = path.read_bytes()
    digest.update(len(relative).to_bytes(8, "big"))
    digest.update(relative)
    digest.update(len(data).to_bytes(8, "big"))
    digest.update(data)
print(digest.hexdigest())
PY
}

validate_managed_path "$DIST_DIR" >/dev/null
mkdir -p "$DIST_DIR"
sync_directory "$ROOT_DIR"
if [[ "${STPD_BUNDLE_TRANSACTION_TESTING:-0}" == "1"
    && "${STPD_BUILD_AND_RUN_TEST_DIST_PARENT_FSYNC_FAILURE:-0}" == "1" ]]; then
  echo "injected failure after dist parent fsync" >&2
  exit 86
fi
validate_managed_path "$DIST_DIR" >/dev/null
validate_managed_path "$APP_DIR" >/dev/null
validate_managed_path "$APP_DIR/Package.swift" >/dev/null
validate_managed_path "$APP_DIR/Sources" >/dev/null
validate_managed_path "$SAMPLE_CSV" >/dev/null
validate_managed_path "$APP_BUNDLE" >/dev/null
validate_managed_path "$LOCK_FILE" >/dev/null
validate_managed_path "$BUNDLE_TRANSACTION_HELPER" >/dev/null
if [[ ! -f "$BUNDLE_TRANSACTION_HELPER" || -L "$BUNDLE_TRANSACTION_HELPER" ]]; then
  echo "bundle transaction helper is not a real file: $BUNDLE_TRANSACTION_HELPER" >&2
  exit 1
fi

exec 9>"$LOCK_FILE"
if ! /usr/bin/lockf -s -t 0 9; then
  echo "another build/package invocation already owns $LOCK_FILE" >&2
  exit 75
fi
LOCK_HELD=true

SOURCE_DIGEST="$(source_digest)"
BUILD_IDENTIFIER="git-${GIT_COMMIT}-source-${SOURCE_DIGEST}"
STAGE_ROOT=""
SCRATCH_ROOT=""
STAGED_APP_BUNDLE=""
PUBLISH_STATE=""
PUBLISH_STATE_FILE=""
PUBLISH_COMMITTED=false
DEBUG_LAUNCH_MARKER=""

release_lock() {
  if [[ "$LOCK_HELD" == true ]]; then
    exec 9>&-
    LOCK_HELD=false
  fi
}

cleanup_stage() {
  if [[ "$PUBLISH_COMMITTED" != true
      && -n "$PUBLISH_STATE_FILE"
      && ( -e "$PUBLISH_STATE_FILE" || -L "$PUBLISH_STATE_FILE" ) ]]; then
    echo "refusing to remove stage root with an unfinished publication journal: $PUBLISH_STATE_FILE" >&2
    return 1
  fi
  if [[ -n "$STAGE_ROOT" && ( -e "$STAGE_ROOT" || -L "$STAGE_ROOT" ) ]]; then
    validate_managed_path "$STAGE_ROOT" >/dev/null
    rm -rf -- "$STAGE_ROOT"
  fi
  STAGE_ROOT=""
}

cleanup_scratch() {
  if [[ -n "$SCRATCH_ROOT" && ( -e "$SCRATCH_ROOT" || -L "$SCRATCH_ROOT" ) ]]; then
    validate_managed_path "$SCRATCH_ROOT" >/dev/null
    rm -rf -- "$SCRATCH_ROOT"
  fi
  SCRATCH_ROOT=""
}

recover_publish_state() {
  local recovered_state
  if [[ -n "$PUBLISH_STATE" ]]; then
    return 0
  fi
  if [[ -z "$PUBLISH_STATE_FILE"
      || ! -e "$PUBLISH_STATE_FILE"
      || -L "$PUBLISH_STATE_FILE" ]]; then
    return 0
  fi
  if ! recovered_state="$(bundle_transaction state)"; then
    echo "failed to recover publication state from $PUBLISH_STATE_FILE" >&2
    return 1
  fi
  case "$recovered_state" in
    swapped|created)
      PUBLISH_STATE="$recovered_state"
      ;;
    *)
      echo "invalid recovered publication state: $recovered_state" >&2
      return 1
      ;;
  esac
}

cleanup() {
  local status=$?
  local preserve_stage=false
  set +e
  if [[ "$PUBLISH_COMMITTED" != true ]] && ! recover_publish_state; then
    status=1
    preserve_stage=true
  fi
  if [[ "$PUBLISH_COMMITTED" != true
      && "$PUBLISH_STATE" == "publishing_unknown" ]]; then
    echo "publication outcome is unknown; preserving transaction state at $STAGE_ROOT" >&2
    status=1
    preserve_stage=true
  elif [[ -n "$PUBLISH_STATE" && "$PUBLISH_COMMITTED" != true ]]; then
    if ! stop_packaged_instances >/dev/null 2>&1; then
      echo "could not confirm that the failed packaged app stopped before rollback; preserving transaction state at $STAGE_ROOT" >&2
      status=1
      preserve_stage=true
    elif ! bundle_transaction rollback "$PUBLISH_STATE"; then
      echo "failed to roll back packaged application; preserving transaction state at $STAGE_ROOT" >&2
      status=1
      preserve_stage=true
    else
      PUBLISH_STATE=""
    fi
  fi
  if [[ "$preserve_stage" != true ]]; then
    cleanup_stage
  fi
  cleanup_scratch
  release_lock
  return "$status"
}
trap cleanup EXIT

packaged_instance_pids() {
  local pid state command process_rows
  if [[ "${STPD_BUNDLE_TRANSACTION_TESTING:-0}" == "1"
      && "${STPD_BUILD_AND_RUN_TEST_PROCESS_ENUMERATION_FAILURE:-0}" == "1" ]]; then
    echo "injected process-enumeration failure" >&2
    return 1
  fi
  if ! process_rows="$(/bin/ps -ww -axo pid=,state=,comm=)"; then
    echo "failed to enumerate running processes" >&2
    return 1
  fi
  while read -r pid state command; do
    [[ -n "$pid" ]] || continue
    [[ "$state" == Z* ]] && continue
    [[ "$command" == "$APP_BINARY" ]] && printf '%s\n' "$pid"
  done <<<"$process_rows"
  return 0
}

stop_packaged_instances() {
  local pid pids remaining attempt
  if ! pids="$(packaged_instance_pids)"; then
    return 1
  fi
  while IFS= read -r pid; do
    [[ -n "$pid" ]] || continue
    if kill -0 "$pid" >/dev/null 2>&1; then
      kill -TERM "$pid" >/dev/null 2>&1 || {
        kill -0 "$pid" >/dev/null 2>&1 && return 1
      }
    fi
  done <<<"$pids"

  for attempt in {1..50}; do
    if ! remaining="$(packaged_instance_pids)"; then
      return 1
    fi
    [[ -z "$remaining" ]] && return 0
    sleep 0.1
  done

  # A GUI process can decline or defer SIGTERM while AppKit is busy. Escalate
  # only the exact executable-path matches that remain after the grace period.
  while IFS= read -r pid; do
    [[ -n "$pid" ]] || continue
    if kill -0 "$pid" >/dev/null 2>&1; then
      kill -KILL "$pid" >/dev/null 2>&1 || {
        kill -0 "$pid" >/dev/null 2>&1 && return 1
      }
    fi
  done <<<"$remaining"

  for attempt in {1..20}; do
    if ! remaining="$(packaged_instance_pids)"; then
      return 1
    fi
    [[ -z "$remaining" ]] && return 0
    sleep 0.1
  done

  echo "existing packaged app survived TERM and KILL: $remaining" >&2
  return 1
}

bundle_transaction() {
  local action="$1"
  local prior_state="${2:-}"
  python3 "$BUNDLE_TRANSACTION_HELPER" \
    "$ROOT_DIR" \
    "$STAGED_APP_BUNDLE" \
    "$APP_BUNDLE" \
    "$action" \
    "$prior_state" \
    "$PUBLISH_STATE_FILE"
}

publish_bundle_transaction() {
  local published_state
  PUBLISH_STATE="publishing_unknown"
  if ! published_state="$(bundle_transaction publish)"; then
    PUBLISH_STATE=""
    if ! recover_publish_state; then
      PUBLISH_STATE="publishing_unknown"
    elif [[ -z "$PUBLISH_STATE" ]]; then
      PUBLISH_STATE="publishing_unknown"
    fi
    return 1
  fi
  case "$published_state" in
    swapped|created)
      PUBLISH_STATE="$published_state"
      ;;
    *)
      echo "bundle transaction returned invalid state: $published_state" >&2
      PUBLISH_STATE="publishing_unknown"
      return 1
      ;;
  esac
}

validate_built_executable() {
  python3 - "$SCRATCH_ROOT" "$1" <<'PY'
import os
import pathlib
import stat
import sys

scratch = pathlib.Path(sys.argv[1])
executable = pathlib.Path(sys.argv[2])

scratch_mode = os.lstat(scratch).st_mode
if stat.S_ISLNK(scratch_mode) or not stat.S_ISDIR(scratch_mode):
    raise SystemExit(f"SwiftPM scratch path is not a real directory: {scratch}")
scratch = scratch.resolve(strict=True)

if not executable.is_absolute():
    raise SystemExit(f"built executable path is not absolute: {executable}")
try:
    relative = executable.relative_to(scratch)
except ValueError:
    raise SystemExit(
        f"built executable escapes the isolated SwiftPM scratch path: {executable}"
    )
if not relative.parts:
    raise SystemExit("built executable unexpectedly equals the scratch directory")

cursor = scratch
for part in relative.parts:
    cursor = cursor / part
    try:
        mode = os.lstat(cursor).st_mode
    except FileNotFoundError:
        raise SystemExit(f"built executable path component is missing: {cursor}")
    if stat.S_ISLNK(mode):
        raise SystemExit(f"built executable path contains a symlink: {cursor}")

mode = os.lstat(executable).st_mode
if not stat.S_ISREG(mode):
    raise SystemExit(f"built executable is not a regular file: {executable}")
if not os.access(executable, os.X_OK):
    raise SystemExit(f"built executable is not executable: {executable}")
PY
}

commit_publication() {
  if [[ -z "$PUBLISH_STATE_FILE"
      || ! -f "$PUBLISH_STATE_FILE"
      || -L "$PUBLISH_STATE_FILE" ]]; then
    echo "cannot commit publication without a real transaction journal" >&2
    return 1
  fi
  bundle_transaction clear >/dev/null
  PUBLISH_COMMITTED=true
  PUBLISH_STATE=""
  cleanup_stage
}

recover_abandoned_stage_transactions() {
  local stage journal recovered_state nullglob_was_set=false
  local -a stage_roots=()
  local -a journaled_stage_roots=()

  if shopt -q nullglob; then
    nullglob_was_set=true
  fi
  shopt -s nullglob
  stage_roots=("$DIST_DIR"/."$APP_NAME".stage.*)
  if [[ "$nullglob_was_set" != true ]]; then
    shopt -u nullglob
  fi

  if (( ${#stage_roots[@]} == 0 )); then
    return 0
  fi

  for stage in "${stage_roots[@]}"; do
    validate_managed_path "$stage" >/dev/null
    if [[ -L "$stage" || ! -d "$stage" ]]; then
      echo "abandoned stage path is not a real directory: $stage" >&2
      return 1
    fi
    journal="$stage/.publish_state"
    if [[ -e "$journal" || -L "$journal" ]]; then
      if [[ -L "$journal" || ! -f "$journal" ]]; then
        echo "abandoned transaction journal is not a real file: $journal" >&2
        return 1
      fi
      journaled_stage_roots+=("$stage")
    else
      # No durable journal means publication never started, or already committed.
      rm -rf -- "$stage"
    fi
  done

  if (( ${#journaled_stage_roots[@]} > 1 )); then
    echo "multiple unfinished app publication transactions require manual inspection:" >&2
    printf '  %s\n' "${journaled_stage_roots[@]}" >&2
    return 1
  fi
  if (( ${#journaled_stage_roots[@]} == 0 )); then
    return 0
  fi

  STAGE_ROOT="${journaled_stage_roots[0]}"
  STAGED_APP_BUNDLE="$STAGE_ROOT/$APP_NAME.app"
  PUBLISH_STATE_FILE="$STAGE_ROOT/.publish_state"
  DEBUG_LAUNCH_MARKER="$STAGE_ROOT/.debug_launch_confirmed"
  PUBLISH_STATE=""
  PUBLISH_COMMITTED=false

  if ! recovered_state="$(bundle_transaction state)"; then
    echo "failed to read abandoned publication journal: $PUBLISH_STATE_FILE" >&2
    return 1
  fi
  case "$recovered_state" in
    swapped|created)
      PUBLISH_STATE="$recovered_state"
      ;;
    *)
      echo "invalid abandoned publication state: $recovered_state" >&2
      return 1
      ;;
  esac

  if ! stop_packaged_instances; then
    echo "could not stop the packaged app before startup recovery" >&2
    return 1
  fi
  if ! bundle_transaction rollback "$PUBLISH_STATE"; then
    echo "startup recovery failed; preserving transaction state at $STAGE_ROOT" >&2
    return 1
  fi

  PUBLISH_STATE=""
  PUBLISH_COMMITTED=true
  cleanup_stage
  PUBLISH_COMMITTED=false
  STAGED_APP_BUNDLE=""
  PUBLISH_STATE_FILE=""
  DEBUG_LAUNCH_MARKER=""
  echo "recovered abandoned packaged-app publication"
}

debug_packaged_app() {
  if [[ -z "$DEBUG_LAUNCH_MARKER"
      || -e "$DEBUG_LAUNCH_MARKER"
      || -L "$DEBUG_LAUNCH_MARKER" ]]; then
    echo "debug launch marker path is not pristine: $DEBUG_LAUNCH_MARKER" >&2
    return 1
  fi
  STPD_DEBUG_LAUNCH_MARKER="$DEBUG_LAUNCH_MARKER" lldb \
    -o "process launch --stop-at-entry" \
    -o "script import lldb, os, pathlib; p = lldb.debugger.GetSelectedTarget().GetProcess(); assert p.IsValid() and p.GetProcessID() != lldb.LLDB_INVALID_PROCESS_ID and p.GetState() in (lldb.eStateStopped, lldb.eStateRunning); pathlib.Path(os.environ['STPD_DEBUG_LAUNCH_MARKER']).write_text(str(p.GetProcessID()), encoding='utf-8')" \
    -- "$APP_BINARY" 9>&-

  python3 - "$DEBUG_LAUNCH_MARKER" <<'PY'
import os
import pathlib
import stat
import sys

marker = pathlib.Path(sys.argv[1])
try:
    mode = os.lstat(marker).st_mode
except FileNotFoundError:
    raise SystemExit("debugger exited before confirming an app launch")
if stat.S_ISLNK(mode) or not stat.S_ISREG(mode):
    raise SystemExit(f"debug launch marker is not a real file: {marker}")
try:
    pid = int(marker.read_text(encoding="utf-8").strip())
except ValueError as error:
    raise SystemExit("debug launch marker does not contain a PID") from error
if pid <= 0:
    raise SystemExit(f"debug launch marker contains an invalid PID: {pid}")
PY
}

recover_abandoned_stage_transactions
if [[ "${STPD_BUILD_AND_RUN_RECOVERY_ONLY:-0}" == "1" ]]; then
  if [[ "${STPD_BUNDLE_TRANSACTION_TESTING:-0}" != "1" ]]; then
    echo "recovery-only mode is restricted to transaction tests" >&2
    exit 2
  fi
  exit 0
fi

SCRATCH_ROOT="$(mktemp -d "$DIST_DIR/.swiftpm.XXXXXX")"
validate_managed_path "$SCRATCH_ROOT" >/dev/null

pushd "$APP_DIR" >/dev/null
swift build --scratch-path "$SCRATCH_ROOT" 9>&-
BUILD_BINARY="$(swift build --scratch-path "$SCRATCH_ROOT" --show-bin-path 9>&-)/$APP_NAME"
popd >/dev/null

validate_built_executable "$BUILD_BINARY"

POST_BUILD_DIGEST="$(source_digest)"
if [[ "$POST_BUILD_DIGEST" != "$SOURCE_DIGEST" ]]; then
  echo "source changed during build; refusing to package a mixed snapshot" >&2
  exit 1
fi

STAGE_ROOT="$(mktemp -d "$DIST_DIR/.${APP_NAME}.stage.XXXXXX")"
validate_managed_path "$STAGE_ROOT" >/dev/null
STAGED_APP_BUNDLE="$STAGE_ROOT/$APP_NAME.app"
PUBLISH_STATE_FILE="$STAGE_ROOT/.publish_state"
DEBUG_LAUNCH_MARKER="$STAGE_ROOT/.debug_launch_confirmed"
STAGED_APP_CONTENTS="$STAGED_APP_BUNDLE/Contents"
STAGED_APP_MACOS="$STAGED_APP_CONTENTS/MacOS"
STAGED_APP_RESOURCES="$STAGED_APP_CONTENTS/Resources"
STAGED_APP_BINARY="$STAGED_APP_MACOS/$APP_NAME"
STAGED_INFO_PLIST="$STAGED_APP_CONTENTS/Info.plist"
validate_managed_path "$STAGED_APP_BUNDLE" >/dev/null
validate_managed_path "$PUBLISH_STATE_FILE" >/dev/null
validate_managed_path "$DEBUG_LAUNCH_MARKER" >/dev/null

mkdir -p "$STAGED_APP_MACOS" "$STAGED_APP_RESOURCES"
cp "$BUILD_BINARY" "$STAGED_APP_BINARY"
chmod +x "$STAGED_APP_BINARY"

if [[ -f "$SAMPLE_CSV" ]]; then
  cp "$SAMPLE_CSV" "$STAGED_APP_RESOURCES/"
fi

cat >"$STAGED_INFO_PLIST" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleExecutable</key>
  <string>$APP_NAME</string>
  <key>CFBundleIdentifier</key>
  <string>$BUNDLE_ID</string>
  <key>CFBundleName</key>
  <string>$APP_NAME</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>LSMinimumSystemVersion</key>
  <string>$MIN_SYSTEM_VERSION</string>
  <key>STPDBuildIdentifier</key>
  <string>$BUILD_IDENTIFIER</string>
  <key>NSPrincipalClass</key>
  <string>NSApplication</string>
</dict>
</plist>
PLIST

POST_PACKAGE_DIGEST="$(source_digest)"
if [[ "$POST_PACKAGE_DIGEST" != "$SOURCE_DIGEST" ]]; then
  echo "source changed during packaging; leaving the existing app untouched" >&2
  exit 1
fi

cleanup_scratch
stop_packaged_instances
validate_managed_path "$APP_BUNDLE" >/dev/null
publish_bundle_transaction

launch_packaged_app() {
  local attempt pids count launched_pid stable_pids
  if ! pids="$(packaged_instance_pids)"; then
    return 1
  fi
  if [[ -n "$pids" ]]; then
    echo "packaged app unexpectedly running immediately before launch" >&2
    return 1
  fi
  /usr/bin/open -n "$APP_BUNDLE" || {
    echo "open failed for packaged app: $APP_BUNDLE" >&2
    return 1
  }
  for attempt in {1..50}; do
    if ! pids="$(packaged_instance_pids)"; then
      return 1
    fi
    if [[ -n "$pids" ]]; then
      count="$(printf '%s\n' "$pids" | wc -l | tr -d ' ')"
      if [[ "$count" != "1" ]]; then
        echo "expected one newly launched packaged app, found $count: $pids" >&2
        return 1
      fi
      launched_pid="$pids"
      sleep 1
      if ! stable_pids="$(packaged_instance_pids)"; then
        return 1
      fi
      if [[ "$stable_pids" != "$launched_pid" ]]; then
        echo "packaged app did not remain stable after launch: initial=$launched_pid current=$stable_pids" >&2
        return 1
      fi
      printf '%s\n' "$launched_pid"
      return 0
    fi
    sleep 0.1
  done
  echo "new packaged app did not launch from exact executable: $APP_BINARY" >&2
  return 1
}

case "$MODE" in
  run)
    NEW_PID="$(launch_packaged_app)"
    commit_publication
    release_lock
    echo "launched $APP_BINARY (pid $NEW_PID)"
    ;;
  --debug|debug)
    debug_packaged_app
    commit_publication
    release_lock
    ;;
  --logs|logs)
    NEW_PID="$(launch_packaged_app)"
    commit_publication
    release_lock
    echo "launched $APP_BINARY (pid $NEW_PID)"
    /usr/bin/log stream --info --style compact --predicate "process == \"$APP_NAME\""
    ;;
  --telemetry|telemetry)
    NEW_PID="$(launch_packaged_app)"
    commit_publication
    release_lock
    echo "launched $APP_BINARY (pid $NEW_PID)"
    /usr/bin/log stream --info --style compact --predicate "subsystem == \"$BUNDLE_ID\""
    ;;
  --verify|verify)
    NEW_PID="$(launch_packaged_app)"
    if ! VERIFIED_PIDS="$(packaged_instance_pids)"; then
      exit 1
    fi
    [[ "$VERIFIED_PIDS" == "$NEW_PID" ]]
    commit_publication
    release_lock
    echo "verified $APP_BINARY (pid $NEW_PID)"
    ;;
esac
