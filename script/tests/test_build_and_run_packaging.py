#!/usr/bin/env python3

from __future__ import annotations

import os
import pathlib
import plistlib
import shlex
import subprocess
import sys
import tempfile
import unittest


BUILD_SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "build_and_run.sh"
BUILD_SCRIPT_TEXT = BUILD_SCRIPT.read_text(encoding="utf-8")


def extract_source_digest_program() -> str:
    marker = (
        'source_digest() {\n'
        '  python3 - "$APP_DIR" "$SAMPLE_CSV" "$ICON_SOURCE" <<\'PY\'\n'
    )
    start = BUILD_SCRIPT_TEXT.index(marker) + len(marker)
    end = BUILD_SCRIPT_TEXT.index("\nPY\n}\n", start)
    return BUILD_SCRIPT_TEXT[start:end]


def extract_icon_packaging_block() -> str:
    start_marker = "APP_ICON_PRESENT=false\n"
    end_marker = '} >"$STAGED_INFO_PLIST"\n'
    start = BUILD_SCRIPT_TEXT.index(start_marker)
    end = BUILD_SCRIPT_TEXT.index(end_marker, start) + len(end_marker)
    return BUILD_SCRIPT_TEXT[start:end]


class BuildAndRunPackagingTests(unittest.TestCase):
    def run_source_digest(
        self,
        app_root: pathlib.Path,
        sample_csv: pathlib.Path,
        icon_source: pathlib.Path,
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [
                sys.executable,
                "-c",
                extract_source_digest_program(),
                str(app_root),
                str(sample_csv),
                str(icon_source),
            ],
            check=False,
            capture_output=True,
            text=True,
        )

    def make_digest_fixture(
        self,
        root: pathlib.Path,
    ) -> tuple[pathlib.Path, pathlib.Path, pathlib.Path]:
        app_root = root / "macos" / "SpikeTrainPatternDetectorMac"
        sources = app_root / "Sources"
        resources = app_root / "Resources"
        sources.mkdir(parents=True)
        resources.mkdir()
        (app_root / "Package.swift").write_text(
            "// icon digest fixture\n",
            encoding="utf-8",
        )
        (sources / "main.swift").write_text(
            "print(\"fixture\")\n",
            encoding="utf-8",
        )
        sample_csv = root / "inst" / "extdata" / "sample.csv"
        sample_csv.parent.mkdir(parents=True)
        sample_csv.write_text("time,train\n0.0,1\n", encoding="utf-8")
        icon_source = resources / "AppIcon.icns"
        return app_root, sample_csv, icon_source

    def run_icon_packaging_block(
        self,
        icon_source: pathlib.Path,
        resources: pathlib.Path,
        info_plist: pathlib.Path,
    ) -> subprocess.CompletedProcess[str]:
        variables = {
            "ICON_SOURCE": icon_source,
            "STAGED_APP_RESOURCES": resources,
            "STAGED_INFO_PLIST": info_plist,
        }
        assignments = "\n".join(
            f"{name}={shlex.quote(str(value))}"
            for name, value in variables.items()
        )
        program = f"""\
set -euo pipefail
validate_managed_path() {{ :; }}
APP_NAME=SpikeTrainPatternDetectorMac
BUNDLE_ID=com.spiketrainpattern.detector.mac
MIN_SYSTEM_VERSION=14.0
BUILD_IDENTIFIER=git-test-source-test
{assignments}
{extract_icon_packaging_block()}
"""
        return subprocess.run(
            ["/bin/bash", "-c", program],
            check=False,
            capture_output=True,
            text=True,
        )

    def test_source_digest_tracks_optional_icon_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            app_root, sample_csv, icon_source = self.make_digest_fixture(root)

            icon_source.write_bytes(b"icon-version-one")
            first = self.run_source_digest(app_root, sample_csv, icon_source)
            self.assertEqual(first.returncode, 0, first.stderr)

            icon_source.write_bytes(b"icon-version-two")
            second = self.run_source_digest(app_root, sample_csv, icon_source)
            self.assertEqual(second.returncode, 0, second.stderr)
            self.assertNotEqual(first.stdout.strip(), second.stdout.strip())

            icon_source.unlink()
            absent = self.run_source_digest(app_root, sample_csv, icon_source)
            self.assertEqual(absent.returncode, 0, absent.stderr)
            self.assertNotEqual(second.stdout.strip(), absent.stdout.strip())

    def test_source_digest_rejects_symlink_icon(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            app_root, sample_csv, icon_source = self.make_digest_fixture(root)
            icon_target = root / "outside.icns"
            icon_target.write_bytes(b"outside-icon")
            os.symlink(icon_target, icon_source)

            result = self.run_source_digest(app_root, sample_csv, icon_source)

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("source digest refuses symlink path", result.stderr)

    def test_present_icon_is_copied_and_declared_in_plist(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            icon_source = root / "AppIcon.icns"
            icon_source.write_bytes(b"verified-icon")
            resources = root / "staged" / "Resources"
            resources.mkdir(parents=True)
            info_plist = root / "staged" / "Info.plist"

            result = self.run_icon_packaging_block(
                icon_source,
                resources,
                info_plist,
            )

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(
                (resources / "AppIcon.icns").read_bytes(),
                icon_source.read_bytes(),
            )
            with info_plist.open("rb") as stream:
                metadata = plistlib.load(stream)
            self.assertEqual(metadata["CFBundleIconFile"], "AppIcon")
            self.assertEqual(
                metadata["CFBundleIdentifier"],
                "com.spiketrainpattern.detector.mac",
            )

    def test_absent_icon_is_not_copied_or_declared_in_plist(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            icon_source = root / "missing" / "AppIcon.icns"
            resources = root / "staged" / "Resources"
            resources.mkdir(parents=True)
            info_plist = root / "staged" / "Info.plist"

            result = self.run_icon_packaging_block(
                icon_source,
                resources,
                info_plist,
            )

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertFalse((resources / "AppIcon.icns").exists())
            with info_plist.open("rb") as stream:
                metadata = plistlib.load(stream)
            self.assertNotIn("CFBundleIconFile", metadata)
            self.assertEqual(
                metadata["CFBundleIdentifier"],
                "com.spiketrainpattern.detector.mac",
            )

    def test_symlink_icon_is_rejected_before_packaging(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            target = root / "target.icns"
            target.write_bytes(b"outside-icon")
            icon_source = root / "AppIcon.icns"
            os.symlink(target, icon_source)
            resources = root / "staged" / "Resources"
            resources.mkdir(parents=True)
            info_plist = root / "staged" / "Info.plist"

            result = self.run_icon_packaging_block(
                icon_source,
                resources,
                info_plist,
            )

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("app icon is not a real file", result.stderr)
            self.assertFalse(info_plist.exists())


if __name__ == "__main__":
    unittest.main()
