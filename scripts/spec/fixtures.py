#!/usr/bin/env python3
"""Conformance fixtures of BARUCH: one Markdown/HTML pair per spec example (plan szaghi/BARUCH#1, step 1.4).

Usage: python3 -I scripts/spec/fixtures.py [--suite NAME]            write the fixtures of every suite (or of one)
       python3 -I scripts/spec/fixtures.py [--suite NAME] --check    compare them with the pinned source (exit 1 if not)
       python3 -I scripts/spec/fixtures.py --list                    print the suites, one per line:
                                                                      NAME<TAB>DIRECTORY<TAB>EXTENSIONS (comma
                                                                      separated, `-` for none), DIRECTORY relative to
                                                                      the repository root (read by scripts/spec.sh)

Each suite has a pinned source file, committed with its fixtures and checked against the SHA-256 recorded below, and
a fixtures directory holding

- ``index.tsv``: one line per example, ``example<TAB>section<TAB>start_line<TAB>end_line`` (the example number, the
  section of the spec it belongs to, and the lines of the spec it is written on), after a header line;
- ``examples/NNNN.md`` and ``examples/NNNN.html``: the example's Markdown and its expected HTML, ``NNNN`` the example
  number on four digits, the bytes exactly as in the source (UTF-8, LF line endings, tabs kept).

The fixtures are generated, never edited: ``--check`` regenerates them in memory and fails if a file differs, is
missing or is not expected, or if the source does not match its SHA-256. A later suite (GFM, VitePress) adds an entry
to ``SUITES`` with its own reader; ``scripts/spec.sh`` names the same suites.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
SPEC_DIR = ROOT / "src" / "tests" / "spec"


@dataclass(frozen=True)
class Example:
    """One example of a spec: its number, section, lines in the spec, Markdown and expected HTML."""

    number: int
    section: str
    start_line: int
    end_line: int
    markdown: str
    html: str


@dataclass(frozen=True)
class Suite:
    """A conformance suite: its pinned source (URL, SHA-256), its fixtures directory and the reader of its source.

    ``title`` names the specification in the reports; ``extensions`` are the BARUCH extensions the driver enables for
    the suite (none for CommonMark: the core alone).
    """

    name: str
    title: str
    directory: Path
    source: str
    url: str
    sha256: str
    reader: Callable[[bytes], list[Example]]
    extensions: tuple[str, ...] = ()


def read_commonmark_json(data: bytes) -> list[Example]:
    """Read the examples of a CommonMark ``spec.json`` (as ``spec_tests.py --dump-tests`` writes it).

    Parameters
    ----------
    data : bytes
        The JSON file.

    Returns
    -------
    list[Example]
        The examples, in the order of the file.
    """
    return [
        Example(
            number=int(e["example"]),
            section=str(e["section"]),
            start_line=int(e["start_line"]),
            end_line=int(e["end_line"]),
            markdown=str(e["markdown"]),
            html=str(e["html"]),
        )
        for e in json.loads(data.decode("utf-8"))
    ]


SUITES: dict[str, Suite] = {
    "commonmark": Suite(
        name="commonmark",
        title="CommonMark 0.31.2",
        directory=SPEC_DIR / "commonmark-0.31.2",
        source="spec.json",
        url="https://spec.commonmark.org/0.31.2/spec.json",
        sha256="d431b29d97b6f73e69d547109cf5081578fac931e72afe95639ebe766c1b2a20",
        reader=read_commonmark_json,
    ),
}


def expected_files(suite: Suite) -> dict[str, bytes]:
    """Return the fixtures of a suite as they must be: relative path to bytes.

    Parameters
    ----------
    suite : Suite
        The suite.

    Returns
    -------
    dict[str, bytes]
        ``index.tsv`` and every ``examples/NNNN.md``, ``examples/NNNN.html``.

    Raises
    ------
    SystemExit
        If the source is missing or does not match its SHA-256.
    """
    path = suite.directory / suite.source
    if not path.is_file():
        sys.exit(f"fixtures: {suite.name}: missing source {path.relative_to(ROOT)} (download {suite.url})")
    data = path.read_bytes()
    digest = hashlib.sha256(data).hexdigest()
    if digest != suite.sha256:
        sys.exit(f"fixtures: {suite.name}: {path.relative_to(ROOT)} has SHA-256 {digest}, pinned {suite.sha256}")
    examples = suite.reader(data)
    numbers = [e.number for e in examples]
    if len(set(numbers)) != len(numbers):
        sys.exit(f"fixtures: {suite.name}: duplicated example numbers")
    index = ["example\tsection\tstart_line\tend_line"]
    files: dict[str, bytes] = {}
    for e in examples:
        if "\t" in e.section or "\n" in e.section:
            sys.exit(f"fixtures: {suite.name}: example {e.number}: a tab or line feed in the section name")
        index.append(f"{e.number}\t{e.section}\t{e.start_line}\t{e.end_line}")
        files[f"examples/{e.number:04d}.md"] = e.markdown.encode("utf-8")
        files[f"examples/{e.number:04d}.html"] = e.html.encode("utf-8")
    files["index.tsv"] = ("\n".join(index) + "\n").encode("utf-8")
    return files


def existing_files(suite: Suite) -> set[str]:
    """Return the generated files present on disk: ``index.tsv`` and the files under ``examples/``."""
    present = {
        p.relative_to(suite.directory).as_posix() for p in (suite.directory / "examples").glob("*") if p.is_file()
    }
    if (suite.directory / "index.tsv").is_file():
        present.add("index.tsv")
    return present


def write(suite: Suite) -> None:
    """Write the fixtures of a suite, removing the generated files that are no longer expected."""
    files = expected_files(suite)
    (suite.directory / "examples").mkdir(parents=True, exist_ok=True)
    for name in sorted(existing_files(suite) - files.keys()):
        (suite.directory / name).unlink()
    for name, data in files.items():
        target = suite.directory / name
        if not target.is_file() or target.read_bytes() != data:
            target.write_bytes(data)
    print(f"fixtures: {suite.name}: {(len(files) - 1) // 2} examples in {suite.directory.relative_to(ROOT)}")


def check(suite: Suite) -> int:
    """Compare the fixtures of a suite with its pinned source; return the number of differences (each printed)."""
    files = expected_files(suite)
    present = existing_files(suite)
    differences = 0
    for name in sorted(files.keys() | present):
        target = suite.directory / name
        if name not in files:
            print(f"fixtures: {suite.name}: unexpected {target.relative_to(ROOT)}")
        elif name not in present:
            print(f"fixtures: {suite.name}: missing {target.relative_to(ROOT)}")
        elif target.read_bytes() != files[name]:
            print(f"fixtures: {suite.name}: differs {target.relative_to(ROOT)}")
        else:
            continue
        differences += 1
    if differences == 0:
        print(f"fixtures: {suite.name}: {(len(files) - 1) // 2} examples equal to the pinned {suite.source}")
    return differences


def main() -> int:
    """Write or check the fixtures of the suites named on the command line."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--suite", choices=sorted(SUITES), help="one suite (default: every suite)")
    parser.add_argument("--check", action="store_true", help="compare with the pinned source, exit 1 on a difference")
    parser.add_argument("--list", action="store_true", help="print the suites: name, directory, extensions")
    args = parser.parse_args()
    if args.list:
        for s in SUITES.values():
            print(f"{s.name}\t{s.directory.relative_to(ROOT).as_posix()}\t{','.join(s.extensions) or '-'}")
        return 0
    suites = [SUITES[args.suite]] if args.suite else list(SUITES.values())
    if args.check:
        return 1 if sum(check(s) for s in suites) > 0 else 0
    for s in suites:
        write(s)
    return 0


if __name__ == "__main__":
    sys.exit(main())
