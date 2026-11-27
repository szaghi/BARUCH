#!/usr/bin/env python3
"""Oracle data of BARUCH's input sanitising (plan szaghi/BARUCH#1, step 1.2).

Usage: python3 -I scripts/input_corpus.py            write src/tests/data/input_corpus.txt
       python3 -I scripts/input_corpus.py --check    compare it with a fresh generation (exit 1 if it differs)

The byte strings are StringiFor's UTF-8 corpus (src/third_party/StringiFor/scripts/utf8/corpus.py, fetched by
`fobis fetch`: ASCII, every class of ill-formed UTF-8 of Unicode Table 3-7, Latin-1 text, random mixes and random
binary data, deterministic). Each one is loaded under several encodings, and the expected UTF-8 is computed by Python,
the reference:

1. the encoding: the one passed (`encoding=`), else the one named by a byte order mark, else UTF-8; `UTF-16` and
   `UTF-32` without an endianness take it from a byte order mark, big endian without one (RFC 2781);
2. `bytes.decode(codec, 'replace')`: every ill-formed sequence becomes U+FFFD, as Python decides;
3. one leading U+FEFF (the byte order mark, now decoded) is removed;
4. every U+0000 becomes U+FFFD.

The test program `src/tests/baruch_test_input.F90` loads every case into a `markdown_document` and checks that its
buffer is byte-identical to the expected one. Each line of the data file is "CATEGORY<TAB>ENCODING<TAB>INPUT<TAB>
EXPECTED", the bytes in lowercase hexadecimal (empty for none), ENCODING `-` for none (the byte order mark decides).
"""

from __future__ import annotations

import argparse
import sys
from collections import Counter
from collections.abc import Iterator
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CORPUS_DIR = ROOT / "src" / "third_party" / "StringiFor" / "scripts" / "utf8"
DATA = ROOT / "src" / "tests" / "data" / "input_corpus.txt"

# byte order marks, in the order StringiFor's bom_encoding reads them (UTF-32LE before UTF-16LE)
BOMS: list[tuple[bytes, str]] = [
    (b"\xff\xfe\x00\x00", "UTF-32LE"),
    (b"\x00\x00\xfe\xff", "UTF-32BE"),
    (b"\xef\xbb\xbf", "UTF-8"),
    (b"\xff\xfe", "UTF-16LE"),
    (b"\xfe\xff", "UTF-16BE"),
]
# canonical name (StringiFor's canonical_encoding) -> Python codec
CODECS: dict[str, str] = {
    "UTF-8": "utf-8",
    "US-ASCII": "ascii",
    "ISO-8859-1": "latin-1",
    "windows-1252": "cp1252",
    "UTF-16LE": "utf-16-le",
    "UTF-16BE": "utf-16-be",
    "UTF-32LE": "utf-32-le",
    "UTF-32BE": "utf-32-be",
}
ALL_ENCODINGS = [
    "UTF-8",
    "US-ASCII",
    "ISO-8859-1",
    "windows-1252",
    "UTF-16",
    "UTF-16LE",
    "UTF-16BE",
    "UTF-32",
    "UTF-32LE",
    "UTF-32BE",
]


def resolve(data: bytes, encoding: str) -> str:
    """Return the canonical encoding the bytes are read in.

    Parameters
    ----------
    data : bytes
        Input bytes.
    encoding : str
        Encoding passed, `-` for none.
    """
    if encoding == "-":
        for bom, name in BOMS:
            if data.startswith(bom):
                return name
        return "UTF-8"
    if encoding == "UTF-16":
        return "UTF-16LE" if data.startswith(b"\xff\xfe") else "UTF-16BE"
    if encoding == "UTF-32":
        return "UTF-32LE" if data.startswith(b"\xff\xfe\x00\x00") else "UTF-32BE"
    return encoding


def reference(data: bytes, encoding: str) -> bytes:
    """Return the sanitised UTF-8 BARUCH must keep for the bytes loaded in an encoding.

    Parameters
    ----------
    data : bytes
        Input bytes.
    encoding : str
        Encoding passed, `-` for none.
    """
    text = data.decode(CODECS[resolve(data, encoding)], "replace")
    text = text.removeprefix("﻿")
    return text.replace("\x00", "�").encode("utf-8")


def cases() -> Iterator[tuple[str, str, bytes]]:
    """Yield the cases as (category, encoding, bytes)."""
    sys.path.insert(0, str(CORPUS_DIR))
    from corpus import FIXED, cases as corpus_cases  # noqa: PLC0415

    corpus = list(corpus_cases())
    fixed = {data for _, _, data in FIXED}
    for category, data in corpus:  # the gate: every case, the encoding read or declared UTF-8
        yield category, "-", data
        yield category, "UTF-8", data
        if data in fixed:
            yield category + "+bom", "-", b"\xef\xbb\xbf" + data
    for category, data in corpus:  # the fixed cases and the binary ones in every other encoding
        if data in fixed or category == "random-binary":
            for encoding in ALL_ENCODINGS[1:]:
                yield category, encoding, data
    for category, data in corpus:  # the well-formed cases written in UTF-16 and UTF-32, read by their mark
        try:
            text = data.decode("utf-8", "strict")
        except UnicodeDecodeError:
            continue
        for bom, name in BOMS:
            if name != "UTF-8":
                yield category + "@" + name, "-", bom + text.encode(CODECS[name])


def generate() -> str:
    """Return the data file's text."""
    return "".join(
        f"{category}\t{encoding}\t{data.hex()}\t{reference(data, encoding).hex()}\n"
        for category, encoding, data in cases()
    )


def main() -> int:
    """Command line entry point."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true", help="compare the data file with a fresh generation")
    args = parser.parse_args()
    if not CORPUS_DIR.is_dir():
        print(f"input_corpus: {CORPUS_DIR} not found (run fobis fetch)", file=sys.stderr)
        return 2
    text = generate()
    lines = text.splitlines()
    by_encoding = Counter(line.split("\t")[1] for line in lines)
    replaced = sum(1 for line in lines if "efbfbd" in line.split("\t")[3])
    summary = ", ".join(f"{name} {count}" for name, count in sorted(by_encoding.items()))
    if args.check:
        same = DATA.is_file() and DATA.read_text(encoding="ascii") == text
        state = "up to date" if same else "DIFFERS"
        print(f"input_corpus: {len(lines)} cases ({summary}); {replaced} with U+FFFD; data file {state}")
        return 0 if same else 1
    DATA.parent.mkdir(parents=True, exist_ok=True)
    DATA.write_text(text, encoding="ascii")
    print(f"input_corpus: {len(lines)} cases ({summary}); {replaced} with U+FFFD; written {DATA.relative_to(ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
