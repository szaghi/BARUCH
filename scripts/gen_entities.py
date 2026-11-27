#!/usr/bin/env python3
"""Generate BARUCH's table of the HTML5 named character references (plan szaghi/BARUCH#1, step 2.2).

Usage: python3 -I scripts/gen_entities.py [--cache DIR]            write the Fortran module and the test data
       python3 -I scripts/gen_entities.py [--cache DIR] --check    fail if either differs from a fresh generation

The source is WHATWG's ``entities.json`` (https://html.spec.whatwg.org/entities.json): the named character references
of HTML, the list CommonMark refers to. It is downloaded once into the cache (default ``build/entities/``,
git-ignored) and verified against the SHA-256 pinned below: a file that differs stops the generation. The JSON is not
committed. To move to a newer table, download it, record its hash in ``SHA256`` and review the diff of the generated
module.

Outputs, both generated and never edited:

- ``src/lib/baruch_entities.F90``: every name as the JSON lists it (2,231: 2,125 ending in ``;``, 106 legacy forms
  without it), without its leading ``&``, in byte order, with the UTF-8 bytes of its ``codepoints``; the lookup is a
  binary search (``entity_lookup``, then ``entity_utf8``).
- ``src/tests/data/entities.txt``: one line per name, ``NAME<TAB>HEX``, the name as in the JSON (with ``&``) and the
  UTF-8 bytes of its ``characters`` in lowercase hexadecimal: the expected data of ``baruch_test_entities``, taken
  from the other field of the JSON than the module's bytes.

CommonMark recognises only the names ending in ``;``; the legacy forms are kept so that the table is the JSON one to
one, and they never change a CommonMark result: each has a ``;`` twin with the same characters (checked here), and the
inline parser looks up ``name;`` only.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import sys
import tempfile
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MODULE = ROOT / "src" / "lib" / "baruch_entities.F90"
DATA = ROOT / "src" / "tests" / "data" / "entities.txt"
URL = "https://html.spec.whatwg.org/entities.json"
SHA256 = "d741d877ac77c4194c4ad526b5b4a19aef8dfe411ab840a466891cdbb9f362e6"  # recorded at the download, 2026-11-27
EXPECTED_COUNT = 2231  # names in the pinned file
NAME_PATTERN = re.compile(r"&[A-Za-z][A-Za-z0-9]*;?")
WIDTH = 116  # longest line of the module
CHUNK_LINES = 120  # continuation lines of one statement at most (the standard allows 255 before Fortran 2023)


class EntitiesError(Exception):
    """The source file is missing, unreadable, differs from its pinned hash or breaks an assumption of the table."""


def fetch(cache: Path) -> bytes:
    """Return the bytes of ``entities.json``, downloading it into the cache if needed and checking its hash.

    Parameters
    ----------
    cache : Path
        Directory of the downloaded file.

    Returns
    -------
    bytes
        The pinned file.

    Raises
    ------
    EntitiesError
        If its SHA-256 differs from the pinned one.
    """
    path = cache / "entities.json"
    if not path.exists():
        path.parent.mkdir(parents=True, exist_ok=True)
        with urllib.request.urlopen(URL, timeout=60) as response:  # noqa: S310 (fixed https URL)
            path.write_bytes(response.read())
    data = path.read_bytes()
    digest = hashlib.sha256(data).hexdigest()
    if digest != SHA256:
        raise EntitiesError(f"{path}: SHA-256 {digest} differs from the pinned {SHA256}")
    return data


def utf8(codepoints: list[int]) -> bytes:
    """UTF-8 of scalar values, encoded by hand (the test data uses Python's encoder on the other field).

    Parameters
    ----------
    codepoints : list[int]
        Scalar values.

    Returns
    -------
    bytes
        Their UTF-8 encoding.
    """
    out = bytearray()
    for code in codepoints:
        if code < 0x80:
            out.append(code)
        elif code < 0x800:
            out += bytes((0xC0 | code >> 6, 0x80 | code & 0x3F))
        elif code < 0x10000:
            out += bytes((0xE0 | code >> 12, 0x80 | code >> 6 & 0x3F, 0x80 | code & 0x3F))
        else:
            out += bytes((0xF0 | code >> 18, 0x80 | code >> 12 & 0x3F, 0x80 | code >> 6 & 0x3F, 0x80 | code & 0x3F))
    return bytes(out)


def table(data: bytes) -> list[tuple[str, bytes, bytes]]:
    """Read the JSON into (name without ``&``, UTF-8 of ``codepoints``, UTF-8 of ``characters``), in byte order.

    Parameters
    ----------
    data : bytes
        The pinned file.

    Returns
    -------
    list[tuple[str, bytes, bytes]]
        One row per name.

    Raises
    ------
    EntitiesError
        If a name or a value breaks an assumption the module relies on.
    """
    entities = json.loads(data.decode("utf-8"))
    rows = []
    for name, value in entities.items():
        if not NAME_PATTERN.fullmatch(name):
            raise EntitiesError(f"{name!r}: not & + ASCII letter + letters or digits + optional ;")
        by_codepoints = utf8(value["codepoints"])
        by_characters = value["characters"].encode("utf-8")
        if by_codepoints != by_characters:
            raise EntitiesError(f"{name}: codepoints and characters disagree")
        if not name.endswith(";") and entities.get(name + ";") != value:
            raise EntitiesError(f"{name}: no {name};, or a different value")
        rows.append((name[1:], by_codepoints, by_characters))
    if len(rows) != EXPECTED_COUNT:
        raise EntitiesError(f"{len(rows)} names, the pinned file has {EXPECTED_COUNT}")
    return sorted(rows, key=lambda row: row[0].encode("ascii"))


def statement(head: str, items: list[str], tail: str, join: str) -> list[str]:
    """Lines of a continued statement: ``head`` then the items, ``join`` between them, wrapped at ``WIDTH``.

    Parameters
    ----------
    head : str
        First line, ending with the opening of the list.
    items : list[str]
        Items, each one kept on one line.
    tail : str
        Text closing the list.
    join : str
        Separator between two items.

    Returns
    -------
    list[str]
        The lines, every one but the last ending with `` &``.
    """
    lines, line = [head + " &"], "   "
    for i, item in enumerate(items):
        text = item + (join if i < len(items) - 1 else tail)
        if len(line) + len(text) + 2 > WIDTH:
            lines.append(line.rstrip() + " &")
            line = "   "
        line += text
    lines.append(line.rstrip())
    return lines


def array(declaration: str, name: str, items: list[str], lower: int, purpose: str) -> list[str]:
    """Declaration of a parameter array, split into chunk arrays of at most ``CHUNK_LINES`` continuation lines.

    Parameters
    ----------
    declaration : str
        Type of the array (``integer(I4P)``, ``character(N)``).
    name : str
        Name of the array.
    items : list[str]
        Its elements, as Fortran literals.
    lower : int
        Its lower bound.
    purpose : str
        Doc comment.

    Returns
    -------
    list[str]
        The lines of the chunks and of the array.
    """
    per_line = (WIDTH - 5) // (max(len(item) for item in items) + 2)  # the item, a comma and a blank
    size = CHUNK_LINES * per_line
    lines, names = [], []
    for k, start in enumerate(range(0, len(items), size), start=1):
        chunk = items[start : start + size]
        part = f"{name}_{k:02d}"
        names.append(part)
        head = f"{declaration}, parameter :: {part}(1:{len(chunk)}) = [{declaration} ::"
        lines += statement(head, chunk, "]", ", ")
    upper = lower + len(items) - 1
    lines.append(f"{declaration}, parameter :: {name}({lower}:{upper}) = & !< {purpose}")
    lines.append(f"   [{', '.join(names)}]")
    return lines


def module_text(rows: list[tuple[str, bytes, bytes]]) -> str:
    """Return the Fortran module.

    Parameters
    ----------
    rows : list[tuple[str, bytes, bytes]]
        The table, in byte order of the names.

    Returns
    -------
    str
        The module source.
    """
    value_end, value_bytes = [0], []
    for _, value, _ in rows:
        value_end.append(value_end[-1] + len(value))
        value_bytes += list(value)
    semicolon = sum(name.endswith(";") for name, _, _ in rows)
    longest = max(len(name) for name, _, _ in rows)
    longest_value = max(len(value) for _, value, _ in rows)
    parts = [
        "!< BARUCH, the HTML5 named character references: generated by scripts/gen_entities.py, do not edit.",
        "module baruch_entities",
        "!< BARUCH, the HTML5 named character references: generated by scripts/gen_entities.py, do not edit.",
        "!<",
        f"!< The {len(rows)} names of WHATWG's `entities.json` ({URL}, SHA-256",
        f"!< {SHA256}): {semicolon} ending in `;` and {len(rows) - semicolon} legacy",
        "!< forms without it, each name as the JSON lists it but without its leading `&`, mapped to the UTF-8 bytes of",
        "!< its code points. CommonMark recognises only the names ending in `;`; every legacy form has a `;` twin with the",
        "!< same value. `entity_lookup` finds a name (binary search over the names in byte order) and `entity_utf8` gives",
        "!< its bytes. The tables are parameters: no module variable, every procedure pure.",
        "use penf, only : I4P",
        "implicit none",
        "private",
        "public :: ENTITY_SOURCE, ENTITY_SHA256, ENTITY_COUNT, ENTITY_NAME_MAX, ENTITY_UTF8_MAX",
        "public :: entity_lookup, entity_utf8",
        "",
        f"character(*), parameter :: ENTITY_SOURCE   = '{URL}' !< Source of the table.",
        "character(*), parameter :: ENTITY_SHA256   = & !< Its SHA-256.",
        f"   '{SHA256}'",
        f"integer(I4P), parameter :: ENTITY_COUNT    = {len(rows)}_I4P !< Number of names.",
        f"integer(I4P), parameter :: ENTITY_NAME_MAX = {longest}_I4P !< Longest name, `;` included, `&` not.",
        f"integer(I4P), parameter :: ENTITY_UTF8_MAX = {longest_value}_I4P !< Longest value, in UTF-8 bytes.",
        "character(*), parameter :: NAME_CHARACTERS = & !< The characters of every name.",
        "   'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789;'",
        "",
        "! the names in byte order, blank-padded (an array, not one string: ifx 2026.2 refuses a character constant",
        "! of more than 7198 characters)",
        *array(f"character({longest})", "NAMES", [f"'{name}'" for name, _, _ in rows], 1, "Names, in byte order."),
        "! the values, concatenated; the bytes of name i are VALUE_BYTES(VALUE_END(i-1)+1:VALUE_END(i))",
        *array("integer(I4P)", "VALUE_BYTES", [str(v) for v in value_bytes], 1, "UTF-8 of the values, concatenated."),
        *array("integer(I4P)", "VALUE_END", [str(v) for v in value_end], 0, "Last byte of each value in VALUE_BYTES."),
        "",
        "contains",
        "   pure function entity_lookup(name) result(entity)",
        "   !< Return the index of a name (`amp;`, `ngE;`, `copy`: without `&`, with `;` when it has one), 0 if it is not",
        "   !< one. Names are case-sensitive; any byte sequence is accepted.",
        "   character(*), intent(in) :: name   !< Name to look up.",
        "   integer(I4P)             :: entity !< Its index in 1..ENTITY_COUNT, 0 if not found.",
        "   integer(I4P)             :: low    !< First candidate.",
        "   integer(I4P)             :: high   !< Last candidate.",
        "   integer(I4P)             :: middle !< Candidate tested.",
        "",
        "   entity = 0_I4P",
        "   if (len(name) < 1 .or. len(name) > ENTITY_NAME_MAX) return",
        "   if (verify(name, NAME_CHARACTERS) > 0) return",
        "   ! the name and the table hold only characters above the blank: llt and lgt (ASCII, blank padding) follow the",
        "   ! byte order of the table, and neither true means the same name",
        "   low = 1_I4P",
        "   high = ENTITY_COUNT",
        "   do while (low <= high)",
        "      middle = (low + high) / 2_I4P",
        "      if (llt(name, NAMES(middle))) then",
        "         high = middle - 1_I4P",
        "      elseif (lgt(name, NAMES(middle))) then",
        "         low = middle + 1_I4P",
        "      else",
        "         entity = middle",
        "         return",
        "      endif",
        "   enddo",
        "   endfunction entity_lookup",
        "",
        "   pure function entity_utf8(entity) result(bytes)",
        "   !< Return the UTF-8 bytes of an entity (an index from `entity_lookup`), empty for an index out of range.",
        "   integer(I4P), intent(in)  :: entity !< Index of the name.",
        "   character(:), allocatable :: bytes  !< Its value, UTF-8.",
        "   integer(I4P)              :: k      !< Counter.",
        "",
        "   if (entity < 1_I4P .or. entity > ENTITY_COUNT) then",
        "      bytes = ''",
        "      return",
        "   endif",
        "   allocate(character(VALUE_END(entity) - VALUE_END(entity-1)) :: bytes)",
        "   do k=1_I4P, VALUE_END(entity) - VALUE_END(entity-1)",
        "      bytes(k:k) = char(VALUE_BYTES(VALUE_END(entity-1)+k))",
        "   enddo",
        "   endfunction entity_utf8",
        "endmodule baruch_entities",
    ]
    return "\n".join(parts) + "\n"


def data_text(rows: list[tuple[str, bytes, bytes]]) -> str:
    """Return the test data: ``&NAME<TAB>HEX`` per line, the bytes from the ``characters`` field.

    Parameters
    ----------
    rows : list[tuple[str, bytes, bytes]]
        The table.

    Returns
    -------
    str
        The data file.
    """
    return "".join(f"&{name}\t{characters.hex()}\n" for name, _, characters in rows)


def main() -> int:
    """Command line entry point.

    Returns
    -------
    int
        Exit status: 0 on success, 1 on a failed check or a bad source.
    """
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--cache", type=Path, help="download cache (default build/entities)")
    parser.add_argument("--check", action="store_true", help="fail if the committed module or data differ")
    args = parser.parse_args()
    cache = args.cache or ROOT / "build" / "entities"
    try:
        rows = table(fetch(cache))
    except (EntitiesError, OSError, ValueError, KeyError) as error:
        print(f"gen_entities: {error}", file=sys.stderr)
        return 1
    outputs = {MODULE: module_text(rows), DATA: data_text(rows)}
    if not args.check:
        for path, text in outputs.items():
            path.write_text(text, encoding="ascii", newline="\n")
            print(f"gen_entities: wrote {path.relative_to(ROOT)}")
        return 0
    status = 0
    for path, text in outputs.items():
        if path.exists() and path.read_text(encoding="ascii") == text:
            continue
        with tempfile.NamedTemporaryFile("w", suffix=path.suffix, delete=False, encoding="ascii") as tmp:
            tmp.write(text)
        print(f"gen_entities: {path.relative_to(ROOT)} differs from the generation (see {tmp.name})", file=sys.stderr)
        status = 1
    if status == 0:
        print(f"gen_entities: {MODULE.name} and {DATA.name} match entities.json ({len(rows)} names, SHA-256 {SHA256})")
    return status


if __name__ == "__main__":
    sys.exit(main())
