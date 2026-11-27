#!/usr/bin/env python3
"""Compare a renderer's HTML with the expected HTML of a conformance suite (plan szaghi/BARUCH#1, step 1.4).

Usage: python3 -I scripts/spec/compare.py [--suite NAME] --actual DIR [--blocks-only] [--failures N]
       python3 -I scripts/spec/compare.py [--suite NAME] --self-test [--blocks-only]

The suites and their fixtures are those of ``scripts/spec/fixtures.py`` (default suite ``commonmark``). ``DIR`` holds
one ``NNNN.html`` per example, the renderer's output for ``examples/NNNN.md`` (``scripts/spec.sh`` writes it). An
example passes as the spec's own runner (``test/spec_tests.py``) decides, in two stages:

1. **exact**: the output bytes equal the expected bytes;
2. **normalised**: otherwise, the output (UTF-8; an output that is not valid UTF-8 fails) and the expected HTML are
   both passed through the spec's normaliser, ``normalize_html`` of ``scripts/spec/normalize.py`` (vendored unchanged),
   and the results compared. The normaliser collapses runs of white space to one space outside ``<pre>``, removes
   white space around block-level tags, writes self-closing tags as open tags (``<br />`` is ``<br>``), sorts the
   attributes, and writes character references as the characters they stand for, except ``< > & "`` which stay
   references.

A missing output file fails. The report prints, per section and overall, the examples passed (exact or normalised),
those passed exactly, and the total; its last line is ``NAME: P/T passed (E exact)``.

``--blocks-only`` restricts every count to the examples whose Markdown has no inline syntax, those a block parser
with an HTML renderer writing inline content as escaped text must already pass (plan steps 1.5 and 1.6). An example
belongs to the subset when all of these hold:

- B1. its section is one of the spec's block sections: Tabs, Thematic breaks, ATX headings, Setext headings, Indented
  code blocks, Fenced code blocks, HTML blocks, Paragraphs, Blank lines, Block quotes, List items, Lists;
- B2. its Markdown contains no backslash and no ``&``: backslash escapes and character references change the text
  without leaving a tag in the HTML, so the expected HTML cannot show whether they were used;
- B3. in the expected HTML, the elements that hold inline content hold nothing but text: ``<p>`` and ``<h1>`` to
  ``<h6>`` contain no element, comment or declaration, and ``<li>`` (whose text is the inline content of a tight
  list) none but the blocks ``<p> <h1>-<h6> <pre> <blockquote> <ul> <ol> <hr>``. Every other inline construct leaves a
  tag there (``<em> <strong> <a> <img> <code> <br />``, raw inline HTML);
- B4. that text (references decoded) contains none of the characters that open an inline construct and can stay text
  when they open none: ``*``, ``_``, backtick, ``[``, ``]``, ``<``.

Outside those elements (code blocks, HTML blocks) any character is allowed: the block parser alone decides that
content. The subset is printed with its size.

``--self-test`` compares the expected HTML with itself through the normaliser (stage 2 for every example, stage 1
skipped), checks that the vendored normaliser is byte-identical to upstream below its header and passes upstream's
docstring examples: every example must pass (exit 1 otherwise).
"""

from __future__ import annotations

import argparse
import doctest
import hashlib
import importlib.util
import sys
import warnings
from collections import Counter
from collections.abc import Callable
from dataclasses import dataclass
from difflib import unified_diff
from html.parser import HTMLParser
from pathlib import Path
from types import ModuleType

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
NORMALIZE_SHA256 = "6fb1dac11284c1ec9e527aaa9a03b02e53d6f64be0034315f2e6a32de7f3a034"  # upstream test/normalize.py
NORMALIZE_HEADER_END = "# " + "-" * 118 + "\n"  # the last line of the vendoring header

BLOCK_SECTIONS = frozenset(
    {
        "Tabs",
        "Thematic breaks",
        "ATX headings",
        "Setext headings",
        "Indented code blocks",
        "Fenced code blocks",
        "HTML blocks",
        "Paragraphs",
        "Blank lines",
        "Block quotes",
        "List items",
        "Lists",
    }
)
TEXT_ONLY = frozenset({"p", "h1", "h2", "h3", "h4", "h5", "h6"})
LI_BLOCKS = frozenset({"p", "h1", "h2", "h3", "h4", "h5", "h6", "pre", "blockquote", "ul", "ol", "hr"})
VOID = frozenset({"area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"})
INLINE_OPENERS = frozenset("*_`[]<")
MARKDOWN_FORBIDDEN = frozenset("\\&")


def load_module(name: str) -> ModuleType:
    """Load a module of this directory by path (``python3 -I`` keeps the script's directory out of ``sys.path``)."""
    spec = importlib.util.spec_from_file_location(f"baruch_spec_{name}", HERE / f"{name}.py")
    if spec is None or spec.loader is None:
        sys.exit(f"compare: cannot load {HERE / name}.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module  # dataclasses look their module up there
    with warnings.catch_warnings():
        # upstream normalize.py writes '\s' and '\<' in plain strings: a SyntaxWarning since Python 3.12
        warnings.simplefilter("ignore", SyntaxWarning)
        spec.loader.exec_module(module)
    return module


fixtures = load_module("fixtures")
normalize = load_module("normalize")
normalize_html: Callable[[str], str] = normalize.normalize_html


@dataclass(frozen=True)
class Case:
    """One example of a suite, read from its fixtures."""

    number: int
    section: str
    markdown: bytes
    html: bytes

    @property
    def stem(self) -> str:
        """Return the file name of the example without extension."""
        return f"{self.number:04d}"


def read_cases(suite: fixtures.Suite) -> list[Case]:
    """Read the examples of a suite from its fixtures (``index.tsv`` and ``examples/``)."""
    index = suite.directory / "index.tsv"
    if not index.is_file():
        sys.exit(f"compare: {suite.name}: no {index.relative_to(ROOT)} (run scripts/spec/fixtures.py)")
    cases = []
    for line in index.read_text(encoding="utf-8").splitlines()[1:]:
        number, section = line.split("\t")[:2]
        stem = f"{int(number):04d}"
        cases.append(
            Case(
                number=int(number),
                section=section,
                markdown=(suite.directory / "examples" / f"{stem}.md").read_bytes(),
                html=(suite.directory / "examples" / f"{stem}.html").read_bytes(),
            )
        )
    return cases


class InlineContent(HTMLParser):
    """Collect the text of the elements holding inline content, and whether anything else appears in them (B3)."""

    def __init__(self) -> None:
        """Start with no element open and no text."""
        super().__init__(convert_charrefs=True)
        self.stack: list[str] = []
        self.text: list[str] = []
        self.foreign = False

    def container(self) -> str:
        """Return the innermost open element if it holds inline content, else ``''``."""
        if self.stack and (self.stack[-1] in TEXT_ONLY or self.stack[-1] == "li"):
            return self.stack[-1]
        return ""

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:  # noqa: ARG002
        """Record an element inside an inline container, unless it is a block of a list item."""
        parent = self.container()
        if parent in TEXT_ONLY or (parent == "li" and tag not in LI_BLOCKS):
            self.foreign = True
        if tag not in VOID:
            self.stack.append(tag)

    def handle_startendtag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        """Check a self-closing tag as a start tag that opens nothing."""
        parent = self.container()
        if parent in TEXT_ONLY or (parent == "li" and tag not in LI_BLOCKS):
            self.foreign = True
        del attrs

    def handle_endtag(self, tag: str) -> None:
        """Close the innermost open element of this name (an unmatched end tag of an HTML block is ignored)."""
        if tag in self.stack:
            while self.stack.pop() != tag:
                pass

    def handle_data(self, data: str) -> None:
        """Keep the text of an inline container."""
        if self.container():
            self.text.append(data)

    def foreign_markup(self) -> None:
        """Record a comment, declaration or processing instruction inside an inline container."""
        if self.container():
            self.foreign = True

    def handle_comment(self, data: str) -> None:  # noqa: D102
        del data
        self.foreign_markup()

    def handle_decl(self, decl: str) -> None:  # noqa: D102
        del decl
        self.foreign_markup()

    def handle_pi(self, data: str) -> None:  # noqa: D102
        del data
        self.foreign_markup()

    def unknown_decl(self, data: str) -> None:  # noqa: D102
        del data
        self.foreign_markup()


def blocks_only(case: Case) -> bool:
    """Return True if an example has no inline syntax (rules B1-B4 of this module's documentation)."""
    if case.section not in BLOCK_SECTIONS:
        return False
    if MARKDOWN_FORBIDDEN & set(case.markdown.decode("utf-8")):
        return False
    parser = InlineContent()
    parser.feed(case.html.decode("utf-8"))
    parser.close()
    if parser.foreign:
        return False
    return not (INLINE_OPENERS & set("".join(parser.text)))


def outcome(expected: bytes, actual: bytes | None, *, exact: bool = True) -> str:
    """Return ``exact``, ``normalised`` or ``fail`` for one example (``actual`` None: no output file)."""
    if actual is None:
        return "fail"
    if exact and actual == expected:
        return "exact"
    try:
        text = actual.decode("utf-8")
    except UnicodeDecodeError:
        return "fail"
    if normalize_html(text) == normalize_html(expected.decode("utf-8")):
        return "normalised"
    return "fail"


def report(suite: fixtures.Suite, cases: list[Case], results: dict[int, str], subset: str) -> tuple[int, int]:
    """Print the counts per section and overall; return (passed, total)."""
    total: Counter[str] = Counter()
    passed: Counter[str] = Counter()
    exact: Counter[str] = Counter()
    for c in cases:
        total[c.section] += 1
        passed[c.section] += results[c.number] != "fail"
        exact[c.section] += results[c.number] == "exact"
    width = max([len(s) for s in total] + [len("section")])
    print(f"{suite.title}{subset}")
    print(f"  {'section':<{width}}  passed   exact   total")
    for section in total:  # in the order of the spec
        print(f"  {section:<{width}}  {passed[section]:6d}  {exact[section]:6d}  {total[section]:6d}")
    p, e, t = sum(passed.values()), sum(exact.values()), sum(total.values())
    print(f"  {'total':<{width}}  {p:6d}  {e:6d}  {t:6d}")
    print(f"{suite.name}: {p}/{t} passed ({e} exact)")
    return p, t


def show_failures(cases: list[Case], results: dict[int, str], actual_dir: Path, limit: int) -> None:
    """Print the Markdown and the normalised diff of the first failing examples."""
    for c in [c for c in cases if results[c.number] == "fail"][:limit]:
        path = actual_dir / f"{c.stem}.html"
        print(f"\nexample {c.number} ({c.section})\n--- markdown\n{c.markdown.decode('utf-8')}", end="")
        if not path.is_file():
            print(f"--- no output {path}")
            continue
        expected = normalize_html(c.html.decode("utf-8")).splitlines(keepends=True)
        actual = normalize_html(path.read_bytes().decode("utf-8", errors="replace")).splitlines(keepends=True)
        for line in unified_diff(expected, actual, "expected (normalised)", "actual (normalised)"):
            print(line, end="" if line.endswith("\n") else "\n")


def self_test_normaliser() -> int:
    """Check the vendored normaliser (body byte-identical to upstream, upstream doctests); return the failures."""
    text = (HERE / "normalize.py").read_text(encoding="utf-8")
    _, sep, body = text.partition(NORMALIZE_HEADER_END)
    failures = 0
    if not sep or hashlib.sha256(body.encode("utf-8")).hexdigest() != NORMALIZE_SHA256:
        print("compare: normalize.py differs from upstream 0.31.2 below its header")
        failures += 1
    finder, runner = doctest.DocTestFinder(), doctest.DocTestRunner(verbose=False)
    for test in finder.find(normalize_html, "normalize_html", globs={"normalize_html": normalize_html}):
        runner.run(test)
    failures += runner.failures
    print(f"normaliser: upstream body {'ok' if failures == 0 else 'NOT ok'}, {runner.tries} docstring examples run")
    return failures


def main() -> int:
    """Compare and report."""
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--suite", default="commonmark", choices=sorted(fixtures.SUITES), help="default commonmark")
    parser.add_argument("--actual", type=Path, help="directory of the renderer's NNNN.html files")
    parser.add_argument("--blocks-only", action="store_true", help="only the examples with no inline syntax")
    parser.add_argument("--self-test", action="store_true", help="the expected HTML against itself, normalised")
    parser.add_argument("--failures", type=int, default=0, metavar="N", help="show the first N failures with a diff")
    args = parser.parse_args()
    if args.self_test == (args.actual is not None):
        parser.error("give exactly one of --actual DIR and --self-test")
    suite = fixtures.SUITES[args.suite]
    cases = read_cases(suite)
    subset = ""
    if args.blocks_only:
        everything = len(cases)
        cases = [c for c in cases if blocks_only(c)]
        subset = f", blocks-only subset: {len(cases)} of {everything} examples"
    if args.self_test:
        failures = self_test_normaliser()
        results = {c.number: outcome(c.html, c.html, exact=False) for c in cases}
        passed, total = report(suite, cases, results, subset + " (self-test: expected HTML against itself)")
        return 0 if failures == 0 and passed == total else 1
    actual_dir: Path = args.actual
    if not actual_dir.is_dir():
        parser.error(f"no directory {actual_dir}")
    results = {}
    for c in cases:
        path = actual_dir / f"{c.stem}.html"
        results[c.number] = outcome(c.html, path.read_bytes() if path.is_file() else None)
    report(suite, cases, results, subset)
    if args.failures > 0:
        show_failures(cases, results, actual_dir, args.failures)
    return 0


if __name__ == "__main__":
    sys.exit(main())
