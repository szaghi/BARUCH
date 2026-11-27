# Conformance

How BARUCH is checked against the specifications it implements: the pinned spec files, the fixtures made from them,
the driver that renders every example, and the comparison that counts the passes. The scripts are in `scripts/spec/`
and `scripts/spec.sh`; the fixtures in `src/tests/spec/`.

## Running a suite

```bash
bash scripts/spec.sh commonmark                  # build the driver (gfortran), render the 652 examples, compare
bash scripts/spec.sh commonmark --blocks-only    # only the examples with no inline syntax
bash scripts/spec.sh commonmark --mode tests-nvf # another compiler: tests-nvf, tests-intel, or a -debug mode
bash scripts/spec.sh commonmark --failures 5     # also the Markdown and the diff of the first 5 failures
bash scripts/spec.sh all                         # every suite
```

`spec.sh` builds the driver alone (`fobis build --mode MODE --target src/tests/spec/baruch_spec_driver.F90`) into
`build/spec/MODE/`, so `exe/` and the objects of the other compilers stay untouched; `--no-build` runs the driver
already there. For each suite it lists the examples, runs the driver once over the list, which writes
`build/spec/MODE/SUITE/html/NNNN.html` (the directory is emptied first, so no stale output can pass), and runs the
comparison, which prints the passes per section of the spec and overall:

```text
CommonMark 0.31.2
  section                                  passed   exact   total
  Tabs                                          0       0      11
  ...
  total                                         1       1     652
commonmark: 1/652 passed (1 exact)
```

The last line of each suite has that form. The exit status is 0 when every suite ran, whatever the count (the count is
the result, compared from step to step), 1 when the driver reported an error on an example (the counts are printed
anyway), 2 on a usage or build error. `--roundtrip` and `--canonical` are reserved for the round trip (plan step 3.2)
and refused until then.

### Suites

A suite is a name bound, in one registry (`SUITES` in `scripts/spec/fixtures.py`), to its pinned source file, its
fixtures directory and the extensions the driver enables for it. `python3 -I scripts/spec/fixtures.py --list` prints
the registry, and `spec.sh` reads it: a new suite is an entry there and a fixtures directory, with no change to
`spec.sh` or to the comparison.

| Suite | Specification | Examples | Extensions | Fixtures |
|---|---|---|---|---|
| `commonmark` | CommonMark 0.31.2 | 652 | none: the core alone | `src/tests/spec/commonmark-0.31.2/` |

The GFM and VitePress steps add theirs, named by family and feature: `gfm-table`, `gfm-strikethrough`,
`gfm-tasklist`, `gfm-autolink`, `gfm-footnote`, `vp-container`, `vp-frontmatter`, `vp-anchor`, ...

## The pinned specification

| | |
|---|---|
| Version | CommonMark 0.31.2 |
| File | `src/tests/spec/commonmark-0.31.2/spec.json`, from <https://spec.commonmark.org/0.31.2/spec.json> |
| SHA-256 | `d431b29d97b6f73e69d547109cf5081578fac931e72afe95639ebe766c1b2a20` |
| Licence | the spec text CC-BY-SA 4.0, the test software BSD-2-Clause: `src/tests/spec/commonmark-0.31.2/LICENSE` |

The JSON is the spec's own dump of its examples (`spec_tests.py --dump-tests`): for each, the Markdown, the expected
HTML, the example number, its section and its lines in `spec.txt`. The hash is recorded in `scripts/spec/fixtures.py`,
which refuses a file that does not match it. Like every external data file of BARUCH it is pinned by hash; the
owner's libraries are not (they follow their default branch).

## The fixtures

`scripts/spec/fixtures.py` turns the pinned file into one pair of files per example, committed and never edited:

| File | |
|---|---|
| `examples/NNNN.md` | the Markdown of example `NNNN` (four digits), UTF-8, bytes exactly as in the JSON |
| `examples/NNNN.html` | its expected HTML |
| `index.tsv` | one line per example after a header: `example`, `section`, `start_line`, `end_line` |

```bash
python3 -I scripts/spec/fixtures.py           # regenerate the fixtures of every suite (--suite NAME: one)
python3 -I scripts/spec/fixtures.py --check   # exit 1 if a fixture differs, is missing or is not expected
```

Their bytes are the test: tabs and trailing spaces matter, so `.gitattributes` keeps git's line-ending conversion and
whitespace fixes away from them (`-text -whitespace`).

## The driver

`src/tests/spec/baruch_spec_driver.F90` takes `--list FILE` (one `INPUT<TAB>OUTPUT` per line) and, for each example,
loads the Markdown (`load_file`), parses it (`markdown_options%parse`, no extension) and writes the HTML. An example
that cannot be read, parsed or written is reported on standard error and gets no output, so it fails. A suite with
extensions passes `--extension NAME` for each.

The HTML is the library's: the driver writes each example with `markdown_options%render_html_file` (an HTML fragment,
as the spec's examples are), so the suite checks the public renderer ([HTML](/reference/html)) and nothing else. Until
step 1.6 the driver carried a test-only block serialiser of its own; the counts were unchanged when the renderer
replaced it.

The driver lives in `src/tests/spec/`, so the test modes build it into `exe/` with every compiler; run with no
argument it does nothing and exits 0, and `scripts/run_tests.sh` (which runs every program of `exe/` that way) counts
it as one test that builds and starts. `fpm test` does not build it.

## What is compared, and what is normalised

`scripts/spec/compare.py` decides each example as the spec's own runner does (`test/spec_tests.py` of the spec
repository), in two stages:

1. **Exact.** The output bytes equal the expected bytes. Counted in the `exact` column.
2. **Normalised.** Otherwise both the output and the expected HTML go through the spec's normaliser, and the results
   are compared. An output that is not valid UTF-8 fails; a missing output fails.

An example passing either way counts in `passed`. The normaliser is the spec's `test/normalize.py`, vendored unchanged
as `scripts/spec/normalize.py` (from the tag 0.31.2, commit `9103e341`; its licence and source in the header). It
treats as the same HTML:

- runs of white space, collapsed to one space outside `<pre>`;
- white space around block-level tags (`<p>`, `<li>`, `<pre>`, `<blockquote>`, `<h1>`, ..., `<table>`, ...), removed;
- a self-closing tag and its open form: `<br />` is `<br>`;
- attributes in any order (sorted) and attribute names in any case (lowercased);
- a character reference and the character it stands for (`&forall;` is `∀`), except `<`, `>`, `&` and `"`, which
  stay `&lt;`, `&gt;`, `&amp;`, `&quot;`.

Everything else must match: tags, their nesting, text, and the white space inside `<pre>`.

```bash
python3 -I scripts/spec/compare.py --self-test   # the expected HTML against itself: 652/652
```

The self-test passes every example through the normaliser (stage 2, not short-circuited by stage 1), checks that the
vendored normaliser is byte-identical to upstream below its header, and runs upstream's docstring examples.

## The blocks-only subset

`--blocks-only` (of `spec.sh` and of `compare.py`) counts only the examples whose Markdown has **no inline syntax**:
those a block parser with an HTML renderer that writes inline content as escaped text must already pass (plan steps
1.5 and 1.6). An example belongs to the subset when all four hold:

| Rule | |
|---|---|
| B1 | its section is a block section: Tabs, Thematic breaks, ATX headings, Setext headings, Indented code blocks, Fenced code blocks, HTML blocks, Paragraphs, Blank lines, Block quotes, List items, Lists |
| B2 | its Markdown contains no backslash and no `&`: backslash escapes and character references change the text without leaving a tag, so the expected HTML could not show them |
| B3 | in the expected HTML, the elements holding inline content hold nothing but text: `<p>` and `<h1>`-`<h6>` no element, comment or declaration; `<li>` (the inline content of a tight list) only the blocks `<p>`, `<h1>`-`<h6>`, `<pre>`, `<blockquote>`, `<ul>`, `<ol>`, `<hr>`. Every other inline construct leaves a tag there: `<em>`, `<strong>`, `<a>`, `<img>`, `<code>`, `<br />`, raw inline HTML |
| B4 | that text, references decoded, contains none of `*`, `_`, `` ` ``, `[`, `]`, `<`: the characters that open an inline construct and stay text when they open none |

Code blocks and HTML blocks may contain any character: their content is decided by the block parser alone. The rule
selects **237** of the 652 examples:

| Section | Examples | In the subset | Subset passed (1.5) | All passed (1.5) |
|---|---|---|---|---|
| Tabs | 11 | 11 | 11 | 11 |
| Thematic breaks | 19 | 15 | 15 | 18 |
| ATX headings | 18 | 15 | 15 | 15 |
| Setext headings | 27 | 20 | 20 | 22 |
| Indented code blocks | 12 | 12 | 12 | 12 |
| Fenced code blocks | 29 | 26 | 26 | 26 |
| HTML blocks | 44 | 32 | 32 | 35 |
| Paragraphs | 8 | 7 | 7 | 7 |
| Blank lines | 1 | 1 | 1 | 1 |
| Block quotes | 25 | 25 | 25 | 25 |
| List items | 48 | 47 | 47 | 48 |
| Lists | 26 | 26 | 26 | 26 |
| **total** | **268** | **237** | **237** | **246** |

The rule is conservative: it never selects an example that needs inline parsing, and it may leave out one that would
pass. Checked when the rule was written (a local check, not part of the harness) with markdown-it 14.1.0, CommonMark
mode, every inline rule disabled so that inline content stays escaped text: all 237 pass; of the 31 block-section
examples left out, 9 would pass too (literal `*`, `_` or `` ` `` that open nothing, an `&` inside an HTML block, an
HTML block inside a list item).

## Counts

`bash scripts/spec.sh commonmark` with gfortran 16.2, nvfortran 26.9 and ifx 2026.2, step by step. The count never
decreases from one step to the next (a standing gate of the plan).

| Step | CommonMark (652) | Blocks-only (237) | |
|---|---|---|---|
| 1.4 | 1 | 0 | no renderer yet: the empty output matches example 207 only |
| 1.5 | 342 | 237 | the block parser, through the driver's test-only block serialiser; the same counts with every compiler and mode |
| 1.6 | 342 | 237 | the public HTML renderer (`render_html_file`) in place of the serialiser; the same counts with gfortran, nvfortran and ifx, debug modes included |

At 1.5 every failing example fails on inline syntax only (emphasis, code spans, links and their resolution,
backslash escapes, character references, raw inline HTML, hard line breaks): the block structure of all 652 examples
was checked against markdown-it-py 3.0.0 (CommonMark mode, inline rules off; a local check, not part of the harness),
and the one difference is a character reference in an info string, decoded with the entity table of plan step 2.2.
