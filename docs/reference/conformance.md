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
bash scripts/spec.sh --roundtrip commonmark      # parse, write back (recorded style), parse, compare the trees
bash scripts/spec.sh --roundtrip --canonical commonmark  # the same, written in the canonical style
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
anyway), 2 on a usage or build error. `--roundtrip` (with `--canonical`) runs the [round trip](#round-trip) instead
of the HTML comparison.

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
loads the Markdown (`load_file`), parses it (`markdown_options%parse`, no extension) and writes the HTML (with
`--roundtrip [--canonical]`, the [round trip](#round-trip) instead). An example
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

## The HTML5 entity table

CommonMark's entity references (`&ouml;`, `&ngE;`) are the named character references of HTML5, whose list WHATWG
publishes as a JSON file. BARUCH carries it as a generated Fortran module, pinned by hash like the spec:

| | |
|---|---|
| Source | <https://html.spec.whatwg.org/entities.json>, downloaded 2026-11-27 |
| SHA-256 | `d741d877ac77c4194c4ad526b5b4a19aef8dfe411ab840a466891cdbb9f362e6` |
| Names | **2,231**: 2,125 ending in `;` and 106 legacy forms without it (`&amp`, `&copy`, ...) |
| Module | `src/lib/baruch_entities.F90`, generated, never edited |
| Test data | `src/tests/data/entities.txt`, generated with it |

```bash
python3 -I scripts/gen_entities.py           # regenerate the module and the test data
python3 -I scripts/gen_entities.py --check   # exit 1 if either differs from what the pinned file gives
```

The JSON itself is not committed: `scripts/gen_entities.py` downloads it once into `build/entities/` (git-ignored)
and refuses a file whose SHA-256 is not the pinned one, as StringiFor's `gen_unicode_tables.py` does with the Unicode
Character Database. `--check` regenerates both outputs in memory and compares them byte for byte. Before writing
anything the generator also checks what the module relies on: every name is `&`, an ASCII letter, letters or digits,
and an optional `;`; the `codepoints` and `characters` fields of every name agree; every legacy form has a `;` twin
with the same value; the count is 2,231.

**What is kept.** Every name the JSON lists, both forms, mapped to the UTF-8 bytes of its code points (1 to 6 bytes;
93 names stand for two code points, `&ngE;` is U+2267 U+0338). CommonMark recognises only the names ending in `;`, and
the inline parser ([Inlines](/reference/inlines)) looks up `name;` only, so the 106 legacy forms never match in CommonMark; they are
kept so that the table is the JSON one to one, and they change no result, since each has a `;` twin with the same
value.

**The module.** `baruch_entities` is internal: the inline parser uses it, `use baruch` does not export it.

| Name | Kind | |
|---|---|---|
| `entity_lookup(name)` | pure function | the index (1 to `ENTITY_COUNT`) of a name given without `&` and with its `;` if it has one (`'amp;'`, `'ngE;'`, `'copy'`), 0 if it is not one; case-sensitive; any byte string is accepted |
| `entity_utf8(entity)` | pure function | the UTF-8 bytes of an index, empty for 0 or an index out of range |
| `ENTITY_COUNT`, `ENTITY_NAME_MAX`, `ENTITY_UTF8_MAX` | constants | 2,231 names, the longest 32 characters (`CounterClockwiseContourIntegral;`), the longest value 6 bytes |
| `ENTITY_SOURCE`, `ENTITY_SHA256` | constants | the URL and the hash above |

The tables are parameters: the names in byte order, blank-padded to 32 characters, and the values concatenated with
the offset of each. `entity_lookup` rejects a string that is empty, longer than 32 characters or has a character no
name has, then binary-searches the names (at most 12 comparisons). The names are an array rather than one string
because ifx 2026.2 refuses a character constant longer than 7,198 characters; every array is split into chunks of at
most 120 continuation lines, within the 255 that Fortran 2018 allows. Compiling the module alone takes 0.1 s with gfortran 16.2,
0.35 s with nvfortran 26.9 and 0.07 s with ifx 2026.2, release or debug flags.

`baruch_test_entities` reads the test data (the name as in the JSON and the UTF-8 of its `characters` field: the
other field than the module's bytes, encoded by Python rather than by the generator), looks every name up and decodes
it: **2,231/2,231** decode to the expected bytes, 0 differences, and every entry of the table is found exactly once.
It also checks that strings that are not names (another case, a missing or doubled `;`, a leading or trailing blank,
a byte above ASCII, 33 characters) are not found.

## Counts

`bash scripts/spec.sh commonmark` with gfortran 16.2, nvfortran 26.9 and ifx 2026.2, step by step. The count never
decreases from one step to the next (a standing gate of the plan).

| Step | CommonMark (652) | Blocks-only (237) | |
|---|---|---|---|
| 1.4 | 1 | 0 | no renderer yet: the empty output matches example 207 only |
| 1.5 | 342 | 237 | the block parser, through the driver's test-only block serialiser; the same counts with every compiler and mode |
| 1.6 | 342 | 237 | the public HTML renderer (`render_html_file`) in place of the serialiser; the same counts with gfortran, nvfortran and ifx, debug modes included |
| 2.3 | 439 | 237 | the inline parser: escapes, character references, code spans, autolinks, raw HTML, line breaks; the same counts with gfortran (`tests-gnu-debug`), nvfortran (`tests-nvf`) and ifx (`tests-intel-debug`) |
| 2.4 | **652** | 237 | emphasis, links, images, reference resolution ([Links and references](/reference/links)): every example passes, all 652 exact (no normalisation needed); the same counts with gfortran (`tests-gnu`, `tests-gnu-debug`), nvfortran (`tests-nvf`) and ifx (`tests-intel-debug`) |

At 1.5 every failing example fails on inline syntax only (emphasis, code spans, links and their resolution,
backslash escapes, character references, raw inline HTML, hard line breaks): the block structure of all 652 examples
was checked against markdown-it-py 3.0.0 (CommonMark mode, inline rules off; a local check, not part of the harness),
and the one difference is a character reference in an info string, decoded with the entity table of plan step 2.2.

### The inline sections at step 2.3

The sections of the constructs step 2.3 writes, and the examples still failing in them: each needs emphasis or a link,
which step 2.4 writes.

| Section | Passed | Total | Failing (example: what it needs) |
|---|---|---|---|
| Backslash escapes | 10 | 13 | 15: emphasis; 22: an inline link; 23: a reference link |
| Entity and numeric character references | 14 | 17 | 32: an inline link; 33: a reference link; 37: emphasis |
| Code spans | 22 | 22 | |
| Autolinks | 19 | 19 | |
| Raw HTML | 20 | 20 | |
| Hard line breaks | 13 | 15 | 638, 639: emphasis around the break |
| Soft line breaks | 2 | 2 | |
| Textual content | 3 | 3 | |

The other inline sections pass what needs no emphasis and no link: Emphasis and strong emphasis 45/132, Links 22/90,
Images 2/22, Link reference definitions 10/27 (their use is a link), Inlines 1/1.

### Step 2.4: every section at 100%

Step 2.4 writes the delimiter algorithm, links, images and the resolution of references: the eight examples left
by 2.3 (15, 22, 23, 32, 33, 37, 638, 639) pass, and so does every other one.

| Section | Passed | Total |
|---|---|---|
| Emphasis and strong emphasis | 132 | 132 |
| Links | 90 | 90 |
| Images | 22 | 22 |
| Link reference definitions | 27 | 27 |
| Backslash escapes | 13 | 13 |
| Entity and numeric character references | 17 | 17 |
| Hard line breaks | 15 | 15 |
| every other section | all | all |
| **total** | **652** | **652** |

The pathological inputs (plan step 2.6) are [below](#pathological-inputs).

## Round trip

The [writer](/reference/writer) is checked on every example of a suite: the driver parses it, writes the tree back as
Markdown (`write_markdown`, the recorded style, or the canonical one with `--canonical`), parses what it wrote and
compares the two trees (`markdown_document%equal`, [Equality](/reference/equality)). The written Markdown goes to
`build/spec/MODE/SUITE/roundtrip/NNNN.md` (`roundtrip-canonical/` with `--canonical`, emptied first); a difference to
`NNNN.md.fail`. `compare.py --roundtrip` lists every failure with its example number and the difference `equal` gives
(the path of the first differing node, what differs), prints the counts per section (the `exact` column counts the
examples written back byte for byte), then:

```text
commonmark: 652/652 round trip (recorded) (515 exact)
commonmark: byte-identical (recorded style, no tab, no trailing space): 500/607 (82.4%)
```

The **byte-identical rate** is measured on the examples with no tab and no trailing space (607 of 652: a tab or a
trailing space is often what an example is about, and no writer keeps both): how much of the source's style the
recorded style gives back. It is a regression gate for the later steps, not a promise; the plan's target was 90%. The
18% left are what the tree does not record, mostly the spec's deliberate oddities:

| Section | Not identical | Why |
|---|---|---|
| List items, Lists, Block quotes | 32 | lazy continuation lines (written with their markers), `>` without its space, the indentation of nested blocks, an item's blank first line |
| Links, Link reference definitions, Images | 28 | escapes and references in destinations and titles, `'` and `(` title quotes, pointy destinations, spaces inside the parentheses, a `[` escaped where it would pair with the link after it |
| Emphasis and strong emphasis | 14 | runs the source let the delimiter algorithm split (`***foo**`, `foo******bar`), written as an emphasis and an escaped text |
| Entity and numeric character references, Backslash escapes | 12 | escapes and references where none is needed (`\$`, `&copy;`): the text holds the characters |
| Fenced code blocks | 10 | an unclosed fence (closed), a fence's indentation, spaces before the info string of a fence inside a container |
| ATX and setext headings, Raw HTML, Code spans, Autolinks | 11 | spaces after the `#`s, a closing sequence escaped another way, an indented continuation line under a paragraph, a `<` escaped where the source broke the HTML differently |

On real documents the rate is far higher: the 120 Markdown pages of the documentation of BARUCH, StringiFor, HERALD and
FLAP (a local check) are all written back byte for byte, and all read back equal.

| Step | Round trip, recorded | Round trip, canonical | Byte-identical (recorded) |
|---|---|---|---|
| 3.2 | **652**/652 | **652**/652 | 498/607 (82.0%) |
| 3.3 | **652**/652 | **652**/652 | 500/607 (82.4%) |

The same counts with gfortran (`tests-gnu-debug`), nvfortran (`tests-nvf`) and ifx (`tests-intel-debug`);
`baruch_test_roundtrip` checks them on every build (`scripts/run_tests.sh`, `fpm test`).

## Pathological inputs

Inputs built to make a parser quadratic: thousands of openers that never close, brackets nested 10^4 deep, block quotes
10^5 deep, backtick strings of every length. cmark keeps a list of them in its test suite; BARUCH runs the same cases,
with the same checks of the output, and gates the time: every case must parse and render in time linear in its size.

```bash
bash scripts/pathological.sh                        # gfortran (tests-gnu): every case at n and 10n, ratios <= 15
bash scripts/pathological.sh --mode tests-nvf       # nvfortran; --mode tests-intel for ifx
bash scripts/pathological.sh --case backticks       # one case; --n N another base size, --max-ratio R another gate
```

`pathological.sh` builds the test alone (`src/tests/baruch_test_pathological.F90`, `--target`) into
`build/pathological/MODE/`, as `spec.sh` builds its driver, and runs it with `--timing`: each case is built at `n` and
`10n`, then loaded, parsed and rendered as HTML three times at each size; the best wall time of each (`system_clock`)
and their ratio go into a table (case, n, input bytes at n, t(n), t(10n), ratio, check). A linear case gives a ratio
near 10; the gate is **15** for every case, and the HTML of both sizes must pass the case's check. A case over the
gate is measured again, up to 3 attempts, and the last attempt is printed with its number (`ok, attempt 2`): another
process loading the machine stretches one measurement, while a super-linear case fails every attempt. Time the release
modes (`tests-gnu`, `tests-nvf`, `tests-intel`): a `-debug` mode measures its checks. The exit status is 0 when every
case passes, 1 otherwise. `scripts/run_tests.sh` runs the same program with no argument: every case once at
`n` = 1000, checked, not timed, so CI has no timing assertion to be flaky.

### The cases

| | |
|---|---|
| Source | cmark's `test/pathological_tests.py`, <https://github.com/commonmark/cmark/blob/64efa3b3b3d35f2ffb604b57a8a9c89047cb420b/test/pathological_tests.py> (the last commit of the file; cmark `master` at `ce2acf59` on 2026-11-27) |
| SHA-256 | `4f1ba5759781f6488c23c9b08d6e2b056b72101057c695e6e525ac2946c48a4f` |
| Licence | BSD-2-Clause (cmark); the generators and checks are rewritten in Fortran, no file is vendored |

The 25 cases of the file (24 checked on the HTML, `nested inlines` on cmark's CommonMark writer) and its commented-out
`many references`, with the same generators, the repetition count `k` in place of cmark's constant; and 9 cases of
BARUCH's own (marked `+`), for the scans that could rescan the subject. Each check is the case's regular expression
written as a test on the HTML: a repeated literal searched for (most), or an anchored pattern matched by hand (`^`, `$`).

| Case | Input (`k` = n unless said) | Check |
|---|---|---|
| `nested strong emph` | `*a **a ` × k, `b`, ` a** a*` × k | `(<em>a <strong>a ){k}b( a</strong> a</em>){k}` |
| `many emph closers with no openers` | `a_ ` × k | `(a_ ){k-1}a_` |
| `many emph openers with no closers` | `_a ` × k | `(_a ){k-1}_a` |
| `many link closers with no openers` | `a]` × k | `(a\]){k}` |
| `many link openers with no closers` | `[a` × k | `(\[a){k}` |
| `mismatched openers and closers` | `*a_ ` × k | `(\*a_ ){k-1}\*a_` |
| `issue #389` | `*a ` × k, `_a*_ ` × k | `(<em>a ){k}(_a</em>_ ?){k}` |
| `openers and closers multiple of 3` | `a**b`, `c* ` × k | `a\*\*b(c\* ){k-1}c\*` |
| `link openers and emph closers` | `[ a_` × k | `(\[ a_){k}` |
| `pattern [ (]( repeated` | `[ (](` × k | the input |
| `pattern ![[]() repeated` | `![[]()` × k | `(!\[<a href=""></a>){k}` |
| `hard link/emph case` | `**x [a*b**c*](d)` and a blank line, × k (cmark: once) | `<p>**x <a href="d">a<em>b**c</em></a></p>`, k times |
| `nested brackets` | `[` × k, `a`, `]` × k | the input |
| `nested block quotes` | `> ` × k, `a` | `(<blockquote>\n){k}` |
| `deeply nested lists` | `* a` indented by 0, 2, ..., 2(k-1); k = √(25 n): 500 at 10^4 | the k nested `<ul>`, exactly |
| `U+0000 in input` | `abc` U+0000 `de` U+0000, × k (cmark: once) | `abc`, U+FFFD, `de`, U+FFFD, k times |
| `backticks` | `e` and 1, 2, ..., k-1 backticks; k = √(625 n): 2500 at 10^4 | `` ^<p>[e`]*</p>\n$ `` |
| `unclosed links A` | `[a](<b` × k | `(\[a]\(&lt;b){k}` |
| `unclosed links B` | `[a](b` × k | `(\[a]\(b){k}` |
| `unclosed <!--` | `</`, `<!--` × k | `&lt;/(&lt;!--){k}` |
| `empty lines in deeply nested lists` | `- ` × k, `x`, LF × k | `^(<\w+>\n?)+x(</\w+>\n)+$` |
| `empty lines in deeply nested lists in blockquote` | `> `, `- ` × k, `x`, LF, `>` LF × k | the same |
| `emph in deep blockquote` | `>` × k, `a*` × k | `^(<\w+>\n)+<p>.*</p>\n(</\w+>\n)+$` |
| `reference collisions` | the k keys `x0`, `x1`, ... whose cmark hash is 0 modulo 16; k-1 definitions `[key]: /url`, each followed by a paragraph `[first key]` | `(<p>\[x..]</p>\n){k-1}` |
| `nested inlines` | `*` × k, `a`, `*` × k (k even) | cmark checks its CommonMark output; here the HTML: k/2 nested `<strong>` |
| `many references` | `[i]: u` for i = 1..k, then `[0] ` × k (commented out in cmark) | `(\[0\] ){k-1}` |
| `+ unclosed <?` | `x`, `<?` × k | the input, escaped |
| `+ unclosed <![CDATA[` | `x`, `<![CDATA[` × k | the input, escaped |
| `+ unclosed <!A` | `x`, `<!A` × k | the input, escaped |
| `+ unclosed attribute values` | `x`, `<a b='` × k | the input, escaped |
| `+ many &#` | `&#` × k | `(&amp;#){k}` |
| `+ pattern [\ repeated` | `[\` × k | `[` × k, `\` |
| `+ pattern [] repeated` | `[]` × k | the input |
| `+ link destinations with open parentheses` | `[a](b(` × k | the input |
| `+ link destinations with open titles` | `[a](b "` × k | the input, escaped |

Two cases grow as the square of their parameter (`deeply nested lists`, `backticks`): their `k` is the one that makes
the input 10 times larger from `n` to `10n`, cmark's own value at `n` = 10^4.

### The timings

`bash scripts/pathological.sh` on 2026-11-27, release modes: gfortran 16.2 (`tests-gnu`), nvfortran 26.9 (`tests-nvf`),
ifx 2026.2 (`tests-intel`), one WSL2 machine shared with other builds (load about 4.5 on 16 threads). Each entry is
t(10n)/t(n); every case passes its check at both sizes with every compiler. The largest ratio is 14.88 with gfortran
(`issue #389`, on its second attempt; 11.6 to 12.3 in earlier runs), 11.92 with nvfortran and 12.36 with ifx. What
lifts a ratio above 10 is the load and the cache, not the algorithm: `link openers and emph closers` costs 0.63, 0.82,
0.86, 0.90 and 0.94 µs per repetition at n = 10^4, 3·10^4, 10^5, 3·10^5 and 10^6 (gfortran), the step from 10^4 to 10^5
being the input leaving the cache, 9% more from 10^5 to 10^6.

| Case | n | Bytes at n | gfortran | nvfortran | ifx |
|---|---:|---:|---:|---:|---:|
| `nested strong emph` | 10,000 | 140,001 | 10.84 | 10.28 | 10.70 |
| `many emph closers with no openers` | 10,000 | 30,000 | 7.45 | 9.74 | 10.20 |
| `many emph openers with no closers` | 10,000 | 30,000 | 12.56 | 10.64 | 10.14 |
| `many link closers with no openers` | 10,000 | 20,000 | 12.32 | 10.59 | 9.98 |
| `many link openers with no closers` | 10,000 | 20,000 | 11.59 | 9.21 | 10.23 |
| `mismatched openers and closers` | 10,000 | 40,000 | 13.03 | 11.08 | 11.14 |
| `issue #389` | 10,000 | 80,000 | 14.88 (attempt 2) | 11.37 | 10.83 |
| `openers and closers multiple of 3` | 10,000 | 30,004 | 9.86 | 10.44 | 9.95 |
| `link openers and emph closers` | 10,000 | 40,000 | 8.39 | 10.76 | 11.78 |
| `pattern [ (]( repeated` | 10,000 | 50,000 | 8.93 | 10.54 | 10.47 |
| `pattern ![[]() repeated` | 10,000 | 60,000 | 11.92 | 10.80 | 11.99 |
| `hard link/emph case` | 10,000 | 180,000 | 14.06 | 11.18 | 11.79 |
| `nested brackets` | 10,000 | 20,001 | 9.54 | 10.15 | 10.18 |
| `nested block quotes` | 10,000 | 20,001 | 9.96 | 10.27 | 10.77 |
| `deeply nested lists` | 10,000 | 251,500 | 10.98 | 8.93 | 9.41 |
| `U+0000 in input` | 10,000 | 70,000 | 12.92 | 11.59 | 12.36 |
| `backticks` | 1,000 | 313,235 | 13.84 | 11.92 | 10.89 |
| `unclosed links A` | 10,000 | 60,000 | 11.08 | 10.26 | 10.16 |
| `unclosed links B` | 10,000 | 50,000 | 12.38 | 10.28 | 10.15 |
| `unclosed <!--` | 10,000 | 40,002 | 6.68 (attempt 2) | 9.90 | 10.34 |
| `empty lines in deeply nested lists` | 10,000 | 30,001 | 12.17 | 10.36 | 10.25 |
| `empty lines in deeply nested lists in blockquote` | 10,000 | 40,004 | 12.91 | 10.31 | 9.93 |
| `emph in deep blockquote` | 10,000 | 30,000 | 14.27 | 11.38 | 10.93 |
| `reference collisions` | 10,000 | 223,036 | 11.16 | 10.97 | 10.60 |
| `nested inlines` | 10,000 | 20,001 | 10.18 | 10.11 | 10.01 |
| `many references` | 10,000 | 138,894 | 12.10 | 11.26 | 11.56 |
| `+ unclosed <?` | 10,000 | 20,001 | 11.26 | 10.68 | 10.37 |
| `+ unclosed <![CDATA[` | 10,000 | 90,001 | 12.24 | 11.13 | 11.47 |
| `+ unclosed <!A` | 10,000 | 30,001 | 10.62 | 9.81 | 10.32 |
| `+ unclosed attribute values` | 10,000 | 60,001 | 10.01 | 10.85 | 10.67 |
| `+ many &#` | 10,000 | 20,000 | 10.51 | 10.44 | 10.59 |
| `+ pattern [\ repeated` | 10,000 | 20,000 | 9.21 | 9.91 | 9.22 |
| `+ pattern [] repeated` | 10,000 | 20,000 | 9.97 | 10.79 | 10.70 |
| `+ link destinations with open parentheses` | 10,000 | 60,000 | 10.32 | 10.46 | 9.97 |
| `+ link destinations with open titles` | 10,000 | 70,000 | 10.29 | 10.08 | 10.29 |

### What was quadratic

The step found two super-linear cases and fixed the parser (652/652 and 237/237 unchanged):

- **`emph in deep blockquote`** (ratio 98 at 10^4 with gfortran): the parser linked each new emphasis and link node
  with `insert_after`/`insert_before`, whose cycle check walks the ancestors of the parent when the node linked has
  children: O(depth) per node, and the paragraph was 10^4 to 10^5 block quotes deep. The node is new (an extension's
  is checked new, detached and childless by the hook contract), so no cycle is possible: the parser now links it
  through internal unchecked twins of the two methods ([The document](/reference/document#editing)).
- **`pattern ![[]() repeated`** (ratio 58): a link deactivates the `[` below it, walking down the bracket stack to the
  first inactive one; the walk skipped the `![` (images may contain links) but visited them, every one of them at
  every link. Each bracket now records the nearest `[` below it, and the walk follows that chain.

And one limit was a conformance defect that no spec example shows: the table of the last backtick string of each
length stopped at 80 (cmark's stops at 1000), so a string of 81 or more backticks could never open a code span. The
table now grows with the longest string met, reset per block only as far as it was used.

`backticks` runs at `n` = 1000 (10n is then cmark's own input, 2500 strings, 3.1 MB): at `n` = 10^4 its input is 3.1
MB and 31 MB, past the last-level cache, and a plain paragraph of `e` of those sizes, with no backtick at all, shows
the same ratio (about 19 with gfortran: loading 13 times slower, rendering 20 times): memory, not the algorithm.


## Fuzzing

Two fuzzers check what the spec examples cannot: inputs nobody wrote by hand. They live in one test,
`src/tests/baruch_test_fuzz.F90`; `scripts/run_tests.sh` and `fpm test` run a fixed campaign, `scripts/fuzz.sh` the long
ones.

```bash
bash scripts/fuzz.sh 100000                    # 10^5 mutations and 10^5 trees, a seed drawn and printed (gfortran)
bash scripts/fuzz.sh 1000000 2887831255        # a given seed: the same inputs again, with any compiler
bash scripts/fuzz.sh 100000 --mode tests-nvf   # another compiler or mode; --no-build, --progress K
```

`fuzz.sh` builds the test alone into `build/fuzz/MODE/`, prints the seed first and the test's summary last (`fuzz: seed
S, mutations 1-N, trees N (R redrawn), digest D, F failure(s)`), and saves each failing input (the first 20), whole
and shrunk, in `build/fuzz/MODE/failures-SEED/`. If the test crashes, the block of iterations after its last progress
line runs again with `--trace`, which writes each input to a file before checking it: the crashing one is left there.
The exit status is 0 when every input passes.

### What is fuzzed

| Fuzzer | Inputs | Checks |
|---|---|---|
| mutations of the reader | an example of the 652 fixtures, mutated 1 to 8 times: bytes inserted, deleted, replaced; spans duplicated, or spliced from another example; Markdown-significant tokens (`*_[]()<>!#-+`, backtick and tilde fences, `\`, `&`, `&#`, `<!--`, `]: `, list markers, setext lines), repeated 2 to 7 times; line endings (LF, CR, CRLF), tabs; ill-formed UTF-8 (a lone continuation byte, an overlong form, a truncated sequence, a surrogate, `F5`, `FF`) and U+0000 | load, parse, `render_html`, a walk to its end and `write_markdown` in both styles never fail; the HTML and both Markdown texts are well-formed UTF-8 (StringiFor `is_utf8`); each Markdown text parses to a tree `equal` to the first |
| random trees of the writer | a document built with the document API: every core kind, blocks nested 4 deep, texts of ASCII punctuation, white space, Unicode letters, punctuation and spaces (U+00A0, U+2003), U+FFFD, `ẞ` (its case folding is `ss`), character references and Markdown tokens; links and images with odd destinations (white space, `<>`, parentheses, backslashes, references) and titles (quotes, parentheses, line feeds), reference forms whose label may or may not be defined, autolinks; code spans and fences with backticks; HTML blocks of the seven types, raw HTML; random recorded style slots (bullets, delimiters, fence characters and lengths, emphasis characters, link forms, break forms) | render, walk and write in both styles never fail, every output is UTF-8, each Markdown text reads back `equal` to the tree |

The round trip on mutated inputs is the strong check: a mutant is a document nobody would write, and the recorded
style has to write it back from its own odd source.

A random tree is drawn among those Markdown can express ([Writer](/reference/writer#trees-that-cannot-be-written)), and
a test function, `writable`, rejects the others (they are drawn again: about 2.7% of them): no empty paragraph or
emphasis, no break at the edges of a paragraph, a heading or an emphasis, no two breaks in a row, no break or line
ending in a heading of level 3 to 6, no raw HTML first in a paragraph or a heading (an HTML block, read back), no two
code spans side by side (their backtick strings would be one), no link in a link, a link first in a paragraph or a
heading with no `]` in its code or raw HTML (a definition, read back), fenced literals ending with a line ending, info
strings with no white space at their ends; a loose list that reads back loose (a blank line after a definition ends
nothing: cmark drops the definition's paragraph, and the blank line with it); in a tight list's items, only blocks that
follow each other with no blank line (no lazy continuation of a paragraph left open, no HTML block of type 6 or 7 to
end, no two block quotes, no definition after a blank line at the item's end). And two limits that are the writer's,
not Markdown's: emphases nest at most two deep, and an emphasis holds at most one emphasis. Some deeper trees have
no Markdown at all (an emphasis in an emphasis in an emphasis of `(`: whatever the characters, the innermost opener
can also close, and closes the outermost; strong holding two strong of punctuation); for the others the writer's
search (below) is bounded. Deeper emphases come from the mutations: a parsed tree is always writable, its source shows
how.

### Seeds and reproducibility

Every draw comes from a xoshiro128** generator held in a variable of the test (no module state, never
`random_number`, whose sequence differs per compiler), its 32-bit words kept in 64-bit integers so that no operation
overflows. Each iteration is seeded from the seed, the fuzzer and the iteration number (MurmurHash3's finaliser), so an
iteration can be replayed alone (`--start I --mutations 1 --trees 0`, `--trace FILE` to see it), and each draw is a
statement of its own (the order of a statement's function references is the processor's). The summary's digest, an
FNV-1a hash of every input made, is the same with every compiler: `5CC4EC65` for the CI campaign with gfortran 16.2,
nvfortran 26.9 and ifx 2026.2.

`baruch_test_fuzz --dump FILE` prints the tree of a Markdown file, `--check FILE` checks one file as a mutant is
checked. A failing input is shrunk before it is printed: spans of its bytes removed, halving their length, while the
same check fails (a tree: nodes unlinked and texts shortened while it stays writable and fails). The output gives the
shrunk input with its bytes escaped, the difference `equal` found, and the Markdown written in both styles.

### The CI campaign

With no argument (as `run_tests.sh` and `fpm test` run it) the test checks the regression inputs (below), then 10^4
mutations and 10^4 trees with the seed 20261127, in a few seconds with every compiler and mode, so no mode is scaled
down:

| Mode | Time |
|---|---|
| `tests-gnu` (gfortran 16.2, release) | 2.1 s |
| `tests-gnu-debug` (all checks) | 3.1 s |
| `tests-nvf` (nvfortran 26.9) | 3.1 s |
| `tests-intel-debug` (ifx 2026.2, all checks but bounds) | 4.2 s |

### Campaigns run

| Seed | Mutations and trees, each | Mode | Failures | Time |
|---|---:|---|---|---:|
| 1183845045 | 10^5 | `tests-gnu` | 0 | 18 s |
| 3049644105 | 10^5 | `tests-gnu` | 0 | 22 s |
| 2887831255 | 10^6 | `tests-gnu` | 1: emphases four deep, from a run of `*` (below) | 206 s |
| 2673062487 | 10^5 | `tests-nvf` | 1: the same class | 42 s |
| 777 | 10^5 | `tests-gnu-debug` | 1: a definition then a setext heading starting with raw HTML (fixed since) | 35 s |

Earlier campaigns found the bugs of the next section; each was fixed and its shrunk input added to the regression
inputs, which the CI campaign checks first: 63 inputs, 47 sources and 16 trees built by hand. With the writer of step
3.2, 58 of them fail, and so do 5322 of the 20000 inputs of the CI campaign.

### Bugs found and fixed

All in the writer; the parser had none (every mutant parsed, rendered and walked; a mutant's round trip failed only
when the writer wrote it back wrong).

| Input class | Cause | Fix |
|---|---|---|
| a letter next to an emphasis whose content starts (ends) with punctuation (`l*<!X y>*`, `**[]()**F`), an emphasis of `_` inside a word (`1_`t`_`) | the delimiter does not flank there: the parser reads text | the letter is written as a numeric character reference (`&#108;*<!X y>*`): it reads as the letter, the delimiter run sees `;` or `&`, punctuation |
| an emphasis first or last in one of the same kind and character (`**6*D*`), strong around an emphasis, an emphasis whose opener can also close inside another of the same character (`_*_(x_*_`), a text's `*` before it | the touching delimiters make one run; the rule of 3 (CommonMark 6.2, rule 9) or a both-flanking run pairs them otherwise | the delimiter characters are chosen by the flanking rules of the written neighbours; then the delimiters written are read back with the parser's algorithm (`emphasis_reads_back`), and when they would pair otherwise the writer tries other choices ([Writer](/reference/writer#emphasis)) |
| a parsed run like `**;*"* b*)**`, a text's `*` touching a closer | escaping the text's `*` breaks the run that the source shows reads right | one of the choices leaves such a text run in the delimiter's run |
| a backslash before a character written as a reference (`\&#108;`, `\&#160;`) | `\&` is an escaped `&`: the reference is lost | the backslash is escaped |
| a `*` run before white space written as a reference (`*&#32;\`) | the run, seen with `&` after it, can open | its flanking is decided with `&` |
| a backtick in an image's description and in its destination (`![`](<`b>)`) | the destination's backticks counted as already written: the text's backtick not escaped, a code span read back | counted after the link's text |
| a text's backtick strings, the later escaped (`` `rN\`\`\` ``) | escaped strings are strings of one backtick, and close a single one before them | the escapes decided from the text's end, on the strings as written |
| an HTML block of type 1 to 5 left open (`<!--`, `<?`, `<!A`) at the end of the document or of an item | a blank line written after it is read as its own | no blank line after it; an item is ended by indentation |
| a raw HTML tag whose quoted value runs past the text (`<a f=">` then `">`) | the `<` not escaped: a tag, read back | escaped when the text could start a tag that what follows ends |
| a link's tail across nodes (`[](l "` then `<a>`), a destination running past the text, a `<` or a backslash the writer escapes (`[](<<!- >)`, `\[](\\)`) | the tail judged on the text as it is, not as written | every reading tried: as written, with the text's escapes, ended later |
| a `]` closing an `[` written before it | the label checked included the `[` (off by one) | the bytes between the brackets |
| a `]` after a link, an `![` before it (`![[]()\]()`), an inactive `[` | a link deactivates `[` but not `![`; an inactive `[` stays on the bracket stack, the next `]` pops it | the writer keeps the stack as the parser does |
| a paragraph or a setext heading whose written first line reads as a link reference definition (`[*\]:*]: t`, `[|]: \<!--`) | an escape, a reference or a later node completes the definition | the written line is checked (`parse_definition`), and its `[` escaped |
| an info string starting with the fence character (`~~~` after `~~~~~`), holding a line ending or white space at its ends (from `&#10;`, `&#32;`) | the fence lengthened, the info string cut or stripped | a space after the fence; line endings and edge white space as references |
| a fence-like line indented by a tab in a code block in an item | the tab, under 4 columns there, makes it a closing fence | tabs counted as indentation when the fence is chosen |
| a destination holding a line ending (`[](&#10;)`) | written raw: no destination | as a reference |
| a title whose lines start with white space | a paragraph's continuation lines lose it | as references |
| a level 1 or 2 heading with a title or raw HTML spanning lines; a setext heading after a paragraph in a tight list; an indented code block there | an ATX heading is one line; a blank line would make the list loose | setext; ATX; fenced |
| nested empty bullet items (`- - -`), a paragraph behind bullets (`* **`), an indented code block first in an item (`- -     -`) | the line is a thematic break | another bullet, an escape, fenced |
| a definition at the end of a quote or an item, a paragraph after it; a paragraph or setext heading starting with raw HTML after a definition (`[F]: x` then `<l>`) | a definition is read from a paragraph: the next line continues it lazily; after a blank line the raw HTML opens an HTML block | a blank line between them, except before raw HTML: on the next line, indented by 4 when it could open an HTML block |
| a block quote ending with a paragraph, a paragraph after it, in a tight item | a blank line makes the list loose | an empty `>` line closes the quote's paragraph; after a nested open paragraph only a lazy line needs the blank line |
| an item's marker followed by a tab (`-\tb`), a fenced code block first in an item (`-  ```), an indented HTML block after the list (`- o` then `  <div`) or first in an item | the content's column written differently: the next lines change items | the padding read in columns (tabs to multiples of 4) and kept before a fenced block; widened before an indented HTML block after the list; the marker alone on its line before one first in the item; a block after a list never indented into its last item |
| a `<` at the end of a link's text or of an emphasis that ends the block | the text was taken for the block's last: the link's `](...)` or the closer after it can end a tag | not the last |

### What is left

Emphases nested four deep or more, parsed from long runs of `*` or `_` (a mutation repeating a token): about one
mutant in 10^6 has a tree whose writing the writer's bounded search of delimiter characters does not find; the
recorded style usually finds it (the source's characters are one of the choices), the canonical style may not. Such
an input fails the round trip and is reported by the fuzzer; no random tree reaches it (emphases nest two deep there).
