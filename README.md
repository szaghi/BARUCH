# BARUCH

**B**idirectional **A**ST **R**eader and **U**nparser for **C**ommonMark, **H**ookable: a Markdown parser and emitter
in pure Fortran.

[![CI](https://github.com/szaghi/BARUCH/actions/workflows/ci.yml/badge.svg)](https://github.com/szaghi/BARUCH/actions/workflows/ci.yml)
[![Matrix](https://github.com/szaghi/BARUCH/actions/workflows/matrix.yml/badge.svg)](https://github.com/szaghi/BARUCH/actions/workflows/matrix.yml)

BARUCH reads Markdown into a document tree and writes a tree back to Markdown:

- **Reader**: CommonMark 0.31.2, block structure first and inlines second, checked against the spec's examples.
- **Writer**: the tree back to CommonMark. The round trip is semantic: re-reading what BARUCH writes gives the same
  tree, though not necessarily the same bytes.
- **Streaming writer**: Markdown emitted block by block, for reports produced while a program runs, with no tree.
- **Hookable**: GitHub's extensions (tables, strikethrough, task lists, autolinks, alerts, footnotes) and VitePress
  syntax (custom containers, math, front matter, anchors, attributes, snippet imports) as opt-in extensions, all
  through one public interface; the CommonMark core does not change when they are off.

Text is handled with [StringiFor](https://github.com/szaghi/StringiFor) and kinds come from
[PENF](https://github.com/szaghi/PENF), both fetched by `fobis fetch`.

Work in progress: the document tree, its input and the extension interface (its block hooks driven by a block-parser
frame) are written, with the conformance harness that runs the 652 CommonMark 0.31.2 examples
(`bash scripts/spec.sh commonmark`); the CommonMark parser, the renderer and the writers are not yet. The plan is [#1](https://github.com/szaghi/BARUCH/issues/1).

```bash
fobis fetch && fobis build --mode tests-gnu && bash scripts/run_tests.sh
```

Baruch, son of Neriah, was Jeremiah's scribe: he wrote the scroll at the prophet's dictation and read it aloud
(Jeremiah 36), the two directions of this library.

Documentation: https://szaghi.github.io/BARUCH. License: your choice of GPL-3.0-or-later, BSD-2-Clause,
BSD-3-Clause or MIT.
