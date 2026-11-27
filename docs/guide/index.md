# About

BARUCH (**B**idirectional **A**ST **R**eader and **U**nparser for **C**ommonMark, **H**ookable) reads Markdown into a
document tree and writes a tree back to Markdown, in pure Fortran.

- **Reader**: CommonMark 0.31.2. The block structure is parsed first, the inline content of each block second, as the
  specification describes it.
- **Writer**: the tree back to CommonMark. The round trip is semantic: re-reading the output gives the same tree,
  not necessarily the same bytes (a `*` emphasis may come back as `_`, a setext heading as an ATX one).
- **Streaming writer**: Markdown emitted block by block, with no tree, for reports a program writes while it runs.
- **Extensions**: MyST and VitePress syntax as opt-in hooks. With every extension off, BARUCH is a CommonMark
  processor.

It depends on [StringiFor](https://github.com/szaghi/StringiFor) (text, UTF-8, Unicode classes) and
[PENF](https://github.com/szaghi/PENF) (kinds).

## Status

The repository is being seeded: the build, the tests and the CI are in place, the parser is not written yet.
