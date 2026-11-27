# Cookbook

Short recipes, one task each, every one a program built and run by `scripts/docs_examples.sh` (unless it says
otherwise).

## Read a file in another encoding

A Markdown file written by an old Windows editor, in windows-1252 (`notes-1252.md`: `Caff`, the byte `E8`, ...):
`encoding=` says so, and the document's source is UTF-8.

<<< @/examples/snippets/read_encoding-declared.f90

Without `encoding=` the bytes are read as UTF-8: the ones that are not UTF-8 each become U+FFFD, and the load still
succeeds (every byte sequence is a document):

<<< @/examples/snippets/read_encoding-undeclared.f90

A file that cannot be read is a status, not a stop:

<<< @/examples/snippets/read_encoding-missing.f90

<<< @/examples/output/read_encoding.ansi{ansi}

- A UTF-16 or UTF-32 file with a byte order mark needs no `encoding=`: the mark says it. A UTF-8 byte order mark is
  removed.
- The line endings are kept as they are (a CRLF file stays CRLF); `line(i)`, `line_first(i)` and `line_last(i)` give
  each line without its ending, as byte positions in the UTF-8 source.
- `load` does the same on text already in memory, a `character` or a StringiFor `string`.

Reference: [Input](/reference/input). The whole program:

<<< @/examples/snippets/read_encoding.f90

## Planned

::: info Planned
The other recipes arrive with the step of the plan ([szaghi/BARUCH#1](https://github.com/szaghi/BARUCH/issues/1)) that
makes them possible.
:::

| Recipe | Plan step |
|---|---|
| List every link of a page | 2.4 |
| Change a table cell with a small diff | 3.2 |
| Report a residual table while the run goes | 4.1 |
| One report per MPI rank (code shown, not run: no MPI in the examples) | 4.2 |
| Read a table into arrays | 5.1 |
| Read the title from the front matter | 6.3 |
