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

## List every link of a page

Every link and image of a page, whatever its form, with the line it is on, its destination, its title, its text and,
for a reference, the label as written (`page.md`). The [walk](/reference/walk) visits the tree in document order; a
link or an image is a span, so its text is a second walk over its subtree (`top=`).

<<< @/examples/snippets/list_links-walk.f90

<<< @/examples/snippets/list_links-text.f90

<<< @/examples/output/list_links.ansi{ansi}

- The four forms are one kind: `[the paper][paper]` is `full`, `[latest release][]` `collapsed`, `[FAQ]` `shortcut`,
  the others `inline` or `autolink` (`SLOT_LINK_FORM`). The destination and the title come from the definition, which
  the label matches case-insensitively and with its white space collapsed (`[faq]` defines `[FAQ]`).
- A reference whose label has no definition is not a link: `[link with an undefined label][nowhere]` and `[this]`
  stay text, and are not listed.
- An image inside a link is listed after it: the walk enters the link, then the image in its text. The text of an
  image is its alt text.

Reference: [Links and references](/reference/links), [Walk](/reference/walk). The whole program:

<<< @/examples/snippets/list_links.f90

## Change a heading with a small diff

A page written by hand, in its author's style (`guide.md`: setext headings, `+` bullets, `1)` with two spaces, a `~~~`
fence, `___`, a collapsed reference, two blank lines before the definition): change one heading's text and write the
page back. In the recorded style ([Writer](/reference/writer#the-recorded-style)) every line that was not edited comes
back as it was, so the diff is the one line.

<<< @/examples/snippets/edit_heading-edit.f90

<<< @/examples/output/edit_heading.ansi{ansi}

- The heading's text is the literal of its text node; a heading holding emphasis or code has more inline nodes (walk
  them, [Walk](/reference/walk)).
- The setext underline stays as long as it was: any length underlines a heading. A text needing escapes gets them, and
  only them ([the escaping rules](/reference/writer#escaping)).
- `canonical=.true.` writes the whole page in the canonical style instead: a page normalised once, then edited in small
  diffs.

::: info A table cell
The plan named this recipe "Change a table cell with a small diff"; tables are GFM, written in plan step 5.1. The
recipe will gain the table then.
:::

Reference: [Writer](/reference/writer). The whole program:

<<< @/examples/snippets/edit_heading.f90

## Planned

::: info Planned
The other recipes arrive with the step of the plan ([szaghi/BARUCH#1](https://github.com/szaghi/BARUCH/issues/1)) that
makes them possible.
:::

| Recipe | Plan step |
|---|---|
| Change a table cell with a small diff (tables: GFM) | 5.1 |
| Report a residual table while the run goes | 4.1 |
| One report per MPI rank (code shown, not run: no MPI in the examples) | 4.2 |
| Read a table into arrays | 5.1 |
| Read the title from the front matter | 6.3 |
