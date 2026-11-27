# 3. Read a document

Chapter 1 built a tree by hand; here BARUCH builds it from Markdown. This chapter parses a file, prints its tree with
what each block records, and reads a block's bytes back from the source.

The file, `report.md`:

<<< @/examples/output/t03_read-source.ansi{ansi}

## Parse

<<< @/examples/snippets/t03_read-parse.f90

- `load_file` reads the file into the document as sanitised UTF-8 and indexes its lines ([Input](/reference/input));
  `load` does the same from a string.
- `parse` belongs to the options, the object that says what a parse does: here an empty one, CommonMark alone. It
  resets the tree to its root and builds the blocks from the lines.
- Parsing never fails on the text: any bytes are a document. A nonzero `stat` comes only from an extension (chapter
  2) or from memory.

## Inspect the tree

The tree is printed as in chapter 1, by following its links, with what each kind records:

<<< @/examples/snippets/t03_read-fields.f90

- The typed accessors give the meaning: `heading_level`, `list_type`, `list_start`, `list_tight`, `literal`, `info`,
  `label`, `destination`, `title`.
- The integer slots give the **source style**, how the block was written: `#` or underlined, the closing `#`s, the
  bullet or the ordered delimiter, the fence and its length, the thematic break's character. The writer of phase 3
  reuses them, so a document written back keeps its look.

## Running it

<<< @/examples/output/t03_read.ansi{ansi}

What the tree shows:

- `# Field report #` is a level-1 ATX heading with one closing `#`, which is not part of its text; `Results` over
  `-------` is a level-2 setext heading.
- A paragraph or a heading holds its **inline content**: the line ending between the paragraph's two lines is a
  `softbreak` between two texts ([chapter 5](./05-inlines) shows the other inline nodes). `*rig*` is emphasis: an
  `emph` node holding the text `rig`.
- `1)` items make an ordered list from 1 with the `)` delimiter; the `-` items indented under `temperature` are a list
  inside its item. Both lists are tight: no blank line between their items.
- The code block keeps its `~~~` fence and its info string `fortran`; its literal ends with LF.
- `reference probe.` has no `>` but continues the quoted paragraph: a **lazy continuation line**.
- The link reference definition is a node in place, its label as written, its destination and title without their
  delimiters. A `[probe]` in the text would be a link to it (the label matches whatever its case).
- The HTML comment is an HTML block, its literal verbatim.

## Spans: back to the source

<<< @/examples/snippets/t03_read-span.f90

Every block knows its bytes in the source (`span_first`, `span_last`): here the second item, from its marker to its
last line, the nested list included. A span refers to the document's own buffer, so `source_slice` returns exactly
what was written, prefixes and all.

The whole program:

<<< @/examples/snippets/t03_read.f90

::: tip What you learned
`load_file` (or `load`) then `options%parse` builds the block tree; the typed accessors read what a block means and
the slots how it was written; a paragraph or a heading holds inline nodes; spans lead back to the source.
Reference: [Blocks](/reference/blocks).
:::
