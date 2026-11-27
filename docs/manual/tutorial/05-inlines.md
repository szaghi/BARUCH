# 5. Inline content

Chapter 3 read the blocks of a document; inside each paragraph and heading there is more: code spans, emphasis,
links, images, raw HTML, character references, line breaks. `parse` reads them too, after the blocks, into **inline nodes**: children of
the paragraph or heading, each with its kind, its content and its bytes in the source. This chapter parses a short
document, prints the inline nodes of each block, and renders it.

::: info Plan steps 2.3 and 2.4
Written: backslash escapes, entity and numeric character references, code spans, autolinks, raw inline HTML, hard and
soft line breaks, text (2.3); emphasis, links and images, the references resolved (2.4). [Inlines](/reference/inlines)
and [Links and references](/reference/links) have the details.
:::

## Parse

<<< @/examples/snippets/t05_inlines-parse.f90

Nothing new to call: `parse` runs the block phase, then the inline phase over every paragraph and heading. The
document above is built from a string (`load`), so the example needs no file.

## The inline nodes

<<< @/examples/snippets/t05_inlines-inlines.f90

Each block's children are its inline nodes, in order. Emphasis, links and images have children of their own, so
`print_inlines` (in the whole program below) walks the block's subtree (`walk` with `top=`) and indents a node under
the span it is in; it prints each node's kind, its span and what it holds: the `literal` of a text, a code span or
raw HTML, the `destination` (and `title`) of a link or an image, the character of an emphasis.

## Render

<<< @/examples/snippets/t05_inlines-html.f90

## Running it

<<< @/examples/output/t05_inlines.ansi{ansi}

What the tree shows:

- **Text is merged.** Consecutive text is one node: `` Not code: `dt`, not a tag: 3 < 4 © `` is one text, though it
  holds two backslash escapes, a `<` that opens nothing and a character reference. The text holds what the source
  **means**: the escapes are gone (`` \` `` is a backtick), `&copy;` is `©`, `&mdash;` is `—`. The span still points
  at what was **written**: bytes 151 to 191, the backslashes and `&copy;` included.
- **Code spans** (`code`) hold their content without the backticks; the content is never processed (no escapes, no
  references inside a code span).
- **Raw HTML** (`html_inline`) is kept verbatim, tag by tag: `<b>` and `</b>` are two nodes around the text
  `solver.ini`, and the comment is one node. The renderer writes them as they are.
- **Autolinks** are `link` nodes, the form `LINK_AUTOLINK` recorded in `SLOT_LINK_FORM`: the destination is the URL
  (`&amp;` decoded to `&`) or `mailto:` and the address; the link's child is a text with the same characters. The
  renderer escapes the destination again (`&amp;` in the `href`).
- **Line breaks.** A line ending is a `softbreak`; two or more spaces before it (after `max=9>`) or a backslash (after
  `&copy;`) make it a `linebreak`, written `<br />`. The spaces are not text, and the next line's indentation is
  skipped.
- **Spans cross the block quote.** The quote's three lines start with `> `, which the block parser consumed: the
  inline nodes' spans skip it (`or write to` starts at byte 118, after the `> ` at 116-117), so each span is the exact
  bytes of its node in the source. A line break's span is its spaces or backslash and the line ending.
- A heading holds inline nodes too: `Run `, the code `42`, ` — done`.
- **Emphasis** is a span with children: `*the*` is an `emph`, `**solver notes**` a `strong`, each recording its
  character (`*` or `_`, `SLOT_EMPHASIS_CHAR`); its span covers the delimiters. A `*` with spaces on both sides opens
  and closes nothing: it is text.
- **Links and images** are spans too: their children are the link text (`the `, then the emphasis `_wiki_`) or the
  image's description (its alt text, `the plot`). `[the _wiki_][Notes]` is a reference: its destination comes from
  the definition `[notes]`, matched whatever the case. The definition itself is a block, not printed here (it has
  no inline content).

The whole program:

<<< @/examples/snippets/t05_inlines.f90

::: tip What you learned
`parse` turns the content of every paragraph and heading into inline nodes: text (merged, escapes and references
decoded), code spans, raw HTML, autolinks, soft and hard line breaks, emphasis, links and images, each spanning its
bytes in the source. Reference: [Inlines](/reference/inlines), [Links and references](/reference/links).
:::
