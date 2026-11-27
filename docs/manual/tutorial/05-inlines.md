# 5. Inline content

Chapter 3 read the blocks of a document; inside each paragraph and heading there is more: code spans, links, raw
HTML, character references, line breaks. `parse` reads them too, after the blocks, into **inline nodes**: children of
the paragraph or heading, each with its kind, its content and its bytes in the source. This chapter parses a short
document, prints the inline nodes of each block, and renders it.

::: info Plan step 2.3
Written now: backslash escapes, entity and numeric character references, code spans, autolinks, raw inline HTML,
hard and soft line breaks, text. Emphasis, links and images (`*rig*`, `[text](url)`, `![alt](src)`) are plan step
2.4: until then they stay text. [Inlines](/reference/inlines) has the details.
:::

## Parse

<<< @/examples/snippets/t05_inlines-parse.f90

Nothing new to call: `parse` runs the block phase, then the inline phase over every paragraph and heading. The
document above is built from a string (`load`), so the example needs no file.

## The inline nodes

<<< @/examples/snippets/t05_inlines-inlines.f90

Each block's children are its inline nodes, in order; `print_inlines` (in the whole program below) prints each one's
kind, its span and what it holds: the `literal` of a text, a code span or raw HTML, the `destination` of a link and
the literal of its text child.

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

The whole program:

<<< @/examples/snippets/t05_inlines.f90

::: tip What you learned
`parse` turns the content of every paragraph and heading into inline nodes: text (merged, escapes and references
decoded), code spans, raw HTML, autolinks, soft and hard line breaks, each spanning its bytes in the source.
Emphasis, links and images join them in plan step 2.4. Reference: [Inlines](/reference/inlines).
:::
