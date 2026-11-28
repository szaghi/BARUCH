# 6. Edit and write back

The tree is not only for reading: change it, then write it back as Markdown. `write_markdown` writes a document in the
style it was read in, or in a canonical style, and what it writes reads back to the same tree. This chapter parses a
short document, writes it back unchanged, edits it (a heading's text, a new list item) and writes it in both styles.

::: info Plan step 3.2
The tree writer: the recorded style, the canonical style, minimal escaping. Its rules are in
[Writer](/reference/writer).
:::

## Read, write back

<<< @/examples/snippets/t06_write-parse.f90

`write_markdown` is a method of the options, as `parse` and `render_html` are: an extension's node is written by the
extension. With no edit the document comes back byte for byte: the setext heading, the `_` emphasis, the `*` bullets,
the `~~~` fence with its info string, the reference link and its definition are the parser's **recorded style**
(the style slots of [the document](/reference/document#the-slot-table)), and the blank lines, an item's spacing, a
fence's line are read from the source where no slot holds them.

## Edit

<<< @/examples/snippets/t06_write-edit.f90

The heading's text is its text node's literal; the new item is three nodes built by hand (item, paragraph, text) and
appended to the list. A new node has no recorded style and no place in the source: the writer gives it the list's
style (the `*` bullet, tight) and the canonical rest.

## Write in the recorded style

<<< @/examples/snippets/t06_write-recorded.f90

## Write in the canonical style

<<< @/examples/snippets/t06_write-canonical.f90

## Read it back

<<< @/examples/snippets/t06_write-check.f90

## Running it

<<< @/examples/output/t06_write.ansi{ansi}

What the output shows:

- **Only the edits change.** In the recorded style the heading keeps its underline (`============`, as long as it
  was: any length underlines a setext heading) and every other line is the source's.
- **The new text is escaped, minimally.** Its literal is `*not* emphasis, [not] a link, 3 < 4`: read back as written,
  the first `*` would open emphasis, so it is written `\*`; the second can then close nothing, and is left alone. `[not]`
  makes no link (no `(` after it, no definition of `not`), and `3 < 4` no HTML: nothing to escape there. What is
  escaped and why: [the escaping rules](/reference/writer#escaping).
- **The canonical style** ignores what was recorded: an ATX heading, `*` for emphasis, `-` bullets, backtick fences,
  inline links (the definition stays, in place: it is a node of the tree).
- **Both read back to the same tree**: `equal` compares what the documents mean, not how they are written
  ([Equality](/reference/equality)). The conformance suite checks it on all 652 CommonMark examples, in both styles
  ([Conformance](/reference/conformance#round-trip)).

## The whole program

<<< @/examples/snippets/t06_write.f90
