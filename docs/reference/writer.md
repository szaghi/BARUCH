# Writer

A document's tree as Markdown: `write_markdown` and `write_markdown_file`, methods of
[`markdown_options`](/reference/extensions#the-options); the work is done by module `baruch_writer` (internal, but for
`escape_markdown`). The writer is the inverse of the parser (settled design 2 of the plan): **what it writes, `parse`
reads back to the same tree** ([Equality](/reference/equality)), not necessarily to the same bytes. A worked example:
[tutorial chapter 6](/manual/tutorial/06-write).

```fortran
call options%write_markdown(doc, markdown)                    ! the recorded style
call options%write_markdown(doc, markdown, canonical=.true.)  ! the canonical style
call options%write_markdown_file(doc, 'out.md', stat=stat, errmsg=errmsg)
```

## The methods

| Method | |
|---|---|
| `write_markdown(doc, markdown, canonical, stat, errmsg)` | into a `character(:), allocatable` or a StringiFor `string` |
| `write_markdown(doc, unit, canonical, stat, errmsg)` | into a unit the caller opened for writing: one record per line (formatted), the bytes (unformatted stream) |
| `write_markdown_file(doc, path, canonical, stat, errmsg)` | into a file, created or replaced, byte for byte |

| Argument | | |
|---|---|---|
| `doc` | `in` | the document: parsed, built by hand, or edited |
| `canonical` | `in`, optional | `.true.`: ignore the recorded style (default `.false.`) |
| `stat`, `errmsg` | `out`, optional | [Conventions](/guide/conventions); `BARUCH_ERR_IO` for a unit or a file that cannot take the text, an extension's own status when its `write_markdown` hook fails (the message names the extension; the output holds what was written before it) |

The sinks are those of [`render_html`](/reference/html): a unit that is not open, open for reading only, or unformatted
but not stream gives `BARUCH_ERR_IO`. Line endings are LF. An empty document (never initialised) writes nothing.

## The recorded style

The parser records how a document was written, in the style slots of
[the slot table](/reference/document#the-slot-table); the writer reuses them:

| Kind | Recorded | Written |
|---|---|---|
| heading | `SLOT_HEADING_FORM`, `SLOT_HEADING_CLOSING` | ATX or setext; an ATX heading's closing `#`s |
| list | `SLOT_LIST_MARKER` | the bullet (`-`, `+`, `*`) or the delimiter (`.`, `)`); items numbered from the start, one by one |
| code_block | `SLOT_CODE_FORM`, `SLOT_CODE_FENCE_CHAR`, `SLOT_CODE_FENCE_LENGTH` | indented or fenced, the fence character and length |
| thematic_break | `SLOT_BREAK_CHAR` | the character |
| emph, strong | `SLOT_EMPHASIS_CHAR` | `*` or `_` |
| link, image | `SLOT_LINK_FORM`, `STRING_LABEL` | inline, full `[text][label]`, collapsed `[text][]`, shortcut `[text]`, autolink `<...>` |
| linebreak | `SLOT_LINEBREAK_FORM`, `SLOT_LINEBREAK_SPACES` | a backslash, or as many spaces as were written |
| code | `SLOT_CODE_TICKS`, `STRING_RAW` | the backtick string's length; the content as written (padding spaces, line endings) |

Where no slot holds it, the recorded style reads the **source** (the document's own buffer, through the nodes'
[spans](/reference/document#spans)), and keeps what it finds only when it says the same thing:

- the **blank lines** between two blocks outside lists (none, one, several), and after the last block (or no final
  line ending);
- the **indentation** (0 to 3 spaces) of a block of the document's own (a paragraph, a heading, a thematic break, a
  definition, a block quote, an item), and of a paragraph's continuation lines;
- the **spaces after an item's marker** (1 to 4), which set the indentation of the item's other lines;
- a **thematic break's** line (`- - -`, `*****`), a **setext underline** (`=====`), a **fence** line (`~~~ ini`, with its
  info string as written) and a longer closing fence: each kept only when the scanner reads it as the same thing.

A node with no span (built or moved by hand) and a span that does not fit (its node moved away) fall back to the
canonical style for that choice; a choice that would change the tree is never made (a recorded `-` thematic break under
an open paragraph is written `***`, it would be a setext underline). Measured on the 652 CommonMark examples with no tab
and no trailing space, the recorded style writes back 82% byte for byte ([Conformance](/reference/conformance#round-trip));
the rest is what the tree does not keep: escapes and character references the source used where none is needed
(`\$`, `&copy;`), container markers written without their space (`>foo`) or lazily left out, a link's title quotes
and spacing, an unclosed fence.

## The canonical style

With `canonical = .true.`, or where nothing is recorded (a tree built by hand):

| Construct | Canonical |
|---|---|
| heading | ATX, `#` × level, one space, no closing sequence; setext (`===`, `---`) only for a level 1 or 2 heading spanning lines |
| bullet list | `-`; a list right after another bullet list takes `*` (then `+`): two lists of the same marker in a row would be one |
| ordered list | `1.` numbering from the start; `)` for a list right after another ordered list |
| item | the marker, one space; the item's other lines indented by the marker's width plus one |
| code block | fenced with three backticks (more than any backtick fence inside); `~~~` when the info string holds a backtick |
| thematic break | `***`; `-` or `_` where `*` would make the line something else (`* ***`, the bullet of the item it starts) |
| emphasis, strong | `*`, `**`; `_`, `__` when the same character would touch the delimiter of the emphasis before or around it (`**_x_**`) |
| link, image | inline `[text](destination "title")`; an autolink when the link is one (its text its destination, a URI or an email) |
| hard line break | a backslash |
| blocks | one blank line between two blocks; none between the items and the blocks of a tight list |
| definition | in place, `[label]: destination "title"`, the label as written |

## Escaping

A text is escaped only where reading it back would change the tree, as the parser decides (settled design 3): the
character is written with a backslash (an ASCII punctuation character), or as a numeric character reference (white
space, a line ending).

| Where | What is escaped |
|---|---|
| the start of a line of a paragraph or a setext heading | what would open a block, as the block scanners read the line: `#` and a space (ATX), `>`, a bullet and a space, `1.`/`1)` and a space (the delimiter is escaped: `1\.`), a fence, a thematic break, `<` and an HTML block start; on a line after the first, only what can interrupt a paragraph (an ordered list starting at 1, a bullet followed by text), and `=` or `-` alone (a setext underline) |
| the start and the end of a line | white space (`&#32;`, `&#9;`): the parser strips it, or four spaces start a code block, two before a line ending a hard break |
| an ATX heading | a closing sequence of the content (`# a #` is written `# a \#`) |
| `*`, `_` | a run that could open (or close) emphasis with a run after (or before) it, the characters around it read as written (the flanking rules, CommonMark 6.2): an opener with a closer later in the text (the opener is escaped), a closer after an opener or inside an emphasis of the same character; a run touching an emphasis's delimiter of the same character |
| `` ` `` | a backtick string when one of the same length follows in the block (it would open a code span), or touching a code span |
| `[`, `]` | a pair that would make a link: followed by an inline link's tail `(...)`, by a reference `[label]` that is defined, or holding a defined label; a `[` at the start of a paragraph that would make a link reference definition; inside a link's text, a lone bracket; a `]` closing an earlier `[` into a link. A link deactivates the `[` before it (CommonMark 6.3): a `]` after a link needs nothing |
| `!` | before a link: it would make an image |
| `&` | before a character reference (`&amp;` is written `\&amp;`) |
| `<` | before raw HTML or an autolink (a tag, a comment, a declaration, a processing instruction, `<scheme:...>`, `<address@...>`), or before what could become one with what follows |
| `\` | before ASCII punctuation, at the end of a line (a hard break) |
| an extension's characters | the trigger and delimiter characters of the enabled extensions (`inline_triggers`, `delimiter_chars`) |

Text is never escaped by the bytes it holds alone: `snake_case`, `3 < 4`, `a * b`, `[x] (y)`, `AT&T` are written as they
are.

What is not text has its own rules:

- **Code span**: the backtick string recorded, else the shortest the content does not hold; a space inside each
  delimiter when the content starts or ends with a backtick, or with a space at both ends (the parser strips one).
- **Code block**: a fence longer than any line inside that would close it.
- **Destination**: between `<` and `>` when empty (a definition), when it holds white space or a control character,
  starts with `<` or has unbalanced parentheses; backslashes before punctuation and `&` before a reference escaped.
- **Title**: between `"`, the `"` inside escaped.
- **Raw HTML, a title, a code span spanning lines**: a continuation line that would open a block is indented by four
  spaces, which a paragraph's continuation line loses.
- **Reference links** keep their form only when the label (or the text, for a collapsed or shortcut reference) still
  finds a definition giving the link's destination and title, and a shortcut only when what follows would not make it
  another link (`(`, `[`, or `:` at the start of a paragraph): else the full form, else inline.

`escape_markdown(text, line_start, first_line)` (module `baruch_writer`) is the text rule for a text whose neighbours
are unknown, the most cautious reading of each rule: every run that could open or close emphasis, every backtick and
every lone bracket escaped; with `line_start`, a line's start too. It returns the text escaped; written inside a
paragraph, it reads back as that text, whatever is around it. The streaming writer (plan step 4.1) uses it.

## Blocks and containers

Containers are line prefixes, kept on a stack: `> ` for a block quote, the item's marker on its first line and as many
spaces after it; a blank line drops the prefixes' trailing white space (`>`). An empty item or block quote is its marker
alone. Between two blocks: a blank line (in the recorded style, the source's); none between the items of a tight list
and between the blocks of its items, unless the second block could not follow the first without merging with it (a
paragraph after a paragraph, an indented code block or a definition after an open paragraph, a block after an HTML block
of type 6 or 7). An indented code block right after a list or another indented code block is fenced: it would join
them.

The walk follows the tree's links: no recursion, constant stack, any depth (`baruch_test_roundtrip` writes 10^5 nested
block quotes and 10^4 nested lists).

## Extensions

Every node is offered to the enabled extensions' `write_markdown` hooks, on entering it and on leaving it, in
registration order, core kinds too; the first that handles an event writes it ([the hook](/reference/extensions)).

| Node | Entering | Leaving |
|---|---|---|
| a block | written as lines behind the containers' prefixes; when the block has children, the text after the last line feed is the **prefix of every line of its children** (`!> ` for a container like a block quote, empty for one with a closing line like `::: tip`) | lines after the children |
| a block of inline content | its lines, then the prefix (after the last line feed) of each line of the inline content | lines after it |
| an inline node | written as it is, before the children | after the children |

An extension's node that no enabled extension writes is written **transparently**: its children as if it were not
there; a leaf's literal as text (a block one as a paragraph). Nothing is dropped silently.

## Trees that cannot be written

The writer writes any tree the parser makes. A tree built by hand can hold what no Markdown reads back:

- a fenced code block whose literal does not end with a line ending (one is added), an empty code span;
- a tight list whose item holds two blocks only a blank line separates (two paragraphs), or a list marked loose with
  one item of one block;
- a heading of level 3 to 6 holding a line break, an empty paragraph (written as nothing);
- a link inside a link's text (links do not nest: CommonMark 6.3).

Such a tree is written as close as Markdown allows, and reads back to a different tree: `equal` says where.
