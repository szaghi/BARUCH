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
and no trailing space, the recorded style writes back 82.4% byte for byte ([Conformance](/reference/conformance#round-trip));
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
| emphasis, strong | `*`, `**`; `_`, `__` where `*` would not read back ([Emphasis](#emphasis)) |
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
| `*`, `_` | a run that could open (or close) emphasis with a run after (or before) it, the characters around it read as written (the flanking rules, CommonMark 6.2; a character written as a reference counts as `&` or `;`): an opener with a closer later in the text (the opener is escaped), a closer after an opener or inside an emphasis of the same character; a run touching an emphasis's delimiter of the same character; an opener when an emphasis's opening run after it can also close ([Emphasis](#emphasis)) |
| a letter next to an emphasis's delimiter | written as a numeric character reference where the delimiter would not flank otherwise: before an opener of `_` or of a content starting with punctuation, after a closer of `_` or of a content ending with punctuation (`&#108;*<!X y>*`) |
| `` ` `` | a backtick string when a backtick follows in the block (a link's destination and title count after its text), or one of the same length later in the text as written (escaped strings are strings of one backtick), or touching a code span |
| `[`, `]` | a pair that would make a link: followed by an inline link's tail `(...)` (judged on every reading the writer's own escapes allow, and as possibly ended by what follows the text), by a reference `[label]` that is defined, or holding a defined label; a `[` at the start of a paragraph or a setext heading that would make a link reference definition, judged on the line as written; inside a link's text, a lone bracket; a `]` closing an earlier `[` into a link. A link deactivates the `[` before it, not a `![` (CommonMark 6.3), and an inactive `[` stays on the stack: the next `]` pops it and needs nothing |
| `!` | before a link: it would make an image |
| `&` | before a character reference (`&amp;` is written `\&amp;`) |
| `<` | before raw HTML or an autolink (a tag, a comment, a declaration, a processing instruction, `<scheme:...>`, `<address@...>`), or before what could become one with what follows (a quoted attribute value running on, a link's `](...)` or a closer after the text) |
| `\` | before ASCII punctuation, at the end of a line (a hard break), before white space or a character written as a reference (`\&#108;` would read `\&`) |
| an extension's characters | the trigger and delimiter characters of the enabled extensions (`inline_triggers`, `delimiter_chars`) |

Text is never escaped by the bytes it holds alone: `snake_case`, `3 < 4`, `a * b`, `[x] (y)`, `AT&T` are written as they
are.

What is not text has its own rules:

- **Code span**: the backtick string recorded, else the shortest the content does not hold; a space inside each
  delimiter when the content starts or ends with a backtick, or with a space at both ends (the parser strips one).
- **Code block**: a fence longer than any line inside that would close it (a line indented by a tab counts); a space
  between the fence and an info string starting with the fence character; in the info string, line endings and the
  white space at its ends as references.
- **Destination**: between `<` and `>` when empty (a definition), when it holds white space or a control character,
  starts with `<` or has unbalanced parentheses; backslashes before punctuation and `&` before a reference escaped;
  a line ending as a reference.
- **Title**: between `"`, the `"` inside escaped; the white space starting a line after the first as references (a
  paragraph's continuation line loses it).
- **Raw HTML, a title, a code span spanning lines**: a continuation line that would open a block is indented by four
  spaces, which a paragraph's continuation line loses.
- **Reference links** keep their form only when the label (or the text, for a collapsed or shortcut reference) still
  finds a definition giving the link's destination and title, and a shortcut only when what follows would not make it
  another link (`(`, `[`, or `:` at the start of a paragraph): else the full form, else inline.

### Emphasis

An emphasis's delimiter is the recorded character (else `*`), unless it would not read back: when its opening (or
closing) delimiter touches one of the same character (the emphasis before it; the one around it, of the same kind,
when it is first or last in it and a run of them could open and close, as `**x*y*` reads `**x` and an emphasis;
strong around an emphasis that is all of its content, `***x***` being an emphasis around strong), or when its opening
run can also close and an emphasis around it has the same character (the run would close that one). The characters
around a delimiter are classified as the parser does (white space, punctuation, other), as written: a reference is
punctuation.

Then the delimiters written are **read back** as the parser reads them (`emphasis_reads_back`): the runs (the
delimiters written next to each other with one character, a text's unescaped runs among them), their flanking from
the bytes around them, and the delimiter algorithm of CommonMark 6.2 (from the inside out, the rule of 3), each link
text apart. Every emphasis's opener must pair with its own closer, whole, and no text's run may pair. When they would
not, the writer tries, in order: the texts' runs that can open or close escaped; a text's run touching a delimiter of
its character left to join the delimiter's run (as a source like `**a*b***` has it); every emphasis with its recorded
character (not in the canonical style), all `*`, all `_`; one emphasis, then two, then all, then all but one with the
other character. The first that reads back is written; if none does (a tree with no Markdown, or one past the
bounded search: 64 single changes, 256 pairs), the first choice is. A paragraph with no failing emphasis is written
once; the search is for the rare ones, and found by the fuzzer ([Conformance](/reference/conformance#fuzzing)).

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
of type 6 or 7; after a paragraph left open in a quote or an item of the first block, only a block read as a lazy
continuation line: a paragraph, a definition, a setext heading, an indented code block, an HTML block of type 7). A
definition counts as an open paragraph (it is read from one). An indented code block right after a list or another
indented code block is fenced: it would join them.

What the tree's shape asks besides:

- in an item of a tight list, a block quote ending with an open paragraph and followed by a block that would continue
  it is closed by an empty `>` line, not a blank line (which would make the list loose); a setext heading or an
  indented code block after an open paragraph there is written ATX or fenced;
- an HTML block of type 1 to 5 that its literal leaves open runs to the end of its container: no blank line is written
  after it (it would be its own), the container ends it;
- a paragraph or a setext heading starting with raw HTML right after a definition stays on the next line (after a blank
  line, the HTML would open an HTML block), its first line indented by 4 when its HTML could open one there: a
  continuation line of the definition's paragraph, which loses the indentation;
- an item's padding (the spaces after its marker) is read in columns, tabs to the next multiple of 4, and kept before
  a fenced code block; the last item of a list followed by an HTML block whose literal is indented gets more padding,
  so the block is not read as its own; an item whose first block is an indented HTML block has its marker alone on its
  line; a block after a list is not indented (recorded style) into the list's last item;
- an empty item behind two bullets of the same character on its line takes another bullet (`- - -` is a thematic
  break), and so does an indented code block first in an item (fenced) or a paragraph's first line (escaped) that
  would make one with the bullets.

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

- a fenced code block whose literal does not end with a line ending (one is added), an empty code span, two code spans
  side by side (their backtick strings are one);
- a tight list whose item holds two blocks only a blank line separates (two paragraphs; a paragraph after a list or a
  quote holding one, which would continue it lazily; a block after an HTML block of type 6 or 7), or a list marked
  loose whose only blank lines would follow definitions (a definition leaves no block, as in cmark, and the blank line
  after it ends nothing), as one item of one block;
- a heading of level 3 to 6 holding a line break or a title spanning lines, an empty paragraph (written as nothing),
  a paragraph or a heading starting with raw HTML (an HTML block, read back), a break at the start or the end of a
  paragraph, a heading or an emphasis;
- a link first in a paragraph whose code span or raw HTML holds `]:` and what a destination needs (a definition, read
  back: definitions are read before inlines);
- a link inside a link's text (links do not nest: CommonMark 6.3);
- some emphases nested at one edge: an emphasis in an emphasis in an emphasis whose content starts with punctuation
  (whatever the characters, the innermost opener can also close and closes the outermost), strong holding two strong
  of punctuation; for other deep nests, the writer's search of delimiter characters ([Emphasis](#emphasis)) is
  bounded.

Such a tree is written as close as Markdown allows, and reads back to a different tree: `equal` says where.
