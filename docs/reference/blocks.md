# Blocks

What `markdown_options%parse` builds from a document's source: the block structure of CommonMark 0.31.2 (spec
sections 4 and 5), one node per block, with the source style each block was written in. Modules `baruch_blocks` (the
parser), `baruch_blocks_state`, `baruch_blocks_scan` and `baruch_blocks_definitions`, all internal: a client calls
`parse` and reads the tree through [the document](/reference/document)'s accessors. A worked example:
[tutorial chapter 3](/manual/tutorial/03-read).

::: info Status (plan step 1.5)
The block phase is complete: every block construct of the spec, checked by the 237 examples of the
[blocks-only subset](/reference/conformance#the-blocks-only-subset) (all pass). The block phase leaves the content of
a paragraph or a heading as one text node; the **inline** phase, which `parse` runs next, replaces it with the
inline nodes ([Inlines](/reference/inlines)).
:::

## Parsing

```fortran
call doc%load_file('report.md', stat=stat, errmsg=errmsg)   ! or doc%load(text)
call options%parse(doc, stat=stat, errmsg=errmsg)
```

`parse` resets the tree to its root (the source and its lines are kept), lets the enabled extensions register their
kinds, and reads the lines in one pass, as the spec's appendix "A parsing strategy" describes:

1. each open block, outermost first, says whether the line continues it and consumes its marker (`>`, an item's
   indentation, four columns of indented code...); the first that does not ends the walk;
2. at the position reached, the [extensions'](/reference/extensions) `block_start` hooks are tried first, in
   registration order, then the core's starts in the spec's order: block quote, ATX heading, code fence, HTML block,
   setext underline, thematic break, list item, indented code. A new container tries again on the rest of the line;
3. the rest of the line goes to the block it belongs to: a lazy continuation line to the open paragraph, a code or
   HTML block takes it whole, a paragraph or heading from its first non-blank byte; anything else opens a paragraph.

Parsing never fails on the text: every byte sequence is a document. `stat` is nonzero only when an extension's hook
fails or breaks the hook contract, or memory runs out.

- **No recursion.** The open blocks are a stack of arrays; the tree is linked as it grows, each block appended with
  no children (O(1) at any depth). A 10<sup>5</sup>-deep nesting of `>` parses within the default 8 MB stack
  (`baruch_test_blocks`), as do 10<sup>5</sup> nested list items.
- **No copies of the text per container.** The source is copied once; a line is a slice of it, and a container's
  marker is consumed by moving an offset and a column (tab stops of 4). A block's strings are built when it closes.
- **No tombstones in a parsed tree.** A setext underline turns its paragraph into a heading and a paragraph of link
  reference definitions only goes away; the tree is compacted once at the end of such a parse, so the handles of a
  parsed document run from 1 to `nodes_number()` with no gap. They follow the order the nodes were created in, not
  necessarily the document order (a heading made from a paragraph comes after the paragraph's text): walk the links.

## Kinds and their fields

| Kind | Opened by | Children | Fields |
|---|---|---|---|
| `NODE_DOCUMENT` | the root | blocks | |
| `NODE_BLOCK_QUOTE` | `>` (up to 3 columns of indentation; one space or tab after it is part of the marker) | blocks | |
| `NODE_LIST` | the first item of a run of items with the same bullet, or the same ordered delimiter | items | `list_type`, `list_start`, `list_tight`, the marker |
| `NODE_ITEM` | `-`, `+`, `*`, or 1 to 9 digits and `.` or `)`, then a space, a tab or the end of the line | blocks | |
| `NODE_PARAGRAPH` | a line that starts nothing else | one text node | |
| `NODE_HEADING` | `#` to `######` and a space, a tab or the end of the line; or a paragraph underlined by `=` or `-` | one text node, none if empty | `heading_level`, the form, the closing `#`s |
| `NODE_CODE_BLOCK` | three or more `` ` `` or `~`; or four columns of indentation outside a paragraph | none | `literal`, `info`, the form, the fence |
| `NODE_HTML_BLOCK` | the seven start conditions of spec 4.6 (a lone tag, type 7, cannot interrupt a paragraph) | none | `literal` |
| `NODE_THEMATIC_BREAK` | three or more `-`, `*` or `_`, alone with spaces and tabs | none | the character |
| `NODE_LINK_REFERENCE_DEFINITION` | `[label]: destination "title"` at the start of a paragraph | none | `label`, `destination`, `title` |
| `NODE_TEXT` | the content of a paragraph or a heading, until the inline phase replaces it | none | `literal` |

### Content

- **Paragraph text**: the paragraph's lines without their leading spaces and tabs, joined by LF, the trailing white
  space of the last line removed. Nothing else is processed: it is the subject of the inline parser
  ([Inlines](/reference/inlines)), which replaces the text node with the inline nodes before `parse` returns. The text
  node has the paragraph's span.
- **Heading text**: an ATX heading's line without its opening `#`s, the white space around it and its closing
  sequence (a run of `#` after a space or a tab, or the whole content when it is only `#`s: `### ###` is an empty
  heading); a setext heading's paragraph text. An empty heading has no child.
- **Code block literal**: the lines of the block, each followed by LF (an empty block: an empty literal). An indented
  block drops four columns from each line and its trailing blank lines; a fenced one drops, from each line, up to as
  many spaces as the opening fence was indented. **Info string**: the rest of the opening fence line, trimmed, its
  backslash escapes and character references processed (no backtick allowed after a backtick fence).
- **HTML block literal**: its lines as written (leading spaces of the first included), each followed by LF. Types 1 to
  5 end on the line meeting their end condition (`</script>`, `</pre>`, `</style>`, `</textarea>`, `-->`, `?>`, `>`,
  `]]>`), which belongs to the block; types 6 and 7 end before a blank line.
- **Link reference definitions** are nodes in place, before the paragraph they were read from (a paragraph made of
  definitions only goes): the `label` as written between the brackets (matching normalises it: [Links and references](/reference/links#matching-a-label)); the
  `destination` without its `<>`, the `title` without its delimiters, both with their backslash escapes and character
  references processed. A
  definition cannot interrupt a paragraph, and under a setext underline a paragraph's definitions come out first (an
  underline under definitions only is paragraph text, spec example 216).

Backslash escapes and entity and numeric character references (`&ouml;`, `&#42;`) in an info string, a destination
or a title are processed in one pass, left to right, with the
[HTML5 entity table](/reference/conformance#the-html5-entity-table) ([Inlines](/reference/inlines#strings-of-the-blocks)).

Line endings are LF in every string, whatever the source used (CR, CRLF); the [spans](#spans) keep pointing at the
source as loaded.

### Lists

An item opens a list unless the open block at that position is a list of the same type and the same bullet or
delimiter: `- a` then `+ b` are two lists. `list_start` is the number of an ordered list's first item (0 for a bullet
list); a list item interrupting a paragraph must have content and, when ordered, start at 1.

A list is **loose** if one of its items is followed by a blank line before the next, or if two blocks of one item are
separated by a blank line (a blank line at the end of the last item does not count), as cmark decides it (spec 5.3).
`list_tight` is set when the list closes. Link reference definitions inside an item are not blocks of it for this
rule.

An item's content starts at the column of its first non-blank character after the marker (1 to 4 columns after it);
with 5 or more, or nothing, the content column is one past the marker and the rest is indented code. Its lines must
be indented that far, or be lazy continuation lines of a paragraph.

### Tabs

A tab is not expanded; where indentation defines structure it counts to the next multiple of 4 columns. A marker can
end inside a tab (`-` then a tab, `>` then a tab): the tab is then **partly consumed**, and a code block taking the
line gets the tab's remaining columns as spaces (`- foo`, a blank line, two tabs and `bar` give the literal
`"  bar\n"`, spec example 5). An extension's block taking such a line receives the tab whole: the byte ranges its
`block_close` hook gets cannot hold part of a tab.

## Style slots

What the parser records of how a block was written, in the integer slots of [the slot table](/reference/document#the-slot-table)
(0 means "not recorded"). The tree writer (plan step 3.2) writes a block back in this style; equality (step 3.1)
ignores it.

| Kind | Slot | Value |
|---|---|---|
| heading | `SLOT_HEADING_FORM` | `HEADING_ATX` or `HEADING_SETEXT` (the level gives `=` or `-`) |
| | `SLOT_HEADING_CLOSING` | the closing `#`s of an ATX heading, 0 if none |
| list | `SLOT_LIST_MARKER` | `iachar` of the bullet (`-`, `+`, `*`) or of the ordered delimiter (`.`, `)`) |
| code_block | `SLOT_CODE_FORM` | `CODE_INDENTED` or `CODE_FENCED` |
| | `SLOT_CODE_FENCE_CHAR`, `SLOT_CODE_FENCE_LENGTH` | `iachar` of `` ` `` or `~`, and the opening fence's length (3 or more) |
| thematic_break | `SLOT_BREAK_CHAR` | `iachar` of `-`, `*` or `_` |

The meaning slots are set too: `SLOT_HEADING_LEVEL`, `SLOT_LIST_TYPE`, `SLOT_LIST_START`, `SLOT_LIST_TIGHT` (through
`heading_level`, `list_type`, `list_start`, `list_tight`). No other slot is used by the block parser.

## Spans

Every block has a span (`span_first`, `span_last`): from the first byte of its marker (or of its first non-blank
character: a paragraph, an indented code block) to the last byte of its last line that is not blank once the markers
of the containers around it are consumed (a `>` alone on a line of a block quote does not extend a list inside the
quote), endings excluded. A list ends where its last item ends. A container's span includes the markers of the
containers inside it, and its lazy continuation lines; a fenced code block's includes its closing fence; a setext
heading's its underline. The root spans the whole source.

```text
> quoted          block quote: "> quoted\n> more"
> more            its paragraph: "quoted\n> more"
```

## Errors

`parse` returns `BARUCH_OK` unless an extension fails a hook (its own status, the message naming it) or breaks the
[hook contract](/reference/extensions#the-contract) (`BARUCH_ERR_CONTRACT`); the tree then holds what was parsed
before.
