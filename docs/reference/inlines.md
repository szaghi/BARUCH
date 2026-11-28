# Inlines

What `markdown_options%parse` builds inside each paragraph and heading: the inline content of CommonMark 0.31.2 (spec
section 6), one node per construct, children of the block, each with its span in the source. Modules
`baruch_inlines` (the parser) and `baruch_inlines_scan` (the scanners), both internal: a client calls `parse` and
reads the tree through [the document](/reference/document)'s accessors. A worked example:
[tutorial chapter 5](/manual/tutorial/05-inlines).

::: info Status (plan steps 2.3, 2.4)
Written: backslash escapes, entity and numeric character references, code spans, autolinks, raw inline HTML, hard and
soft line breaks, textual content (2.3); emphasis and strong emphasis, links and images in their four forms, the
resolution of references against the link reference definitions (2.4, links and images on their own page:
[Links and references](/reference/links)). Every inline section of the spec passes 100%, the whole suite 652/652
([Conformance](/reference/conformance#counts)). The extensions' inline hooks are called (2.5): trigger bytes,
delimiter characters paired by the delimiter algorithm, blocks of an extension with inline content
([Extensions](/reference/extensions#inline-triggers)); with no extension enabled the parse is CommonMark's alone.
:::

## When it runs

`parse` runs the block phase ([Blocks](/reference/blocks)), then the inline phase over every paragraph and heading
(and every block of an enabled extension whose `contains_inlines` is true):
the block parser leaves in each one text node holding its content (its lines without their leading white space,
joined by LF, the trailing white space of the last removed), and the inline parser replaces it with the inline nodes.
All the link reference definitions of the document are known by then, as references need: the inline phase
collects them first ([Links and references](/reference/links#reference-definitions)). The tree is
compacted at the end, so a parsed document has no tombstone; the handles follow the order the nodes were created in
(blocks first, then each block's inline nodes), not the document order: walk the links.

An empty heading (`##`) has no child. Code blocks and HTML blocks have no inline content: their `literal` is the
block parser's. After the inline phase the enabled extensions' `post_process` hooks run
([Extensions](/reference/extensions#post-processing)).

**Extensions.** A byte the core does not read and an enabled extension declares is offered to it: a trigger to its
`inline_start` (a node of its kind, linked in place with its span), a delimiter character to the delimiter algorithm
below, paired by its `delimiter_run` and `delimiter_match` hooks. The core's bytes (`` ` `` `\` `&` `<` `*` `_` `[`
`]` `!` and the line ending) cannot be declared, so escapes, code spans, autolinks and raw HTML hide extension syntax
as they hide emphasis.

## Kinds and their fields

| Kind | Written as | Fields | Span |
|---|---|---|---|
| `NODE_TEXT` | any text: plain characters, backslash escapes, character references, characters that open nothing | `literal`: the text as it reads (escapes and references decoded) | the source bytes, escapes and references as written |
| `NODE_CODE` | `` `code` ``: a backtick string, the content, a backtick string of the same length | `literal`: the content (line endings as spaces, one space stripped at both ends, see below); `SLOT_CODE_TICKS` (style): the backticks of the strings; `STRING_RAW` (style): the content as written, when it is not the literal | the backtick strings included |
| `NODE_HTML_INLINE` | an open or closing tag, a comment, a processing instruction, a declaration, a CDATA section | `literal`: the bytes as written (line endings LF) | the HTML |
| `NODE_LINK` (autolink) | `<scheme:...>`, `<address@domain>` | `destination`: the URL, or `mailto:` and the address, references decoded; `title` empty; `SLOT_LINK_FORM` = `LINK_AUTOLINK`; one `NODE_TEXT` child, the URL or the address | `<` to `>` (its text: inside them) |
| `NODE_LINEBREAK` | two or more spaces, or a backslash, then a line ending | `SLOT_LINEBREAK_FORM` (style): `LINEBREAK_SPACES` or `LINEBREAK_BACKSLASH`; `SLOT_LINEBREAK_SPACES` (style): the spaces | the spaces (or the backslash) and the line ending |
| `NODE_SOFTBREAK` | a line ending | none | the line ending's first byte |
| `NODE_EMPH` | `*text*`, `_text_` | `SLOT_EMPHASIS_CHAR` (style): `*` or `_` (`iachar`); children: the emphasised content | the delimiters included |
| `NODE_STRONG` | `**text**`, `__text__` | as emphasis | the delimiters included |
| `NODE_LINK` | `[text](destination "title")`, `[text][label]`, `[label][]`, `[label]` | `destination`, `title`, `label`, `SLOT_LINK_FORM`; children: the link text ([Links and references](/reference/links)) | `[` to the last `)` or `]` |
| `NODE_IMAGE` | `![description](...)` and the same three reference forms | as a link; children: the description, whose plain text is the alt text | `!` to the last `)` or `]` |

`literal`, `destination` and `title` are the accessors of [the document](/reference/document#typed-accessors); the
renderer writes these kinds as [HTML](/reference/html#what-is-written) describes.

### Text

Text is **merged**: one node per run of text, whatever it is made of. `a\*b &amp; c` is one text node, `a*b & c`. A
run ends at another inline node (a code span, raw HTML, a link, emphasis, a line break) or at the end of the block.
Text holds what the source means, its span what was written.

- **Backslash escapes** (spec 2.4): a backslash before an ASCII punctuation character is that character
  (``\*``, ``\` ``, ``\\``...); a backslash before anything else is a backslash. Not in code spans, autolinks or raw
  HTML.
- **Character references** (spec 2.5): `&name;` for the 2,125 HTML5 names ending in `;` (the
  [entity table](/reference/conformance#the-html5-entity-table)), `&#` and 1 to 7 decimal digits, `&#x` (or `&#X`)
  and 1 to 6 hexadecimal digits, then `;`, as their UTF-8 bytes. A numeric reference to U+0000, to a surrogate
  (U+D800 to U+DFFF) or beyond U+10FFFF is U+FFFD. Anything else (`&copy` with no `;`, `&bogus;`, `&#12345678;`)
  is text as written. Not in code spans or raw HTML.
- Trailing spaces and tabs before a line ending are not text (they make a hard break or nothing); the leading spaces
  of a line are not text either (the block parser removed them).

### Code spans

A backtick string opens a code span, closed by the next backtick string of the **same length**; with none, the opening
string is text and the parse goes on after it. The content: line endings become spaces, then, if it begins **and**
ends with a space and is not only spaces, one space is removed at each end (``` `` `x` `` ``` holds `` `x` ``). Nothing
else is processed: `` `\*` `` holds `\*`. The style is recorded for the [writer](/reference/writer): the length of the
backtick strings (`SLOT_CODE_TICKS`) and, when the literal is not what was written between them (a line ending, the
stripped spaces), the content as written (`STRING_RAW`).

### Autolinks

- **URI**: `<`, a scheme (a letter, then 1 to 31 letters, digits, `+`, `.` or `-`), `:`, then any characters but
  ASCII controls (U+0000 to U+001F, U+007F), spaces, `<` and `>`, then `>`. `<made-up-scheme://foo,bar>` is one;
  `<https://foo bar>` is not.
- **Email**: `<`, one or more of letters, digits and ``.!#$%&'*+/=?^_`{|}~-``, `@`, labels of 1 to 63 letters,
  digits and `-` (not first, not last) separated by `.`, `>`. The destination gets `mailto:`.

Backslash escapes do not work in an autolink (`<https://a/\[>` has the destination `https://a/\[`); character
references do. The renderer percent-encodes the destination as cmark does (`escape_href`).

### Raw HTML

The spec's six forms (6.6), the tag scanners shared with the HTML blocks: an **open tag** (`<`, a tag name, attributes,
optional `/`, `>`), a **closing tag** (`</name>`), a **comment** (`<!-->`, `<!--->`, or `<!--` to the first `-->`), a
**processing instruction** (`<?` to `?>`), a **declaration** (`<!` and a letter, to `>`), a **CDATA section**
(`<![CDATA[` to `]]>`). White space in a tag may include line endings. What is not one of them is text: `<` and the
rest read on.

### Emphasis

A run of `*` or of `_` (a **delimiter run**: as many of the same character as follow each other, not escaped) can
open emphasis, close it, both or neither, by what is around it (spec 6.2):

- it is **left-flanking** when the character after it is not white space, and either not punctuation or preceded by
  white space or punctuation; **right-flanking** the other way round. White space is Unicode White_Space (U+00A0,
  U+3000... too), punctuation is Unicode punctuation and symbols (General Categories P and S: `«`, `—`, `€` too),
  both through StringiFor (`is_space(unicode=.true.)`, `is_punct(unicode=.true.)`); the start and the end of the
  block count as white space;
- a `*` run opens when left-flanking and closes when right-flanking; a `_` run opens when left-flanking and either
  not right-flanking or preceded by punctuation, and closes symmetrically: `snake_case_name` holds no emphasis.

Matching follows the spec's appendix: each closer, left to right, takes the nearest opener of the same character
before it, unless one of them can both open and close and the sum of the two runs' lengths (as written) is a multiple
of 3 while they are not both multiples of 3 (the **rule of 3**: `*foo**bar**baz*` is emphasis around a strong
`bar`). Two characters from each make strong emphasis when both runs have two left, else one makes emphasis; the
rest of a run is matched again or stays text (`**a*` is the text `*` and emphasis around `a`). Emphasis nests in
the order the closers come: `***a***` is emphasis around strong emphasis.

The node records the character (`SLOT_EMPHASIS_CHAR`), so a writer gives back `_a_` as `_a_`. Its span runs from the
first delimiter it uses to the last; a run's leftover characters are a text whose span is just them.

### Line breaks

A line ending inside a paragraph or a heading is a **soft break**; preceded by two or more spaces, or by a backslash,
it is a **hard break**. At the end of a block neither: `foo\` ends with a backslash, trailing spaces were removed by
the block parser.

## Spans

Every inline node has the span of its bytes in the source (`span_first`, `span_last`), as the blocks do. The inline
parser reads the block's content (its lines joined by LF, the container markers left out) and keeps a **line map**:
for each line of the content, its first byte in the source. So a span inside a block quote or a list item skips the
`>` and the indentation, and a node spanning lines (a code span, a tag, a merged text) runs from its first byte to its
last, the markers between them included. A line ending is mapped to its first byte (the CR of a CRLF).

```text
> a  \n> `b`        text "a": byte 3; linebreak: bytes 4-6 (two spaces, the LF); code "b": bytes 9-11
```

## Strings of the blocks

The block parser stores the **info string** of a fenced code block and the **destination** and **title** of a link
reference definition with their backslash escapes **and** character references processed, in one pass left to right
(`unescape_string`): `` ``` f&ouml;\* `` has the info string `fö*`; `\&amp;` is `&amp;` (the escape makes `&` a
character, which then starts no reference), `&#92;*` is `\*` (a reference's bytes are not read again). The label of a
definition is kept as written: matching a reference normalises it ([Links and references](/reference/links#matching-a-label)).

## How it is built

The parser follows the spec's appendix "A parsing strategy" and cmark's `inlines.c` (BSD-2), whose structure it keeps;
nothing recurses and nothing is module state: one `inline_parser` per parse holds the arrays, reused block after
block.

- **The subject and the cursor.** Each block's content is the subject; one cursor reads it left to right, once. Plain
  text is found with `scan` for the next special byte (LF, `` ` ``, `\`, `&`, `<`, `*`, `_`, `[`, `]`, `!`, and the
  bytes the enabled extensions declare); a node's literal is a slice or the few bytes a construct stands for. Text is
  accumulated and written as one node when something else comes.
- **The delimiter stack** holds the runs of `*` and `_` (and of the extensions' delimiter characters) that can open or
  close (each a text node of its own): character, count left, original count, first offset left, can open, can close,
  the extension owning the character, linked both ways through indices so that a run can leave from its middle. **The
  bracket stack** holds the `[` and `![` (each a text node too): position, image or not, active, a bracket after it,
  the first delimiter pushed after it.
- **Emphasis** is matched by cmark's `process_emphasis`: forward over the closers, back to an opener. A closer that
  finds none raises the lower bound of later searches for its key (character, can open, original length modulo 3:
  `openers_bottom`), so no opener is looked at twice for nothing: linear. The nodes between a matched opener and
  closer move under the new node, which is linked last (no ancestor walk per child). A run's text node is rewritten
  once, when the run leaves the stack, to the characters no match used. A closer of an extension's character is paired
  by its extension's `delimiter_match`, opener by opener, with its own lower bound (character, can open).
- **Links** are resolved at `]` against the top bracket only; a link deactivates the `[` below it, the walk following
  a chain that links each bracket to the nearest `[` below it (so the `![` in between are not visited again) and
  stopping at the first inactive one; a shortcut label is looked up only when no bracket was pushed inside it, and a
  `[label]` after `]` is scanned for at most 999 characters (cmark's devices). The references are a hash table
  ([Links and references](/reference/links#reference-definitions)).
- **Merging.** After a block, adjacent text nodes (runs and brackets that matched nothing) are merged, one pass over
  the block's subtree, each run's literal built once.
- **Linear time.** A code span's closer is searched for once: where the last backtick string of each length was seen
  is remembered (a table growing with the longest string met, so a string of any length can open a code span; cmark
  stops at 1000), so unmatched backticks do not rescan the subject; a failed search for the end of a comment, a
  processing instruction, a declaration or a CDATA section is remembered too (cmark's devices). An emphasis or a link
  node is linked into the block without the document's cycle check, which walks the ancestors (the node is new, so no
  cycle is possible): O(1) per node at any block depth. The pathological inputs and their timings are in
  [Conformance](/reference/conformance#pathological-inputs).

## Errors

The inline phase never fails on the text. `parse` returns a nonzero `stat` only when memory runs out, an extension's
hook fails (a block hook before it, an inline hook or `post_process`), or a hook breaks the
[contract](/reference/extensions#the-contract) (`BARUCH_ERR_CONTRACT`, the message naming the extension, the hook and
the source byte), or two enabled extensions declare one delimiter character (`BARUCH_ERR_ARGUMENT`).
