# Inlines

What `markdown_options%parse` builds inside each paragraph and heading: the inline content of CommonMark 0.31.2 (spec
section 6), one node per construct, children of the block, each with its span in the source. Modules
`baruch_inlines` (the parser) and `baruch_inlines_scan` (the scanners), both internal: a client calls `parse` and
reads the tree through [the document](/reference/document)'s accessors. A worked example:
[tutorial chapter 5](/manual/tutorial/05-inlines).

::: info Status (plan step 2.3)
Written: backslash escapes, entity and numeric character references, code spans, autolinks, raw inline HTML, hard and
soft line breaks, textual content. The sections Code spans, Autolinks, Raw HTML, Soft line breaks and Textual content
of the spec pass 100%; Backslash escapes, Entity and numeric character references and Hard line breaks pass every
example but the eight that need emphasis or links ([Conformance](/reference/conformance#counts)).

**Plan step 2.4** completes the parser: emphasis and strong emphasis (`NODE_EMPH`, `NODE_STRONG`), links and images
(`NODE_LINK`, `NODE_IMAGE`, inline and reference forms) and the resolution of references against the link reference
definitions. Until then `*`, `_`, `[`, `![` and `]` are text.
:::

## When it runs

`parse` runs the block phase ([Blocks](/reference/blocks)), then the inline phase over every paragraph and heading:
the block parser leaves in each one text node holding its content (its lines without their leading white space,
joined by LF, the trailing white space of the last removed), and the inline parser replaces it with the inline nodes.
All the link reference definitions of the document are known by then, as references need (step 2.4). The tree is
compacted at the end, so a parsed document has no tombstone; the handles follow the order the nodes were created in
(blocks first, then each block's inline nodes), not the document order: walk the links.

An empty heading (`##`) has no child. Code blocks and HTML blocks have no inline content: their `literal` is the
block parser's.

## Kinds and their fields

| Kind | Written as | Fields | Span |
|---|---|---|---|
| `NODE_TEXT` | any text: plain characters, backslash escapes, character references, characters that open nothing | `literal`: the text as it reads (escapes and references decoded) | the source bytes, escapes and references as written |
| `NODE_CODE` | `` `code` ``: a backtick string, the content, a backtick string of the same length | `literal`: the content (line endings as spaces, one space stripped at both ends, see below) | the backtick strings included |
| `NODE_HTML_INLINE` | an open or closing tag, a comment, a processing instruction, a declaration, a CDATA section | `literal`: the bytes as written (line endings LF) | the HTML |
| `NODE_LINK` (autolink) | `<scheme:...>`, `<address@domain>` | `destination`: the URL, or `mailto:` and the address, references decoded; `title` empty; `SLOT_LINK_FORM` = `LINK_AUTOLINK`; one `NODE_TEXT` child, the URL or the address | `<` to `>` (its text: inside them) |
| `NODE_LINEBREAK` | two or more spaces, or a backslash, then a line ending | none | the spaces (or the backslash) and the line ending |
| `NODE_SOFTBREAK` | a line ending | none | the line ending's first byte |

`literal`, `destination` and `title` are the accessors of [the document](/reference/document#typed-accessors); the
renderer writes these kinds as [HTML](/reference/html#what-is-written) describes.

### Text

Text is **merged**: one node per run of text, whatever it is made of. `a\*b &amp; c` is one text node, `a*b & c`. A
run ends at another inline node (a code span, raw HTML, an autolink, a line break) or at the end of the block. Text
holds what the source means, its span what was written.

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
else is processed: `` `\*` `` holds `\*`.

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
definition is kept as written: matching a reference normalises it (step 2.4).

## How it is built

The parser follows the spec's appendix "A parsing strategy" and cmark's `inlines.c` (BSD-2), whose structure it keeps;
nothing recurses and nothing is module state: one `inline_parser` per parse holds the arrays, reused block after
block.

- **The subject and the cursor.** Each block's content is the subject; one cursor reads it left to right, once. Plain
  text is found with `scan` for the next special byte (LF, `` ` ``, `\`, `&`, `<`, `*`, `_`, `[`, `]`, `!`); a node's
  literal is a slice or the few bytes a construct stands for. Text is accumulated and written as one node when
  something else comes.
- **The delimiter stack** holds the runs of `*` and `_` (each a text node of its own): character, count, original
  count, can open, can close, linked both ways through indices so that a run can be removed from its middle. **The
  bracket stack** holds the `[` and `![` (each a text node too): position, image or not, active, a bracket after it,
  the delimiter below it. Step 2.3 builds and maintains both; `process_emphasis` only clears the delimiters and a `]`
  only pops its bracket. Step 2.4 adds the flanking rules, the matching and the link resolution.
- **Merging.** After a block, adjacent text nodes (runs and brackets that matched nothing) are merged, one pass over
  the block's subtree, each run's literal built once.
- **Linear time.** A code span's closer is searched for once: where the last backtick string of each length (up to
  80) was seen is remembered, so unmatched backticks do not rescan the subject; a failed search for the end of a
  comment, a processing instruction, a declaration or a CDATA section is remembered too (cmark's devices). The
  pathological inputs are measured in plan step 2.6.

## Errors

The inline phase never fails on the text. `parse` returns a nonzero `stat` only when memory runs out or an
extension's block hook failed before it ([Blocks](/reference/blocks#errors)).
