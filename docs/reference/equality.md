# Equality

Two documents compared by their **meaning**: the same kinds in the same structure, the same processed text, the same
semantic fields; how the source was written (spans, recorded style, line endings) is ignored. Method `equal` of
`markdown_document`; submodule `baruch_equal` (internal). It is the test of the round trip (settled design 2 of the
plan): `parse(write(parse(x)))` equals `parse(x)`.

```fortran
character(:), allocatable :: path, difference

if (.not. a%equal(b, path=path, difference=difference)) print '(A)', difference
```

| Method | |
|---|---|
| `equal(other, path, difference)` | true if the document and `other` mean the same |

| Argument | | |
|---|---|---|
| `other` | `in` | the other document |
| `path` | `out`, optional | the [path](#the-path) of the first differing node; empty if equal |
| `difference` | `out`, optional | the path, `: `, then what differs there (`kind emph vs strong`, `literal "a" vs "b"`, `only in the second document`); empty if equal |

A difference is a result, not an error: `equal` has no `stat`, it never fails, and two empty documents (never
initialised) are equal. It is a method of the document, not of [the options](/reference/extensions#the-options):
it needs no hook, only what the two documents hold, their extension kinds' names included.

## An example

<<< @/examples/snippets/equal-pairs.f90

<<< @/examples/snippets/equal-compare.f90

<<< @/examples/output/equal.ansi{ansi}

The first two pairs differ only in style. The third differs in a node's kind, found two levels down: the second list
of the document (`list[2]`), its item, the item's paragraph. In the fourth the links are equal (an inline link and a
shortcut reference resolving to the same destination), but [a link reference definition](/reference/links) is a node
kept in place: the second document has one more.

## What is compared

The two trees are walked in lockstep, from the roots, in document order; the first node that differs stops the
walk. Of each pair of nodes, equality compares:

- the **kind**: a core kind by its number, an extension kind by its [registered name](/reference/document#kinds) (two
  documents may give the same extension's kind different numbers);
- the **children**, in order: the same number of them, each pair compared in turn;
- the **semantic fields** of the kind ([the table](#semantic-fields-per-kind));
- the **attributes**: the same names with the same values, in any order.

### Semantic fields per kind

| Kind | Compared | Ignored (style) |
|---|---|---|
| document, block_quote, item, paragraph, thematic_break, softbreak, linebreak, emph, strong | the kind and the children only | `SLOT_BREAK_CHAR`, `SLOT_EMPHASIS_CHAR` |
| heading | `SLOT_HEADING_LEVEL` | `SLOT_HEADING_FORM` (ATX, setext), `SLOT_HEADING_CLOSING` |
| list | `SLOT_LIST_TYPE`; `SLOT_LIST_TIGHT` (as tight or loose); `SLOT_LIST_START` of an ordered list | `SLOT_LIST_MARKER` (bullet, delimiter); the start of a bullet list |
| code_block | the literal, the info string | `SLOT_CODE_FORM` (indented, fenced), `SLOT_CODE_FENCE_CHAR`, `SLOT_CODE_FENCE_LENGTH` |
| html_block, code, html_inline | the literal | |
| text | the literal of the [text run](#text-runs) | |
| link, image | the destination, the title (and the children: the link text, the image description) | `SLOT_LINK_FORM` (inline, full, collapsed, shortcut, autolink), the label |
| link_reference_definition | the label **normalised** as [matching](/reference/links#matching-a-label) normalises it, the destination, the title | the label as written |
| extension kinds | every integer slot, every string slot | |
| every kind | the attributes | the span; the integer slots no constant names (free) |

Strings are compared byte for byte, CRLF, CR and LF counting as one line ending. Destinations and titles are compared
processed (escapes and character references resolved: `a\*b` and `a&#42;b` are one text `a*b`), never as written.

- **A link's form and label are style**: `[a](/u)`, `[a][x]` and `[a]` with `[x]: /u` are links to `/u` with the text
  `a`; what they point to is compared, not how. An autolink `<https://a.b>` equals the inline link
  `[https://a.b](https://a.b)`.
- **A definition is meaning**: it is a node in the tree, in place, so a document with one more definition differs,
  even when no link uses it. Its label is compared normalised: `[Foo]: /u` and `[foo]: /u` define the same reference.
- **An indented and a fenced code block are equal** when their literal and info string are (an indented block has no
  info string).
- **An extension kind's slots are all meaning**: the document registers an extension kind's number, name and role,
  not which of its slots hold style, so two nodes of an extension kind are equal only when every slot is. Recording
  which slots hold style in the kind's registration is left to the first extension that needs it.

### Text runs

Adjacent text nodes are one text, their literals concatenated, and a run of text nodes whose literals are all empty is
no node at all. How a tree is cut into text nodes is not its meaning: a parse merges adjacent texts, a tree built by
hand or edited need not. A run's attributes are those of its first node. Only `text` nodes join: two code spans side
by side stay two nodes.

## What is ignored

| | |
|---|---|
| spans | the byte ranges into the source: two documents read from different texts compare equal |
| style slots | [the table](#semantic-fields-per-kind), right column; a style slot is what the writer reuses, not what the document means |
| free slots of a core kind | an integer or string slot that no `SLOT_*`/`STRING_*` constant names for the kind |
| line endings | in every string compared: CRLF, CR and LF are one |
| tombstones | an [unlinked](/reference/document#editing) node is not in the tree; neither is a node created and never linked |
| the source | the text a document was loaded from, its lines, its encoding |
| node numbers | the trees are compared by their links: a document and its [compacted](/reference/document#editing) copy are equal |

## The path

The path names the first differing node from the root, one step per node: its [kind name](/reference/document#kinds)
and, when its parent has more than one child of that kind, its position among them, from 1:
`/document/list[2]/item[1]/paragraph/emph` is the emphasis in the paragraph of the first item of the document's second
list. A [text run](#text-runs) counts as one text. The path is in the first document (the one `equal` is called on);
for a node only the second document has, in the second.

| `difference` ends with | |
|---|---|
| `kind X vs Y` | the two nodes have different kinds |
| `literal "..." vs "..."`, `info string`, `destination`, `title`, `label` | a string field differs (each value quoted up to 40 bytes, line endings as `\n` and `\r`) |
| `heading level 1 vs 2`, `list type bullet vs ordered`, `list start 1 vs 2`, `list tight vs loose` | a semantic integer differs |
| `integer slot K I vs J`, `string slot K "..." vs "..."` | a slot of an extension kind differs |
| `attribute "NAME" "..." vs "..."`, `attribute "NAME" only in the first document` | an attribute differs or is missing |
| `only in the first document`, `only in the second document` | one document has a node where the other has none (more children, more siblings) |

## No recursion

The walk follows the links (first child, next sibling, parent) of both trees at once: no recursion, no stack, constant
memory beside the path. A 10^5-deep nesting of block quotes or lists compares like a flat list; its path, built only on
a difference, holds every step (`baruch_test_equal` checks both).
