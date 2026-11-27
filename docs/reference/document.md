# The document

Module `baruch_document`, type `markdown_document`; the kinds and the slot layout in `baruch_kinds_nodes`, the status
codes in `baruch_status`. A client reaches all of them through `use baruch`.

A document is a set of flat arrays: a node is an integer index into them. No node is an object, nothing is a pointer,
nothing recurses. Each node has

- a **kind** (`NODE_*`);
- five **links**: parent, first child, last child, previous sibling, next sibling (0 is "none");
- a **span**: its first and last byte in the document's source (the sanitised buffer, from step 1.2 of the plan);
- `NODE_INT_SLOTS` (6) **integer slots** and `NODE_STRING_SLOTS` (3) **string slots**, whose meaning depends on the
  kind ([the slot table](#the-slot-table));
- a list of **named attributes**, name and value strings, for what the slots do not hold (extensions: a heading's
  `{#id}`, a `{.class}`).

The strings live in one pool owned by the document. Node 1 is the root, of kind `NODE_DOCUMENT`.

Handles, slot values and spans are `integer(I4P)`, PENF's 32-bit integer: the default `integer` of gfortran,
nvfortran and ifx. A document holds at most 2<sup>31</sup>-1 nodes and as many bytes of strings.

## Life cycle

| Method | |
|---|---|
| `initialize(capacity, stat, errmsg)` | reset to a document holding only its root; `capacity` nodes allocated up front (default 64) |
| `destroy()` | free everything: an empty document, with no root |
| `root()` | the root, 1; 0 for an empty document |
| `nodes_number()` | live nodes, the root included |
| `last_node()` | the highest handle in use, live or [tombstone](#editing): handles run from 1 to it |
| `is_node(n)` | true if `n` is a live node |
| `pool_bytes()` | bytes the string pool holds, dead strings included until `compact` |

An empty document needs no `initialize`: the first `new_node` or `register_kind` creates the root. Arrays grow by
doubling, so `n` nodes cost amortised O(1) each. Assignment is a deep copy (every component is allocatable): `copy =
doc` gives an independent document whose handles are the same numbers.

## Kinds

One constant per CommonMark node type, below `NODE_EXTENSION_FIRST` (1000):

| Blocks | | Inlines | |
|---|---|---|---|
| `NODE_DOCUMENT` | 1, the root | `NODE_TEXT` | 11 |
| `NODE_BLOCK_QUOTE` | 2 | `NODE_SOFTBREAK` | 12 |
| `NODE_LIST` | 3 | `NODE_LINEBREAK` | 13 |
| `NODE_ITEM` | 4 | `NODE_CODE` | 14, code span |
| `NODE_CODE_BLOCK` | 5 | `NODE_HTML_INLINE` | 15 |
| `NODE_HTML_BLOCK` | 6 | `NODE_EMPH` | 16 |
| `NODE_PARAGRAPH` | 7 | `NODE_STRONG` | 17 |
| `NODE_HEADING` | 8 | `NODE_LINK` | 18, autolinks too |
| `NODE_THEMATIC_BREAK` | 9 | `NODE_IMAGE` | 19 |
| `NODE_LINK_REFERENCE_DEFINITION` | 10, kept in place | | |

`NODE_BLOCK_FIRST`, `NODE_BLOCK_LAST`, `NODE_INLINE_FIRST`, `NODE_INLINE_LAST` bound the two ranges. `NODE_NONE` (0)
is the kind of a tombstone.

An extension's kinds are numbers from `NODE_EXTENSION_FIRST` up, registered in the document with a name and a role
(block or inline), so that a document describes itself to whatever walks, renders or compares it:

| Method | |
|---|---|
| `register_kind(kind, name, inline, stat, errmsg)` | register an extension kind; the number and the name are unique |
| `is_known_kind(kind)` | true for a core kind or a registered one |
| `kind_is_inline(kind)` | true for an inline kind, core or registered |
| `kind_name(kind)` | `document`, `block_quote`, `code_block`... (cmark's names) or the registered name; empty if unknown |

## Nodes and links

| Method | |
|---|---|
| `new_node(kind, stat, errmsg)` | a new detached node of a core kind (not `NODE_DOCUMENT`) or a registered one; 0 on error |
| `node_kind(n)` | its kind |
| `parent(n)`, `first_child(n)`, `last_child(n)` | the links, 0 for none |
| `next_sibling(n)`, `prev_sibling(n)` | |
| `children_number(n)` | counted along the sibling links |

The document's order is the depth-first order of the links: a node, then its children from the first to the last.
Walking it needs no stack: from a node, go to its first child; with none, to its next sibling; with none, up to the
nearest ancestor that has a next sibling ([the tutorial](/manual/tutorial/01-document) prints a tree this way).

## Editing

| Method | |
|---|---|
| `append_child(parent, child, stat, errmsg)` | `child` becomes the last child of `parent` |
| `prepend_child(parent, child, stat, errmsg)` | `child` becomes the first child |
| `insert_before(sibling, node, stat, errmsg)` | `node` becomes the previous sibling of `sibling` (which must have a parent) |
| `insert_after(sibling, node, stat, errmsg)` | `node` becomes the next sibling |
| `unlink(n, stat, errmsg)` | `n` and its whole subtree become tombstones |
| `compact(map, stat, errmsg)` | drop the tombstones and the dead strings; `map(old)` is the new handle, 0 for a tombstone |

- A node already linked elsewhere is **moved** by the four linking methods, with its subtree: no copy, no tombstone.
- The links stay a tree: linking the root, a node under itself or under one of its descendants is refused
  (`BARUCH_ERR_LINK`). The ancestors are walked only when the node being linked has children: building a document
  top-down, leaf by leaf, costs O(1) per link at any depth.
- **Editing never renumbers.** An unlinked node keeps its number as a tombstone: `is_node` is false, every accessor
  refuses it, its strings stay in the pool. Its subtree is visited by following the links, deepest first, with no
  recursion and no stack.
- **`compact` is the one operation that renumbers.** The live nodes keep their order: a node's new handle is the
  number of live nodes up to it, so the root stays 1. Detached live nodes (created and never linked) are kept.
  Every handle the caller holds must go through `map`.

## Spans

| Method | |
|---|---|
| `set_span(n, first, last, stat, errmsg)` | bytes `first` to `last` of the source; `last = first - 1` is an empty span |
| `span_first(n)`, `span_last(n)` | 0 and -1 for a node with no span (created by hand) |

A span is the slice `source(first:last)` of the document's sanitised buffer, which the document will own (plan step
1.2); a node with no span gives an empty slice.

## Slots

| Method | |
|---|---|
| `int_slot(n, slot)`, `set_int_slot(n, slot, value)` | integer slot 1 to `NODE_INT_SLOTS`, any kind |
| `string_slot(n, slot)`, `set_string_slot(n, slot, value)` | string slot 1 to `NODE_STRING_SLOTS`, any kind |

Slots are zero (or empty) until set. A new string goes into the pool; the value it replaces stays there, dead, until
`compact`. The generic slots check only the slot number: they are how a parser records style and how an extension
kind stores its own integers.

### The slot table

The integer slots hold the meaning first and the source style after: equality (plan step 3.1) compares the semantic
ones, the writer (step 3.2) reuses the style ones. A style slot left at 0 is "not recorded": the writer is canonical
there. Characters are stored as their ASCII code, `iachar`.

| Kind | Integer slots | String slots |
|---|---|---|
| heading | 1 `SLOT_HEADING_LEVEL`: 1 to 6<br>2 `SLOT_HEADING_FORM` (style): `HEADING_ATX`, `HEADING_SETEXT`<br>3 `SLOT_HEADING_CLOSING` (style): closing `#`s of an ATX heading | |
| list | 1 `SLOT_LIST_TYPE`: `LIST_BULLET`, `LIST_ORDERED`<br>2 `SLOT_LIST_START`: start number<br>3 `SLOT_LIST_TIGHT`: 1 tight, 0 loose<br>4 `SLOT_LIST_MARKER` (style): bullet `-` `+` `*`, or delimiter `.` `)` | |
| code_block | 1 `SLOT_CODE_FORM` (style): `CODE_INDENTED`, `CODE_FENCED`<br>2 `SLOT_CODE_FENCE_CHAR` (style): `` ` `` or `~`<br>3 `SLOT_CODE_FENCE_LENGTH` (style): 3 or more | 1 `STRING_LITERAL`: the content<br>2 `STRING_INFO`: the info string |
| thematic_break | 1 `SLOT_BREAK_CHAR` (style): `-`, `*` or `_` | |
| emph, strong | 1 `SLOT_EMPHASIS_CHAR` (style): `*` or `_` | |
| link, image | 1 `SLOT_LINK_FORM` (style): `LINK_INLINE`, `LINK_FULL`, `LINK_COLLAPSED`, `LINK_SHORTCUT`, `LINK_AUTOLINK` | 1 `STRING_DESTINATION`<br>2 `STRING_TITLE`<br>3 `STRING_LABEL` |
| link_reference_definition | | 1 `STRING_DESTINATION`<br>2 `STRING_TITLE`<br>3 `STRING_LABEL` |
| text, code, html_inline, html_block | | 1 `STRING_LITERAL` |
| extension kinds | the extension's own | 1 `STRING_LITERAL` by convention, the rest the extension's own |
| document, block_quote, item, paragraph, softbreak, linebreak | | |

Slots no constant names are free: the block parser (plan step 1.5) and the inline parser (phase 2) add rows to this
table as they record more.

## Typed accessors

They check the kind: on a node of another kind they return the default (0, false, empty) with `BARUCH_ERR_KIND`.

| Method | Kinds | |
|---|---|---|
| `heading_level(n)`, `set_heading_level(n, level)` | heading | 1 to 6 |
| `list_type(n)`, `set_list_type(n, ltype)` | list | `LIST_BULLET` or `LIST_ORDERED` |
| `list_start(n)`, `set_list_start(n, start)` | list | 0 or more |
| `list_tight(n)`, `set_list_tight(n, tight)` | list | logical |
| `literal(n)`, `set_literal(n, value)` | text, code, html_inline, html_block, code_block, extension kinds | the content |
| `info(n)`, `set_info(n, value)` | code_block | the info string |
| `destination(n)`, `set_destination(n, value)` | link, image, link_reference_definition | |
| `title(n)`, `set_title(n, value)` | link, image, link_reference_definition | |
| `label(n)`, `set_label(n, value)` | link, image, link_reference_definition | the reference label as written |

## Attributes

| Method | |
|---|---|
| `set_attribute(n, name, value, stat, errmsg)` | set (or replace) an attribute; the name is not empty, the value may be |
| `attribute(n, name, found, stat, errmsg)` | its value, empty if absent (`found` tells) |
| `has_attribute(n, name)` | |
| `attributes_number(n)` | |
| `attribute_name(n, i)`, `attribute_value(n, i)` | the i-th, in the order they were first set |
| `remove_attribute(n, name)` | nothing happens if absent |

Names are compared byte by byte (`"id"` is not `"id "`). Any kind can carry attributes.

## Errors

Every method that can fail has optional `stat` and `errmsg` ([Conventions](/guide/conventions)): `BARUCH_OK` (0) and an
empty message on success; otherwise a code and a sentence naming the method and the values, for example
`markdown_document%heading_level: node 2 is a paragraph, not a heading`. Without `stat` the method returns its
default result and nothing stops.

| Code | |
|---|---|
| `BARUCH_OK` | success |
| `BARUCH_ERR_ARGUMENT` | a value out of range: a heading level of 7, a slot of 0, an unregistered kind, an empty name |
| `BARUCH_ERR_NODE` | a handle that is not a live node: 0, beyond `last_node()`, a tombstone |
| `BARUCH_ERR_KIND` | a typed accessor on a node of another kind |
| `BARUCH_ERR_LINK` | an edit that would break the tree: a cycle, the root moved or unlinked, a sibling with no parent |
| `BARUCH_ERR_ALLOCATION` | memory could not be allocated |
