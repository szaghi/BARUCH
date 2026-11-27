# 1. A document by hand

Before BARUCH reads any Markdown, a document is something you can build yourself: the parser of the later chapters
builds exactly this, node by node. This chapter makes the tree of

```markdown
# BARUCH

A *flat* tree.

- one
- two
```

reads it back, misuses it on purpose, then edits and compacts it.

## Nodes are integers

<<< @/examples/snippets/t01_document-build.f90

- `new_node(kind)` returns a **handle**, an integer: the node's index in the document's arrays. The node is detached
  until a linking method puts it in the tree; `root()` is the document node, 1.
- The kinds are constants, one per CommonMark node type: `NODE_HEADING`, `NODE_PARAGRAPH`, `NODE_EMPH`, `NODE_TEXT`...
- What a node holds depends on its kind. The typed accessors check it: `set_heading_level` only on a heading,
  `set_list_type` and `set_list_tight` only on a list, `set_literal` only on text-like nodes.
- The integer slots hold the source style the parser records, here the `#` form of the heading, the `*` of the
  emphasis and the `-` of the list: `SLOT_HEADING_FORM`, `SLOT_EMPHASIS_CHAR`, `SLOT_LIST_MARKER` name them. A slot
  left at 0 means "no style recorded".

`add_text` is a helper of the program: a text node, its literal, appended to a parent.

## Reading it back

The tree is printed in document order by following the links: first child, else next sibling, else up to the nearest
ancestor with a next sibling. No recursion and no stack, so the depth of the tree does not matter.

<<< @/examples/snippets/t01_document-walk.f90

## Misuse is a status, not a stop

<<< @/examples/snippets/t01_document-misuse.f90

A heading level asked of a paragraph returns 0 and, because the program passed `stat` and `errmsg`, a code
(`BARUCH_ERR_KIND`) and a sentence. Without them the call returns the same 0 and nothing else happens: BARUCH never
stops a program.

## Editing and compacting

<<< @/examples/snippets/t01_document-edit.f90

- `prepend_child` puts the new item first. The linking methods also move a node that is already in the tree.
- `unlink` takes the first item out with its paragraph and text. Their handles are not reused: they become
  **tombstones**, so every handle the program holds keeps meaning the same node.
- `compact` is the one operation that renumbers: tombstones and dead strings go, the live nodes keep their order, and
  `map(old)` gives each old handle its new number.

## Running it

<<< @/examples/output/t01_document.ansi{ansi}

The whole program:

<<< @/examples/snippets/t01_document.f90

::: tip What you learned
A node is an integer; `new_node`, the linking methods, the typed accessors and the slots build a tree; the links walk
it without recursion; a misuse returns a status; `unlink` leaves tombstones and `compact` renumbers, with a map.
Reference: [The document](/reference/document).
:::
