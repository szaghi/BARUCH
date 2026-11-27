# Walk

The reader interface beside the accessors: a document's tree as a sequence of events, md4c's callbacks in a loop.
Method `walk` of `markdown_options`, constants `EVENT_*`; module `baruch_walk` (internal). A worked example:
[tutorial chapter 4](/manual/tutorial/04-render#walk-the-events).

## The loop

```fortran
integer(I4P) :: node, event, first, last

node = 0                                   ! start
do
   call options%walk(doc, node, event, first=first, last=last)
   if (node == 0) exit                     ! the walk is over
   select case(event)
   case(EVENT_ENTER_BLOCK) ! ...
   case(EVENT_LEAVE_BLOCK) ! ...
   case(EVENT_ENTER_SPAN)  ! ...
   case(EVENT_LEAVE_SPAN)  ! ...
   case(EVENT_TEXT)        ! ... doc%literal(node)
   endselect
enddo
```

| Method | |
|---|---|
| `walk(doc, node, event, top, first, last, stat, errmsg)` | advance the walk by one event |

| Argument | | |
|---|---|---|
| `node`, `event` | `inout` | the walk's state: the current event on entry (`node = 0` to start), the next one on exit; `node = 0` and `event = EVENT_NONE` once the walk is over |
| `top` | `in`, optional | walk the subtree of this node only (default: the root, the whole document); pass the same `top` at every call |
| `first`, `last` | `out`, optional | the node's byte span in the [source](/reference/input) (`span_first`, `span_last`; 0 when the walk is over) |
| `stat`, `errmsg` | `out`, optional | [the status](#errors) |

The node's kind is `doc%node_kind(node)`, its content the document's accessors (`literal`, `heading_level`,
`destination`...): an event carries a handle, not a copy.

### Why an iterator

The walk is an **iterator whose state the caller holds**, two integers, instead of a callback. Fortran calls back
through procedure arguments or procedure pointers, and a callback needs its context (counters, an output buffer) in
module variables or in a type extended for the purpose; the loop keeps it in the caller's local variables, with a
`select case` on the event, and nothing is stored in the library. Each call reads the tree's links around the current
node (first child, next sibling, parent) and returns: no stack, no recursion, constant memory, so a 10^5-deep nesting
walks like a flat list. Any number of walks can run at once, over one document or several, from several threads.

## Events

| Constant | Value | |
|---|---|---|
| `EVENT_NONE` | 0 | no event: before the first call, after the last |
| `EVENT_ENTER_BLOCK` | 1 | entering a block: its children's events follow |
| `EVENT_LEAVE_BLOCK` | 2 | leaving it, after its children |
| `EVENT_ENTER_SPAN` | 3 | entering a span (an inline container) |
| `EVENT_LEAVE_SPAN` | 4 | leaving it |
| `EVENT_TEXT` | 5 | an inline leaf: one event, no leave |

Every node of the walked subtree gives exactly one entering event (`EVENT_ENTER_BLOCK`, `EVENT_ENTER_SPAN` or
`EVENT_TEXT`), in document order, and each enter is matched by the leave of the same node once its children are done.
Which events a node gives is the class of its kind:

| Class | Kinds | Events |
|---|---|---|
| block | every core block kind (`NODE_DOCUMENT` to `NODE_LINK_REFERENCE_DEFINITION`), an extension kind registered as a block | enter, children, leave |
| span | `NODE_EMPH`, `NODE_STRONG`, `NODE_LINK`, `NODE_IMAGE`, an extension kind registered as inline | enter, children, leave |
| text | `NODE_TEXT`, `NODE_SOFTBREAK`, `NODE_LINEBREAK`, `NODE_CODE`, `NODE_HTML_INLINE` | one `EVENT_TEXT`, children not visited |

A leaf block (a code block, a thematic break) is entered and left with nothing between: its content is its literal.
Unlike md4c, a code span is a text (its content is one literal) and a code block reports no text events.

## Extensions

An extension can change the class of its own kinds (numbers from `NODE_EXTENSION_FIRST`) with its `walk_event` hook:

```fortran
subroutine walk_event(self, doc, node, entering, event, handled)
```

Asked with `entering = .true.` it answers `EVENT_ENTER_BLOCK`, `EVENT_ENTER_SPAN` or `EVENT_TEXT` (an inline math
node is a text); asked with `entering = .false.`, when the walk leaves a node it entered, the matching `EVENT_LEAVE_*`.
The enabled extensions are asked in registration order, the first answering `handled = .true.` wins; with none, the
class of the kind as it was registered (block or inline) applies. Any other answer breaks the contract:
`BARUCH_ERR_CONTRACT`, the extension named. Core kinds are never offered to the hook.

## Errors

A failing call ends the walk: `node = 0`, `event = EVENT_NONE`.

| `stat` | When |
|---|---|
| `BARUCH_OK` | the next event, or the end; an uninitialised document has no event |
| `BARUCH_ERR_NODE` | `top` is not a live node |
| `BARUCH_ERR_ARGUMENT` | the current `node` is not a live node, or `event` is not an event |
| `BARUCH_ERR_CONTRACT` | an extension's `walk_event` answer is not an event of the right direction |

Editing the tree during a walk is the caller's risk: the walk reads the links of the current node at each call, so a
node unlinked while it is current ends the walk with `BARUCH_ERR_ARGUMENT`.
