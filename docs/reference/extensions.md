# Extensions

Type `markdown_extension` (abstract), the hooks every extension overrides or inherits; type `markdown_options`, which
holds the extensions and parses with them. Modules `baruch_extension`, `baruch_options` and `baruch_blocks` (the
block parser, internal); a client reaches them through `use baruch`.

Every syntax beyond CommonMark goes through this interface, BARUCH's own extensions (GFM, VitePress: plan phases 5
and 6) as much as a client's. The core is CommonMark only; extensions are compiled in and enabled per parse. A worked
example: [tutorial chapter 2](/manual/tutorial/02-extension).

::: info Status (plan step 1.6)
Every hook is declared with its default. The **block** hooks are live: the CommonMark block parser
([Blocks](/reference/blocks)) calls them on every line, an extension's starts before the core's. The **HTML** and
**walk** hooks are live too: the renderer ([HTML](/reference/html#extensions)) and the walk
([Walk](/reference/walk#extensions)) call them. The inline hooks and `post_process` are called from step 2.5,
`write_markdown` from step 3.2.
:::

## The options

`markdown_options` holds the registered extensions, each a copy (polymorphic) with an enabled flag, in registration
order. There is no module-level registry: the options object is the caller's, and a parse uses the one it is given.
Assignment copies it (the extensions included).

| Method | |
|---|---|
| `register(extension, enabled, stat, errmsg)` | keep a copy of an extension, after the ones registered; enabled unless `enabled = .false.`; its name must be non-empty and not taken |
| `enable(name, stat, errmsg)`, `disable(name, stat, errmsg)` | switch a registered extension on or off; an unknown name is `BARUCH_ERR_ARGUMENT` |
| `is_registered(name)`, `is_enabled(name)` | |
| `extensions_number()`, `extension_name(i, stat, errmsg)` | the registered extensions, enabled or not |
| `register_kinds(doc, stat, errmsg)` | the enabled extensions register their kinds in a document (`parse` does it; a document built by hand with extension nodes needs it first) |
| `parse(doc, stat, errmsg)` | the document's source (`load`, `load_file`) into its tree, with the enabled extensions |
| `render_html(doc, html, ...)`, `render_html_file(doc, path, ...)` | the tree as HTML, the enabled extensions' nodes by their hooks ([HTML](/reference/html)) |
| `walk(doc, node, event, ...)` | the tree as events, one per call ([Walk](/reference/walk)) |

`parse` resets the tree to its root (the source and its lines are kept, the registered kinds are dropped), lets the
enabled extensions register their kinds, then runs the block parser. It never fails on the text: every byte sequence is
a document. A nonzero `stat` comes from an extension (a hook reporting a failure, or breaking the
[contract](#the-contract)) or from memory; the tree then holds what was parsed up to that point.

## The hooks

An extension extends `markdown_extension` and overrides the bindings it needs. All the hooks receive the extension as
`intent(in)`: a parse keeps its state in the document, never in the extension, so one options object can serve several
parses at the same time (different threads, different documents). Integer arguments are `integer(I4P)` (PENF); a
hook that can fail has `stat` (`intent(inout)`, 0 on entry: leave it so unless the hook fails) and `errmsg`.

| Hook | Default | Called | |
|---|---|---|---|
| `name()` | none: deferred | `register`, `enable`, `disable`, messages | the extension's name, unique in an options object |
| `register_kinds(doc, stat, errmsg)` | no kinds | before every parse | `doc%register_kind(...)` for each of its kinds |
| `block_start(doc, line, line_number, offset, column, indent, container, paragraph, paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)` | not mine (`node = 0`) | **live**: at each position of each line where a block can start | [opening a block](#opening-a-block) |
| `block_continue(doc, node, line, line_number, offset, column, indent, match, consumed, stat, errmsg)` | `BLOCK_UNMATCHED` | **live**: on each line, for each open block of the extension | [continuing a block](#continuing-a-block) |
| `block_close(doc, node, lines_first, lines_last, stat, errmsg)` | nothing | **live**: when one of its blocks closes | its content lines, as byte ranges of the source |
| `block_content(kind)` | `CONTENT_NONE` | **live**: when one of its blocks opens | what a block of the kind holds |
| `can_contain(kind, child_kind)` | any block | **live**: when a block would open inside one of its containers | false: the container closes, the child opens in its parent |
| `contains_inlines(kind)` | false | inline parser (phase 2) | the block's content is inline content (emphasis, links...) |
| `inline_triggers()` | none (`''`) | step 2.5 | the characters that can open one of its inline constructs |
| `inline_start(doc, text, offset, node, consumed, stat, errmsg)` | not mine | step 2.5 | an inline construct at a trigger character |
| `delimiter_chars()` | none | step 2.5 | characters whose runs take part in the delimiter algorithm (`~` for strikethrough) |
| `delimiter_match(doc, delimiter, opener_length, closer_length, used, node, stat, errmsg)` | no match | step 2.5 | characters used from each run, the node wrapping the content |
| `post_process(doc, stat, errmsg)` | nothing | step 2.5: after the whole parse | edit the parsed tree (task list checkboxes, footnote numbers) |
| `write_html(doc, node, entering, html, handled, stat, errmsg)` | not mine | **live**: by `render_html`, for every event of every node, core kinds too | a node as HTML, on entering and on leaving it ([HTML](/reference/html#extensions)) |
| `write_markdown(doc, node, entering, markdown, handled, stat, errmsg)` | none: deferred | step 3.2 | a node as Markdown: mandatory, whatever an extension reads it writes back |
| `walk_event(doc, node, entering, event, handled)` | not mine | **live**: by `walk` (and the renderer), for the extension kinds | the walk event a node is reported as ([Walk](/reference/walk#extensions)) |

The inline hooks' arguments are declared now and settled in step 2.5, when the inline parser calls them (the plan
amends the interface there if a GFM extension needs it, before any client depends on it).

### Opening a block

The parser offers each position of a line to the enabled extensions' `block_start`, in registration order, before
the core's block starts (so an extension can take syntax from the core, as front matter takes `---`). The hook sees:

| Argument | |
|---|---|
| `line` | the whole line, its ending excluded |
| `line_number` | its number in the source (`doc%line(line_number)`) |
| `offset` | the first byte of `line` not consumed yet: the open containers' markers before it are |
| `column` | the column of `line(offset:)`, 0-based, tab stops of 4 (CommonMark 2.2) |
| `indent` | columns of white space from `offset` (4 or more: an indented code block, for the core) |
| `container` | the block the new block would open in |
| `paragraph` | a paragraph open there, 0 if none |
| `paragraph_first(:)`, `paragraph_last(:)` | the bytes of the paragraph's lines (their content after the containers' markers) in `doc%source()`; empty without a paragraph |

To claim the position the hook sets `node` to a new detached block of one of its kinds (`doc%new_node`) and
`consumed` to the bytes of `line(offset:)` its marker takes. A block that turns the paragraph's last lines into
something else (a table header, a setext-like underline) reads them and sets `paragraph_taken`: they leave the
paragraph, and a paragraph left with no line is removed.

The parser then closes the open blocks the line did not continue, closes the paragraph, links the new block as the last
child of the deepest open block that can hold it (closing the ones that cannot: `can_contain`), and sets its span from
its marker. A container (`CONTENT_BLOCKS`) lets the parser offer the rest of the line again (a block inside it); a
leaf ends the block starts for the line, and the rest of the line is its first content line (`CONTENT_LINES`) or a
new paragraph (`CONTENT_NONE`, if it is not blank).

### Continuing a block

On each line the open blocks are asked, outermost first, whether the line continues them; an extension is asked about
its own blocks only. `match` is:

| Value | |
|---|---|
| `BLOCK_MATCHED` | the line continues the block; `consumed` bytes of `line(offset:)` are its continuation marker |
| `BLOCK_UNMATCHED` | it does not: the block, and the blocks inside it, close (a paragraph inside can still take the line lazily, CommonMark 5.1) |
| `BLOCK_FINISHED` | the line is the block's last and belongs to it whole (a closing fence): the block closes, the line is done |

A block that takes lines receives the rest of each line it matches; they are handed to `block_close` as byte ranges
(`lines_first(i)` to `lines_last(i)` of `doc%source()`), never copied by the parser.

### What a block holds

| `block_content(kind)` | |
|---|---|
| `CONTENT_BLOCKS` | a container (a block quote): other blocks open inside it, as `can_contain` allows |
| `CONTENT_LINES` | a leaf taking the rest of each line it matches (a code block) |
| `CONTENT_NONE` | a one-line leaf (a thematic break) |

## The contract

What the parser checks on every claim, so that a faulty extension costs a status, never a hang or a corrupt tree. A
violation gives `stat = BARUCH_ERR_CONTRACT` and a message naming the extension, the hook, the line and the rule; the
parse stops there.

| Rule | Why |
|---|---|
| `block_start`'s `node` is a new, detached block node of an extension kind (number from `NODE_EXTENSION_FIRST`, not inline) | the parser links it; a node already in the tree, or a core kind, would corrupt the tree |
| `0 <= consumed <=` the bytes left in the line, for `block_start` and `block_continue` | the position stays inside the line |
| `consumed > 0` for a claimed block, unless it takes lines (`CONTENT_LINES`) | a container claimed without consuming input would be offered the same position again, forever: every claim makes progress, so the block starts of a line are bounded by its bytes |
| `0 <= paragraph_taken <=` the paragraph's lines | |
| `block_content` answers a `CONTENT_*` value, `block_continue` a `BLOCK_*` value | |

A hook that fails for its own reasons (memory, a kind clash in `register_kinds`) sets `stat` and `errmsg`; the parse
stops and returns them, prefixed with the extension and the hook.

## Kinds

An extension picks its kind numbers from `NODE_EXTENSION_FIRST` (1000) up and registers them in `register_kinds`
with a name and a role (block or inline): see [kinds](/reference/document#kinds). The registration happens in each
document, before each parse, so a document always describes its own nodes (`kind_name`, `kind_is_inline`). Two
enabled extensions registering one number or one name make the parse fail with `BARUCH_ERR_ARGUMENT`, the extension
named. A node of an extension kind has the [slots](/reference/document#the-slot-table) of every node (by convention
`STRING_LITERAL` for its content) and named attributes.

## Constants

| Name | Value | |
|---|---|---|
| `BLOCK_UNMATCHED` | 0 | `block_continue`: the line does not continue the block |
| `BLOCK_MATCHED` | 1 | `block_continue`: it does |
| `BLOCK_FINISHED` | 2 | `block_continue`: it is the block's last line |
| `CONTENT_NONE` | 0 | `block_content`: a one-line leaf |
| `CONTENT_BLOCKS` | 1 | `block_content`: a container |
| `CONTENT_LINES` | 2 | `block_content`: a leaf taking lines |
| `BARUCH_ERR_CONTRACT` | 7 | an extension broke the hook contract |

## No module state

The registry lives in the options, the parse state in local variables of the parse, the hooks' state in the
document: nothing in BARUCH has the SAVE attribute. `scripts/audit_state.sh` checks the library for it (SAVE
attributes, variables initialised in their declaration, fixed or preconnected units, `stop`); every count is 0:

```bash
bash scripts/audit_state.sh
```
