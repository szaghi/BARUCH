# Extensions

Type `markdown_extension` (abstract), the hooks every extension overrides or inherits; type `markdown_options`, which
holds the extensions and parses with them. Modules `baruch_extension`, `baruch_options`, `baruch_blocks` (the block
parser) and `baruch_inlines` (the inline parser), the last two internal; a client reaches them through `use baruch`.

Every syntax beyond CommonMark goes through this interface, BARUCH's own extensions (GFM, VitePress: plan phases 5
and 6) as much as a client's. The core is CommonMark only; extensions are compiled in and enabled per parse. A worked
example: [tutorial chapter 2](/manual/tutorial/02-extension).

::: info Status (plan step 2.5)
Every hook is declared with its default, and all are live but `write_markdown` (the tree writer, step 3.2). The
**block** hooks: the CommonMark block parser ([Blocks](/reference/blocks)) calls them on every line, an extension's
starts before the core's. The **inline** hooks and `contains_inlines`: the inline parser ([Inlines](/reference/inlines))
calls them at the bytes the extensions declare, and pairs their delimiter runs in its delimiter algorithm.
`post_process`: `parse` calls it after the inlines. The **HTML** and **walk** hooks: the renderer
([HTML](/reference/html#extensions)) and the walk ([Walk](/reference/walk#extensions)).
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
| `parse(doc, stat, errmsg)` | the document's source (`load`, `load_file`) into its tree, with the enabled extensions: blocks, inlines, post-processing |
| `render_html(doc, html, ...)`, `render_html_file(doc, path, ...)` | the tree as HTML, the enabled extensions' nodes by their hooks ([HTML](/reference/html)) |
| `walk(doc, node, event, ...)` | the tree as events, one per call ([Walk](/reference/walk)) |

`parse` resets the tree to its root (the source and its lines are kept, the registered kinds are dropped), lets the
enabled extensions register their kinds, runs the block parser, the inline parser, then the enabled extensions'
`post_process` hooks. It never fails on the text: every byte sequence is a document. A nonzero `stat` comes from an extension (a hook reporting a failure, or breaking the
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
| `contains_inlines(kind)` | false | **live**: by the inline parser, for each block of an extension kind | the block's content is inline content (emphasis, links...): [inline content of a block](#inline-content-of-a-block) |
| `inline_triggers()` | none (`''`) | **live**: once per parse | the bytes that hand it an inline position ([inline triggers](#inline-triggers)) |
| `inline_start(doc, text, offset, node, consumed, stat, errmsg)` | not mine | **live**: at each of its trigger bytes in inline content | an inline construct there |
| `delimiter_chars()` | none | **live**: once per parse | characters whose runs take part in the delimiter algorithm, `~` for strikethrough ([delimiter participation](#delimiter-participation)) |
| `delimiter_run(doc, text, first, length, left_flanking, right_flanking, before_punct, after_punct, can_open, can_close)` | the rule of `*` | **live**: at each run of its characters | whether the run can open, close, or is text |
| `delimiter_match(doc, delimiter, opener_length, closer_length, opener_original, closer_original, opener_can_close, closer_can_open, used, node, stat, errmsg)` | no match | **live**: for each closer of its characters, opener by opener | characters used from each run, the node wrapping the content |
| `post_process(doc, stat, errmsg)` | nothing | **live**: after the inlines, once per parse | edit the parsed tree (task list checkboxes, footnote numbers): [post-processing](#post-processing) |
| `write_html(doc, node, entering, html, handled, stat, errmsg)` | not mine | **live**: by `render_html`, for every event of every node, core kinds too | a node as HTML, on entering and on leaving it ([HTML](/reference/html#extensions)) |
| `write_markdown(doc, node, entering, markdown, handled, stat, errmsg)` | none: deferred | step 3.2 | a node as Markdown: mandatory, whatever an extension reads it writes back |
| `walk_event(doc, node, entering, event, handled)` | not mine | **live**: by `walk` (and the renderer), for the extension kinds | the walk event a node is reported as ([Walk](/reference/walk#extensions)) |

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

## Inline triggers

`inline_triggers` returns bytes; at each of them, in the inline content of every block the inline parser reads, the
parser offers the position to the `inline_start` of the enabled extensions that declared the byte, in registration
order, and the first that claims it wins. The bytes CommonMark reads stay the core's: an extension cannot declare
`` ` `` `\` `&` `<` `*` `_` `[` `]` `!` (nor anything but a graphic ASCII character, `!` to `~`): such a declaration is
a [contract](#the-contract) violation, reported when the inline phase starts (before any inline content is read). So
a backslash escapes an extension's trigger as any ASCII punctuation (`\%` is text), and a code span or an autolink
hides it.

| Argument | |
|---|---|
| `text` | the *subject*: the inline content of the block, its lines without their leading white space, joined by LF |
| `offset` | where the trigger byte stands; `text(:offset-1)` is parsed (the hook may look back: a construct allowed only at a word start) |
| `node` | out: a new detached node of an inline kind of the extension, 0 for "not mine" |
| `consumed` | out: the bytes of `text(offset:)` it takes, at least 1 |

On a claim the parser writes the text before the position as a text node, links `node` after it, sets its span to
the source bytes of `text(offset:offset+consumed-1)` (the subject's offsets mapped back through the container markers
removed between its lines), and goes on after them. The node may have children the hook built; their spans are the
hook's to set (a leaf whose content is its `STRING_LITERAL` is the usual case). "Not mine" from every extension: the
byte is text.

## Delimiter participation

Syntax that pairs runs of a character, as emphasis does, is read by the core's delimiter algorithm (CommonMark 6.2
and its appendix: cmark's `process_emphasis`) on the extension's behalf; GFM strikethrough (`~`, `~~`: plan step 5.2)
needs nothing else. One character belongs to one extension: two enabled extensions declaring it make the parse fail
with `BARUCH_ERR_ARGUMENT`. At a byte that is both an extension's trigger and a delimiter character, `inline_start` is
tried first.

1. **The run.** At a run of the character the parser computes what it computes for `*`: left- and right-flanking
   (Unicode White_Space and punctuation around the run, the start and the end of the subject as white space), and
   whether punctuation stands before and after. `delimiter_run` answers `can_open` and `can_close` from them (and the
   run's length: GFM strikethrough refuses runs of three or more); a run that can do neither is text. Default: the rule
   of `*`.
2. **The match.** When the delimiter algorithm reaches a closer of the character, it goes down the delimiter stack to
   the openers of the same character, nearest first, and offers each to `delimiter_match` with what the emphasis rules
   look at: the characters left in both runs (`opener_length`, `closer_length`), their lengths as read
   (`opener_original`, `closer_original`, the "rule of 3"'s input), whether the opener can also close and the closer
   also open. `used = 0` refuses the opener (the search goes on below); `used > 0` takes that many characters from
   each run, and `node` is a new detached childless inline node of the extension.
3. **The node.** The parser moves the nodes between the two runs into `node` (emphasis, links, other extensions'
   nodes, already paired: closers are processed left to right), sets its span from the first used character of the
   opener to the last used of the closer, links it in their place and drops the delimiters between. Characters left
   in the closer try again, as for emphasis; characters left unused stay text.

A closer that finds no opener (every one refused, or none) bounds the search of the later closers of the same character
and the same can-open flag, as cmark's `openers_bottom` does for emphasis: the matching is linear in the number of
runs. Runs inside a link's text are paired before the link closes, as emphasis is.

## Inline content of a block

A block of an extension kind whose `contains_inlines(kind)` is true is read by the inline parser as a paragraph is,
when it holds exactly one text node: `block_close` creates it, with the content as its `STRING_LITERAL` (lines joined
by LF, their leading white space removed) and its span starting at the content's first source byte (each later line
of the content is taken to end where its source line ends). The text node is replaced by the inline nodes, with spans,
emphasis, links and the extensions' inline syntax. A table cell (plan step 5.1) is such a block.

## Post-processing

After the inline phase, `parse` calls `post_process(doc, stat, errmsg)` of each enabled extension, in registration
order, once: the hook sees the whole tree, blocks and inlines, and may edit it (set attributes, insert or unlink
nodes: a task list item's checkbox, footnote numbers, an autolink found in text). The tree is compacted after the hooks
when they left tombstones, so a parsed document holds none; node handles taken during a hook are only valid until
then. A nonzero `stat` from a hook stops the parse and is returned, prefixed with the extension and the hook.

## The contract

What the parser checks on every claim, so that a faulty extension costs a status, never a hang or a corrupt tree. A
violation gives `stat = BARUCH_ERR_CONTRACT` and a message naming the extension, the hook, where (the line for a block
hook, the source byte for an inline one) and the rule; the parse stops there.

| Rule | Why |
|---|---|
| `block_start`'s `node` is a new, detached block node of an extension kind (number from `NODE_EXTENSION_FIRST`, not inline) | the parser links it; a node already in the tree, or a core kind, would corrupt the tree |
| `0 <= consumed <=` the bytes left in the line, for `block_start` and `block_continue` | the position stays inside the line |
| `consumed > 0` for a claimed block, unless it takes lines (`CONTENT_LINES`) | a container claimed without consuming input would be offered the same position again, forever: every claim makes progress, so the block starts of a line are bounded by its bytes |
| `0 <= paragraph_taken <=` the paragraph's lines | |
| `block_content` answers a `CONTENT_*` value, `block_continue` a `BLOCK_*` value | |
| `inline_triggers` and `delimiter_chars` declare graphic ASCII bytes (`!` to `~`), none of the core's (`` ` `` `\` `&` `<` `*` `_` `[` `]` `!`) | the core's syntax stays the core's; white space and UTF-8 bytes would break the text runs |
| `inline_start`'s `node` is a node created by the call, detached, of an inline kind of an extension | the parser links it; a node of the tree, a core kind or a block would corrupt the tree |
| `1 <= consumed <=` the bytes left in the subject, for a claimed inline | every claim makes progress: the inline parse is bounded by the subject's bytes |
| `0 <= used <=` the characters left in each run, for `delimiter_match`; `used > 0` with a node created by the call, detached, childless, of an inline kind of an extension; `used = 0` with `node = 0` | the parser moves the content into the node: it must be empty and free |

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
