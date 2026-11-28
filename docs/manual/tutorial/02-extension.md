# 2. Write an extension

Every syntax beyond CommonMark, GitHub's tables as much as your own, enters BARUCH the same way: a type extending
`markdown_extension`, registered in a `markdown_options` object, enabled when a parse should know it. This chapter
writes a small one: the lines that start with `%%` become one raw block, a kind of its own, with nothing inside parsed
as Markdown; in running text, `%name%` becomes a node of its own too. A second extension then marks the text between
two `==` runs, through the same delimiter algorithm emphasis uses.

```markdown
A paragraph.
%% raw line, *not* emphasis
  %%   indented marker

Another paragraph.
```

::: info Status
The block parser offers every position to the extensions' block hooks before CommonMark's own starts (headings,
lists, code...: [chapter 3](./03-read)), so an extension can take syntax away from the core. The inline parser
([chapter 5](./05-inlines)) offers the bytes an extension declares to its inline hooks; the bytes CommonMark reads
(`*`, `[`, `` ` ``...) stay the core's.
:::

## The type

<<< @/examples/snippets/t02_extension-type.f90

Each binding overrides one **hook**. Every hook has a default meaning "not mine", so an extension writes only the ones
its syntax needs: here nine of them, out of seventeen (the last two are for `%name%`, [below](#inline-syntax)). Two have no default: `name`, which the options find it by, and
`write_markdown`, because whatever an extension reads it must be able to write back.

## Its name and its kind

<<< @/examples/snippets/t02_extension-name.f90

A node's kind is an integer; the core kinds are below `NODE_EXTENSION_FIRST` (1000), an extension picks its own from
there. `register_kinds` is called before every parse, so the document knows the kind by name: `kind_name(1001)` is
`percent_block`, which is what the program prints below. The second kind, `percent_name`, is an inline one, for
`%name%`. Two extensions with one number (or one name) are a status
from the parse, not a silent mix-up.

## Opening a block

<<< @/examples/snippets/t02_extension-start.f90

The parser offers each position of each line to the extensions' `block_start`, in the order they were registered.
`line(offset:)` is what is left of the line once the open containers took their markers, `indent` the white space
after it in columns (4 or more belongs to an indented code block). To claim the position the hook creates a detached
node of its kind and says how many bytes its marker takes. The parser links the node, sets its span and closes what the
new block ends (here the paragraph above it).

The arguments it does not use are part of the interface every hook receives: `paragraph`, `paragraph_first` and
`paragraph_last` give the lines of a paragraph open at the position, for a block that turns them into something else
(a table header, a caption) and says so through `paragraph_taken`.

## Continuing it

<<< @/examples/snippets/t02_extension-continue.f90

On every following line the parser asks the open block's own extension whether the line continues it:
`BLOCK_MATCHED` with the bytes of the continuation marker, `BLOCK_UNMATCHED` to let it close (and `BLOCK_FINISHED`
for a last line that belongs to the block, as a closing fence does).

## What it holds, and closing it

<<< @/examples/snippets/t02_extension-close.f90

`block_content` says what a block of the kind holds: `CONTENT_LINES`, the rest of each line it matches;
`CONTENT_BLOCKS` for a container (it would hold paragraphs, lists, other extensions' blocks); `CONTENT_NONE` for a
one-line block. When the block closes, `block_close` receives its lines as byte ranges of the document's source,
never copies: the extension decides what to keep, here the text after the markers as the node's literal.

## Writing it back

<<< @/examples/snippets/t02_extension-markdown.f90

The tree writer ([chapter 6](/manual/tutorial/06-write)) calls `write_markdown` for every node, entering and leaving
it; a node of another kind answers `handled = .false.`. One hook serves both kinds of the extension: the block, and
`%name%` written back as it was read. A block's Markdown is written as lines behind the prefixes of the containers it
is in ([Writer](/reference/writer#extensions)).

## Parsing with it

<<< @/examples/snippets/t02_extension-parse.f90

`register` keeps a copy of the extension in the options, enabled. `parse` reads the document's source into its tree:
the tree is reset to its root, the enabled extensions register their kinds, the blocks are parsed.

<<< @/examples/snippets/t02_extension-markdown-out.f90

Disabling an extension keeps it registered; its syntax is then ordinary text:

<<< @/examples/snippets/t02_extension-disable.f90

## The contract

A hook that claims a position must make progress: consume at least one byte of the line, unless its block takes the
rest of the line as content. A container claimed without consuming anything would be offered the same position again,
forever. The parser checks this, and the rest of the
[contract](/reference/extensions#the-contract), and stops with a status instead:

<<< @/examples/snippets/t02_extension-careless-type.f90

<<< @/examples/snippets/t02_extension-careless.f90

<<< @/examples/snippets/t02_extension-contract.f90

## Inline syntax

Inside paragraphs, headings and any block whose content is inline, the inline parser reads the text byte by byte. An
extension enters there in two ways.

**A trigger.** `inline_triggers` returns the bytes that hand the extension the position, here `%`; `inline_start` is
then called wherever one stands, with the whole inline content of the block (`text`) and the trigger's position:

<<< @/examples/snippets/t02_extension-inline.f90

The hook reads `%`, a name, `%`; on a match it creates a detached node of its inline kind and says how many bytes it
takes, and the parser links the node in place, with its span. Anything else is "not mine" (`node = 0`): the `%` is
text, as in `100%`. A backslash still escapes a trigger (`\%name%` is text), because escapes are the core's. A claim
must take at least one byte, and the node must be new, detached and of an inline kind of the extension: anything else
is the same contract status as for blocks.

**A delimiter.** Some syntax cannot be read left to right: whether `==` opens a mark depends on a `==` further on,
and on what nests in between. Such an extension declares its character and lets the core's delimiter algorithm (the
one of emphasis) do the pairing:

<<< @/examples/snippets/t02_extension-mark-type.f90

<<< @/examples/snippets/t02_extension-mark.f90

The parser reads every run of `=` and computes its flanking exactly as for `*`; `delimiter_run` decides whether the
run can open or close (here: only runs of one or two). When a closer is reached, `delimiter_match` is asked about the
nearest opener of the same character: it says how many characters each run gives (0: not this one) and creates the
node; the parser moves what lies between the two runs into it, emphasis and links included. This is how GitHub's
strikethrough (`~~`) is built, with no change to the core.

Both extensions, enabled in one options object; the `%%` block stays raw (its `contains_inlines` is false), and the
`%name%` node has no `write_html`, so the renderer writes its content:

<<< @/examples/snippets/t02_extension-inline-parse.f90

## Running it

<<< @/examples/output/t02_extension.ansi{ansi}

The whole program:

<<< @/examples/snippets/t02_extension.f90

::: tip What you learned
An extension is a type extending `markdown_extension` that overrides the hooks its syntax needs; it registers its
kinds, claims positions in `block_start`, continues its blocks in `block_continue`, says what they hold in
`block_content` and receives their lines in `block_close`. Inline syntax enters through trigger bytes
(`inline_triggers`, `inline_start`) or delimiter characters paired by the core's algorithm (`delimiter_chars`,
`delimiter_run`, `delimiter_match`). The options hold the extensions and enable them by name; `parse` uses the enabled
ones. A hook that breaks the contract is a status, not a hang.
Reference: [Extensions](/reference/extensions).
:::
