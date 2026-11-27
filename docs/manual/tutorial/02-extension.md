# 2. Write an extension

Every syntax beyond CommonMark, GitHub's tables as much as your own, enters BARUCH the same way: a type extending
`markdown_extension`, registered in a `markdown_options` object, enabled when a parse should know it. This chapter
writes a small one: the lines that start with `%%` become one raw block, a kind of its own, with nothing inside parsed
as Markdown.

```markdown
A paragraph.
%% raw line, *not* emphasis
  %%   indented marker

Another paragraph.
```

::: info Status
The block parser offers every position to the extensions' block hooks before CommonMark's own starts (headings,
lists, code...: [chapter 3](./03-read)), so an extension can take syntax away from the core. The inline hooks come
with phase 2.
:::

## The type

<<< @/examples/snippets/t02_extension-type.f90

Each binding overrides one **hook**. Every hook has a default meaning "not mine", so an extension writes only the ones
its syntax needs: here seven of them, out of sixteen. Two have no default: `name`, which the options find it by, and
`write_markdown`, because whatever an extension reads it must be able to write back.

## Its name and its kind

<<< @/examples/snippets/t02_extension-name.f90

A node's kind is an integer; the core kinds are below `NODE_EXTENSION_FIRST` (1000), an extension picks its own from
there. `register_kinds` is called before every parse, so the document knows the kind by name: `kind_name(1001)` is
`percent_block`, which is what the program prints below. Two extensions with one number (or one name) are a status
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

The tree writer (plan step 3.2) calls `write_markdown` for every node, entering and leaving it; a node of another kind
answers `handled = .false.`.

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

## Running it

<<< @/examples/output/t02_extension.ansi{ansi}

The whole program:

<<< @/examples/snippets/t02_extension.f90

::: tip What you learned
An extension is a type extending `markdown_extension` that overrides the hooks its syntax needs; it registers its
kinds, claims positions in `block_start`, continues its blocks in `block_continue`, says what they hold in
`block_content` and receives their lines in `block_close`. The options hold the extensions and enable them by name;
`parse` uses the enabled ones. A hook that breaks the contract is a status, not a hang.
Reference: [Extensions](/reference/extensions).
:::
