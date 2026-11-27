# Conventions

The rules every page assumes. They are the contract the library is being written to: the procedures they mention are
**planned** (see [About](/guide/)); the rules themselves are settled.

## Every byte sequence is a document

Parsing never fails: there is no Markdown syntax error, and a malformed input is sanitised, not rejected.

| Input | What BARUCH does |
|---|---|
| UTF-8 | the core encoding |
| malformed UTF-8 sequence, U+0000 | replaced by U+FFFD |
| UTF-8 byte order mark | stripped |
| UTF-16 or UTF-32 with a byte order mark | converted to UTF-8 (StringiFor's `to_utf8`) |
| Latin-1, windows-1252 | converted to UTF-8 when the caller says so (`encoding=`) |
| LF, CR, CRLF line endings | read alike; written as LF (CRLF on option) |

Byte offsets (node spans, walk events) refer to the document's own sanitised buffer, which the document keeps and
exposes, not to the caller's original bytes.

## Errors are returned, never stopped on

A procedure that can fail has optional `stat` and `errmsg` arguments: `stat = 0` on success, a nonzero code and a
message otherwise. BARUCH never executes `stop` or `error stop`: the caller decides what a failure means. The failures
are few, because parsing never fails:

- input and output (a file that cannot be opened, read or written);
- API misuse (an accessor called on a node of the wrong kind, a handle that is not a node, an `end_list` with no list
  open in the streaming writer);
- an extension breaking the hook contract.

## MPI- and OpenMP-agnostic

BARUCH runs inside parallel programs without knowing it:

- no MPI is linked or called; a program writes one report per rank, or one from rank 0, as it chooses;
- no mutable module state: no `save` variables, no module-level registry (the extensions enabled for a parse live in
  the options object the caller passes);
- reentrant procedures: different threads can parse different documents at the same time;
- units are opened with `newunit=`; nothing is written to a preconnected unit (`output_unit`, `error_unit`) unless the
  caller passes it;
- any number of writers can be open at once.

## Handles are integers

A node is an integer index into its document, valid while the document lives: editing (append, insert, unlink) never
renumbers nodes. An unlinked node is a tombstone until the caller compacts the document, the one operation that
renumbers.

## Names

`use baruch` is the only module a client needs. The public names are four types, `markdown_document`,
`markdown_options`, `markdown_writer` and `markdown_extension`, plus a few constants; the procedures are their
type-bound methods. PENF's kinds and StringiFor's `string` are not re-exported: a client that wants them uses those
libraries directly.
