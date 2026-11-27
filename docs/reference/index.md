# Reference

Every public entity of BARUCH, one page per part of the library, plus the [API](/api/) generated from the sources.

::: info Planned
The pages are written phase by phase, as the plan ([szaghi/BARUCH#1](https://github.com/szaghi/BARUCH/issues/1)) adds
the code; each public entity gets its row in the step that adds it. The rules every page assumes are in
[Conventions](/guide/conventions).
:::

## What `use baruch` exports

| Name | Kind | | Page |
|---|---|---|---|
| `markdown_document` | type | the document: the flat node arrays, the string pool, the typed accessors | [The document](/reference/document) |
| | | its source: loading from text, a `string` or a file, sanitised UTF-8, the lines | [Input](/reference/input) |
| `markdown_options` | type | what a parse does: the extensions registered and enabled by name; `parse` | [Extensions](/reference/extensions#the-options), [Blocks](/reference/blocks#parsing) |
| | | `render_html`, `render_html_file`: the tree as HTML, a fragment or a standalone page | [HTML](/reference/html) |
| | | `walk`: the tree as events, one per call | [Walk](/reference/walk) |
| `EVENT_*` | constants | the walk's events: enter and leave a block or a span, text | [Walk](/reference/walk#events) |
| `markdown_extension` | abstract type | the extension interface: the hooks and their defaults | [Extensions](/reference/extensions#the-hooks) |
| `BLOCK_*`, `CONTENT_*` | constants | what the block hooks answer | [Extensions](/reference/extensions#constants) |
| `NODE_*` | constants | the node kinds, one per CommonMark node type, and the ranges | [Kinds](/reference/document#kinds) |
| `NODE_INT_SLOTS`, `NODE_STRING_SLOTS`, `SLOT_*`, `STRING_*` | constants | the slot layout per kind | [The slot table](/reference/document#the-slot-table) |
| `HEADING_*`, `LIST_*`, `CODE_*`, `LINK_*` | constants | the values of the style and type slots | [The slot table](/reference/document#the-slot-table) |
| `BARUCH_OK`, `BARUCH_ERR_*` | constants | the status codes | [Errors](/reference/document#errors) |

Planned, with their steps:

| Name | Kind | | Plan step |
|---|---|---|---|
| `markdown_options` | type | more of what a write does: the line ending, canonical output | 3.2 |
| `markdown_writer` | type | the streaming writer | 4.1 |

The procedures are type-bound methods of these types.

## Pages, phase by phase

| Phase | Page | Contents | Plan step |
|---|---|---|---|
| 1. The document and the blocks | [The document](/reference/document) | nodes, kinds, accessors, editing, compaction | 1.1 |
| | [Input](/reference/input) | loading from text, a `string` or a file; sanitising, encodings, line endings | 1.2 |
| | [Extensions](/reference/extensions) | the hooks, their contract, registered kinds, the options | 1.3 |
| | [Conformance](/reference/conformance) | how to run the spec suite, what is normalised, the pinned version and counts | 1.4 |
| | [Blocks](/reference/blocks) | the block kinds, their fields and style slots, what `parse` builds | 1.5 |
| | [HTML](/reference/html) | the renderer: fragment and standalone output, sinks, extension hooks | 1.6 |
| | [Walk](/reference/walk) | the events and their offsets | 1.6 |
| 2. Inlines | Conformance | the pinned HTML5 entity table; the pathological inputs | 2.2, 2.6 |
| | Inlines | the inline kinds and their fields | 2.3 |
| | Links and references | links, images, reference definitions | 2.4 |
| | Extensions | the inline hooks | 2.5 |
| 3. The tree writer | Equality | what tree equality compares and ignores | 3.1 |
| | Writer | recorded style, canonical mode, escaping | 3.2 |
| | Conformance | fuzzing | 3.3 |
| 4. The streaming writer | Streaming writer | containers, leaf blocks, tables, helpers | 4.1 |
| 5. GFM extensions | GFM | tables, strikethrough, task lists, extended autolinks, alerts, footnotes | 5.1-5.6 |
| 6. VitePress syntax | VitePress | containers, math, front matter, anchors, attributes, imports, `[[toc]]`, fence titles | 6.1-6.8 |
| | Extensions | underline (md4c dialect, not CommonMark) | 6.9 |
| 7. Measure and close | Performance | throughput against cmark, and how it was measured | 7.1 |

## Elsewhere

- [Conformance](/reference/conformance): the CommonMark 0.31.2 suite, how to run it, the current counts.
- [API](/api/): every module, type and procedure, generated from the sources by formal.
- [Conventions](/guide/conventions): errors, input, the MPI/OpenMP-agnostic contract.
