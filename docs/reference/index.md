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
| `NODE_*` | constants | the node kinds, one per CommonMark node type, and the ranges | [Kinds](/reference/document#kinds) |
| `NODE_INT_SLOTS`, `NODE_STRING_SLOTS`, `SLOT_*`, `STRING_*` | constants | the slot layout per kind | [The slot table](/reference/document#the-slot-table) |
| `HEADING_*`, `LIST_*`, `CODE_*`, `LINK_*` | constants | the values of the style and type slots | [The slot table](/reference/document#the-slot-table) |
| `BARUCH_OK`, `BARUCH_ERR_*` | constants | the status codes | [Errors](/reference/document#errors) |

Planned, with their steps:

| Name | Kind | | Plan step |
|---|---|---|---|
| `markdown_options` | type | what a parse or a write does: the enabled extensions, the encoding, the line ending, canonical output | 1.3 |
| `markdown_writer` | type | the streaming writer | 4.1 |
| `markdown_extension` | abstract type | the extension interface | 1.3 |

The procedures are type-bound methods of these types.

## Pages, phase by phase

| Phase | Page | Contents | Plan step |
|---|---|---|---|
| 1. The document and the blocks | [The document](/reference/document) | nodes, kinds, accessors, editing, compaction | 1.1 |
| | [Input](/reference/input) | loading from text, a `string` or a file; sanitising, encodings, line endings | 1.2 |
| | Extensions | the hooks, their contract, registered kinds | 1.3 |
| | Conformance | how to run the spec suite, what is normalised, the pinned version and counts | 1.4 |
| | Blocks | the block kinds and their fields | 1.5 |
| | HTML | the renderer: fragment and standalone output | 1.6 |
| | Walk | the events and their offsets | 1.6 |
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

- [API](/api/): every module, type and procedure, generated from the sources by formal.
- [Conventions](/guide/conventions): errors, input, the MPI/OpenMP-agnostic contract.
