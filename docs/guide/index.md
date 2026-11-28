# About

BARUCH (**B**idirectional **A**ST **R**eader and **U**nparser for **C**ommonMark, **H**ookable) reads Markdown into a
document tree and writes a tree back to Markdown, in pure Fortran.

::: warning Status
Implemented so far: the document tree (`markdown_document`, [reference](/reference/document)), its input (loading
any text or file as sanitised UTF-8, [reference](/reference/input)), the extension interface with the options that
hold the extensions (`markdown_extension`, `markdown_options`, [reference](/reference/extensions)), the CommonMark
block parser, which records the source style and calls the extensions' block hooks ([reference](/reference/blocks)),
the inline parser ([reference](/reference/inlines), [links](/reference/links)), the HTML renderer
([reference](/reference/html)) and the event walk ([reference](/reference/walk)), and the conformance harness that
runs the 652 CommonMark examples, all passing ([reference](/reference/conformance)), and the tree writer, whose output
reads back to the same tree on all 652 ([reference](/reference/writer)); the streaming writer is not written yet. This
page
states the design the code is being written to: everything below is **planned** unless it says otherwise. The plan,
step by step, is [szaghi/BARUCH#1](https://github.com/szaghi/BARUCH/issues/1).
:::

## What it is for

- **Reader** (written: 652/652 at plan step 2.4; cmark's pathological inputs in linear time at step 2.6): CommonMark 0.31.2,
  checked against the 652 examples of the specification. The block structure is parsed first
  ([Blocks](/reference/blocks)), the inline content of each block second ([Inlines](/reference/inlines)), as the specification describes it.
- **HTML renderer** (written, [HTML](/reference/html)): an HTML fragment by default (no `<html>` wrapper, no external
  CSS or JavaScript, ready to embed in a page), a standalone document on option.
- **Tree writer** (written, [Writer](/reference/writer)): the tree back to Markdown, in the style it was read in or in a
  canonical one. The round trip is semantic: re-reading what BARUCH writes gives the same tree, not necessarily the
  same bytes (652/652 in both styles at plan step 3.2).
- **Streaming writer** (planned): Markdown written block by block, with no tree, for the reports a program writes
  while it runs.
- **Extensions** (planned): the GitHub Flavored Markdown group, then the VitePress syntax, all built through one public
  extension interface. With every extension off, BARUCH is a CommonMark processor.

The first clients are FIGURE (BARUCH replaces md4c, its last C Markdown
dependency), ADAM (run reports) and foresight (reads Markdown into its HTML
pages and writes it). Each client wires BARUCH in its own repository.

## The design

Settled before the first line of the parser; the steps of the plan implement it in this order.

### Pure Fortran

No C binding. External tools (cmark, md4c) serve only as local investigation aids, never in the CI.

### Reader before writer

Escaping is defined by the parser: the writer escapes exactly what the reader would otherwise read as syntax.

### Semantic round trip

`parse(write(parse(x))) == parse(x)`, not byte equality. The reader records the source style of each construct:

| Construct | Style recorded |
|---|---|
| Emphasis, strong emphasis | `*` or `_` |
| Bullet list | `-`, `+` or `*` |
| Ordered list | `.` or `)` |
| Heading | ATX or setext; the closing `#`s of an ATX heading |
| Fenced code | the fence character and length |
| Thematic break | `-`, `*` or `_` |
| Link | inline, full, collapsed or shortcut reference, and its label |

Link reference definitions are nodes, in place. The writer reuses the recorded style and is canonical where there is
none; the `canonical` option ignores it. [Tree equality](/reference/equality) ignores byte spans and style.

### A flat tree

One document holds every node in flat arrays; a node is an integer index:

| Array | Holds |
|---|---|
| kind | the node's kind (paragraph, heading, emphasis, ... or an extension's registered kind) |
| parent, first and last child, previous and next sibling | the links of the tree |
| span | the node's first and last byte in the source |
| integers | the kind's own fields (heading level, list start, fence length, ...) |

Processed text lives in one string pool. No polymorphism and no recursion in the tree: the parser and the walk are
iterative, so nesting depth is bounded by memory, not by the stack. Integer handles stay valid while the document is
edited (FIGURE holds them): a deleted node is a tombstone until an explicit compaction. An extension adds a
registered kind number plus named attributes.

### The reader's interface

The tree, plus a walk that emits enter/leave block, enter/leave span and text events with byte offsets (md4c's
callback shape), written as an iterator whose state the caller holds ([Walk](/reference/walk)). There is no streaming
parser.

### One extension interface

An abstract type with type-bound hooks, each with a "not mine" default: block start, block continuation, content
rules, inline trigger and delimiter participation, post-processing, output as HTML, as Markdown (mandatory) and as
events. Every extension, BARUCH's own included, goes through it; the core is CommonMark only. Extensions are compiled
in and enabled per parse; a client can register its own. The block-start hook sees the lines of the open paragraph
(a GFM table needs them).

| Group | Extensions |
|---|---|
| GitHub Flavored Markdown | tables, strikethrough, task lists, extended autolinks, alerts, footnotes |
| VitePress | custom containers, math, front matter, heading anchors, `{.class}` attributes, `<<<` snippet imports and `<!--@include:-->` (kept as nodes, never resolved), `[[toc]]`, fence titles |
| md4c dialect | underline (FIGURE only, off by default, not CommonMark) |

MyST is out of scope until a client needs it.

### Every input is a document

Parsing never fails. See [Conventions](/guide/conventions) for the input rules and the error policy.

### Dependencies

[StringiFor](https://github.com/szaghi/StringiFor) (text, UTF-8, Unicode classes, encodings) and
[PENF](https://github.com/szaghi/PENF) (kinds), with FACE and BeFoR64, which StringiFor uses. The HTML5 entity table,
percent-encoding and HTML escaping are BARUCH's own.

