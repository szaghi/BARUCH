# Tutorial

A path through BARUCH from an empty document to a run report, one chapter per capability, each built on a program you
can compile and run: the code and its output on every page are generated from `docs/examples/src` by
`scripts/docs_examples.sh`.

::: info Planned
Each chapter arrives with the step of the plan ([szaghi/BARUCH#1](https://github.com/szaghi/BARUCH/issues/1)) that
makes it possible; the ones not linked below are not written yet.
:::

| Chapter | What it shows | Plan step |
|---|---|---|
| [1. A document by hand](./01-document) | build a tree node by node, read it back through the accessors, edit and compact it | 1.1 |
| Write an extension | a `%%` block extension through the public interface, its block hooks | 1.3 (inline hooks: 2.5) |
| Read a document | parse a file, inspect the tree | 1.5 |
| Render HTML | an HTML fragment and a standalone page; the walk's events | 1.6 |
| Inline content | code spans, autolinks, entities, emphasis and links in the tree | 2.3, 2.4 |
| Edit and write back | change a parsed document and write it, in its own style or canonical | 3.2 |
| A run report | a solver-like loop writing its report while it runs, with the streaming writer | 4.1 (final at G4) |
