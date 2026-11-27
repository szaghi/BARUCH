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
| [2. Write an extension](./02-extension) | a `%%` block extension through the public interface, its block hooks, the contract | 1.3 (inline hooks: 2.5) |
| [3. Read a document](./03-read) | parse a file, inspect the tree: what each block means and how it was written | 1.5 |
| [4. Render HTML](./04-render) | an HTML fragment and a standalone page; the walk's events | 1.6 |
| [5. Inline content](./05-inlines) | code spans, autolinks, raw HTML, references, line breaks in the tree (emphasis and links: 2.4) | 2.3, 2.4 |
| Edit and write back | change a parsed document and write it, in its own style or canonical | 3.2 |
| A run report | a solver-like loop writing its report while it runs, with the streaming writer | 4.1 (final at G4) |
