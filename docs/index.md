---
layout: home

hero:
  name: BARUCH
  text: Bidirectional AST Reader and Unparser for CommonMark, Hookable
  tagline: A Markdown parser and emitter in pure Fortran
  actions:
    - theme: brand
      text: Start here
      link: /guide/
    - theme: alt
      text: API Reference
      link: /api/
    - theme: alt
      text: View on GitHub
      link: https://github.com/szaghi/BARUCH

features:
  - title: Read and write
    details: Markdown into a document tree, and the tree back to Markdown. Re-reading what BARUCH writes gives the same tree.
  - title: CommonMark, checked
    details: The reader follows CommonMark 0.31.2 and is tested against the examples of its specification.
  - title: Opt-in extensions
    details: GitHub Flavored Markdown and the VitePress syntax (custom containers, math, front matter, snippet imports), built through one public extension interface; the core stays CommonMark.
---
