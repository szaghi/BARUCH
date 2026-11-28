---
layout: home

hero:
  name: BARUCH
  text: Bidirectional AST Reader and Unparser for CommonMark, Hookable
  tagline: A Markdown parser and emitter in pure Fortran
  image:
    light: /logo/baruch-mark.svg
    dark: /logo/baruch-mark-dark.svg
    alt: BARUCH, a leaning igeta holding 文 in a brushed ring
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
  - icon:
      light: /icons/light/round-trip.svg
      dark: /icons/dark/round-trip.svg
      width: 48
      height: 48
    title: Read and write
    details: Markdown into a document tree, and the tree back to Markdown. Re-reading what BARUCH writes gives the same tree.
  - icon:
      light: /icons/light/spec-examples.svg
      dark: /icons/dark/spec-examples.svg
      width: 48
      height: 48
    title: CommonMark, checked
    details: The reader follows CommonMark 0.31.2 and is tested against the examples of its specification.
  - icon:
      light: /icons/light/hookable-extensions.svg
      dark: /icons/dark/hookable-extensions.svg
      width: 48
      height: 48
    title: Opt-in extensions
    details: GitHub Flavored Markdown and the VitePress syntax (custom containers, math, front matter, snippet imports), built through one public extension interface; the core stays CommonMark.
---
