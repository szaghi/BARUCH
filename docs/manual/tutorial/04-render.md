# 4. Render HTML

Chapter 3 parsed `report.md` into a tree; here the tree goes out again, as HTML, and as the events of a walk. The
program reads the same file:

<<< @/examples/output/t03_read-source.ansi{ansi}

## An HTML fragment

<<< @/examples/snippets/t04_render-fragment.f90

- `render_html` belongs to the options, like `parse`: the options hold the extensions, and an extension's nodes are
  written by its own `write_html` hook ([HTML](/reference/html#extensions)).
- The default output is a **fragment**: what goes inside a page's `<body>`, with no `<html>` wrapper, no style sheet
  and no script, so that a program (foresight, a site generator) embeds it in its own page.
- The result is a `character(:), allocatable`; the same call writes into a StringiFor `string` or into a unit opened
  for writing (`render_html(doc, unit)`), and `render_html_file` into a file.

## A standalone page

<<< @/examples/snippets/t04_render-page.f90

`standalone = .true.` wraps the fragment in a whole HTML5 document: doctype, `<meta charset="utf-8">`, a `<title>`. The
title is `title=` when given, else the text of the first heading. Still no style sheet and no script: the page is
plain HTML.

## Walk the events

<<< @/examples/snippets/t04_render-walk.f90

The renderer is a loop over the **walk**, and a program can run the same loop: `options%walk` advances one event per
call, and the program holds the walk's whole state, `node` and `event` (start with `node = 0`; `node = 0` again means
the walk is over).

- A block gives `EVENT_ENTER_BLOCK`, its children's events, `EVENT_LEAVE_BLOCK`; emphasis, links and images give
  `EVENT_ENTER_SPAN` and `EVENT_LEAVE_SPAN`; text, code spans, line breaks and raw inline HTML give one `EVENT_TEXT`.
  The events are md4c's callbacks, as a loop.
- `first` and `last` are the node's bytes in the source: the heading `# Field report #` is bytes 1 to 16, its closing
  `#` included.
- No recursion: the walk follows the tree's links, and a 10^5-deep nesting walks like a flat list.

## Running it

<<< @/examples/output/t04_render.ansi{ansi}

<<< @/examples/output/t04_render-page.ansi{ansi}

What the output shows:

- Every block is written as cmark writes it: `<p>` but in a tight list (`pressure`, `inlet`), `<ol>` for the `1)`
  items, `class="language-fortran"` from the info string, the HTML comment verbatim, nothing for the link reference
  definition.
- `*rig*` is still `*rig*`: the inline content of a paragraph is escaped text until the inline parser (plan phase 2).
  The renderer writes the inline kinds already (`<em>`, `<a href>`, `<img>`...): a tree built by hand with them comes
  out as cmark would write it.
- 20 blocks entered and left (the root, the headings, the paragraphs, the lists and their items, the code block, the
  quote, the break, the definition, the HTML block) and 8 texts: one per paragraph and heading.

The whole program:

<<< @/examples/snippets/t04_render.f90

::: tip What you learned
`options%render_html` writes a parsed document as an HTML fragment (into a `character`, a `string`, a unit) or, with
`standalone = .true.`, as a page; `render_html_file` writes it into a file. `options%walk` gives the same tree as a
sequence of events, the caller holding the state. Reference: [HTML](/reference/html), [Walk](/reference/walk).
:::
