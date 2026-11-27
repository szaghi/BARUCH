# HTML

The HTML renderer: a document's tree written as cmark writes it, as an HTML fragment or a standalone page. Methods of
`markdown_options` (the options hold the extensions, whose nodes their own hooks write); module `baruch_html`
(internal: a client calls the methods). A worked example: [tutorial chapter 4](/manual/tutorial/04-render).

::: info Status (plan step 1.6)
Every core kind is written, blocks and inlines. Until the inline parser (plan phase 2) a parsed paragraph or heading
holds one text node, so its content is written as escaped text, `*emphasis*` and all; a tree built by hand with inline
nodes is written in full. The conformance driver renders through this renderer:
[Conformance](/reference/conformance#counts).
:::

## Methods

| Method | |
|---|---|
| `render_html(doc, html, standalone, title, stat, errmsg)` | `html`: `character(:), allocatable`, `intent(out)` |
| `render_html(doc, html, standalone, title, stat, errmsg)` | `html`: StringiFor `type(string)`, `intent(out)` |
| `render_html(doc, unit, standalone, title, stat, errmsg)` | `unit`: an `integer` unit the caller opened for writing |
| `render_html_file(doc, path, standalone, title, stat, errmsg)` | `path`: a file, created or replaced |

`render_html` is one generic name: the type of the third argument picks the sink. Every argument after it is optional:

| Argument | Default | |
|---|---|---|
| `standalone` | `.false.` | `.true.`: a whole HTML5 document instead of a fragment |
| `title` | the plain text of the first heading, or empty | the `<title>` of a standalone document (escaped); ignored for a fragment |
| `stat`, `errmsg` | | [the status](/guide/conventions) |

The document is read only (`intent(in)`), and the options too: several renders, of one document or of several, can run
at once.

## Fragment and standalone page

The default output is an HTML **fragment**: what goes inside a page's `<body>`, with no `<html>` wrapper, no style
sheet and no script, ready to embed (foresight's pages, a site generator, an e-mail). Line endings are LF.

`standalone = .true.` writes a whole document around the same fragment:

```html
<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>TITLE</title>
</head>
<body>
FRAGMENT
</body>
</html>
```

Still no style sheet, no script and no external resource: styling the page is the caller's business.

## What is written

Byte for byte what cmark 0.31 writes with raw HTML allowed (`cmark --unsafe`), the reference the
[conformance suite](/reference/conformance) compares with:

| Kind | HTML |
|---|---|
| `NODE_DOCUMENT` | its children |
| `NODE_BLOCK_QUOTE` | `<blockquote>` ... `</blockquote>` |
| `NODE_LIST` | `<ul>`, or `<ol>` (`<ol start="N">` when N is not 1) |
| `NODE_ITEM` | `<li>` ... `</li>` |
| `NODE_PARAGRAPH` | `<p>` ... `</p>`; nothing around it in a tight list |
| `NODE_HEADING` | `<h1>` to `<h6>` |
| `NODE_CODE_BLOCK` | `<pre><code>`, `<pre><code class="language-X">` when it has an info string (X its first word), the literal escaped |
| `NODE_HTML_BLOCK` | its literal, verbatim |
| `NODE_THEMATIC_BREAK` | `<hr />` |
| `NODE_LINK_REFERENCE_DEFINITION` | nothing |
| `NODE_TEXT` | its literal, escaped |
| `NODE_SOFTBREAK` | a line feed |
| `NODE_LINEBREAK` | `<br />` and a line feed |
| `NODE_CODE` | `<code>`, the literal escaped, `</code>` |
| `NODE_HTML_INLINE` | its literal, verbatim |
| `NODE_EMPH`, `NODE_STRONG` | `<em>`, `<strong>` |
| `NODE_LINK` | `<a href="D" title="T">` (no `title` when empty) |
| `NODE_IMAGE` | `<img src="D" alt="A" title="T" />`: A is the image's content as plain text |

Each block starts on a new line. **Escaping** is cmark's: in text, `&`, `<`, `>` and `"` become character references,
every other byte (UTF-8 included) is kept. A **destination** (D) is written as cmark's `escape_href` does: letters,
digits and `-_.+!*(),%#@?=;:/$~` kept, `&` as `&amp;`, `'` as `&#x27;`, every other byte percent-encoded (`%5B` for
`[`, `%20` for a space, each byte of a UTF-8 character). The **alt text** of an image is its content with the markup
removed: texts and code spans escaped, line breaks as spaces.

Not done here: dangerous URLs (`javascript:`) are not filtered and raw HTML is not suppressed, as `cmark --unsafe`;
the renderer writes what the document holds.

## Extensions

Each event of the [walk](/reference/walk) is offered first to the enabled extensions' `write_html` hooks, in
registration order, for **every** node, core kinds included: the first hook answering `handled = .true.` writes that
event (its `html`, possibly empty) and the core writes nothing for it. An extension can so write its own kinds and
change how a core kind is written (a heading with an anchor, a task list item with its checkbox).

```fortran
subroutine write_html(self, doc, node, entering, html, handled, stat, errmsg)
```

The hook is called with `entering = .true.` when the walk enters a node (or reaches a text) and `.false.` when it
leaves it, after its children. A hook setting `stat` stops the render: `render_html` returns that status, the message
prefixed with the extension's name, and the HTML written before it. The hook writes its own line feeds.

An **extension node no enabled extension writes** (its extension disabled, or with no `write_html`) is rendered
transparently: its children as if it were not there; with no children, its literal, escaped (a block on its own line).
Nothing is dropped and no markup is invented.

The output hooks were declared in step 1.3 ([Extensions](/reference/extensions#the-hooks)); the inline parser of
phase 2 adds no hook here.

## Errors

| `stat` | When |
|---|---|
| `BARUCH_OK` | the HTML was written; an uninitialised document gives an empty fragment |
| `BARUCH_ERR_IO` | a unit not open, open for reading only, or unformatted but not stream; a file that cannot be opened or written (the processor's message in `errmsg`) |
| `BARUCH_ERR_CONTRACT` | an extension's `walk_event` answer is not an event ([Walk](/reference/walk#extensions)) |
| any code | an extension's `write_html` hook failed with it |

A **unit**: a formatted one (sequential or stream) gets one record per line, the last one left open when the HTML does
not end with a line feed; an unformatted **stream** unit gets the bytes. The unit is the caller's: BARUCH never writes
to a unit it was not given (no `print`, no `output_unit` of its own), so the renderer is safe in MPI and OpenMP
programs ([Conventions](/guide/conventions)).
