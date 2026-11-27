# Links and references

Links and images in a parsed document, and the link reference definitions their references resolve against
(CommonMark 0.31.2, spec 4.7, 6.3, 6.4). Two inline kinds, `NODE_LINK` and `NODE_IMAGE`, read through
[the document](/reference/document)'s accessors; modules `baruch_inlines` (the parser) and `baruch_references`
(the definitions by label), both internal. A worked example: the cookbook's
[List every link of a page](/manual/cookbook#list-every-link-of-a-page).

## The nodes

A link and an image are **spans**: their children are the link text, or the image's description, parsed as inline
content (emphasis, code spans, images... but no link inside a link). The rest is in their fields:

| Field | Accessor | |
|---|---|---|
| destination | `destination(n)` | the URL, backslash escapes and character references processed (`/f%C3%B6` stays as written, the renderer percent-encodes what needs it) |
| title | `title(n)` | without its delimiters, processed the same way; empty for none |
| form (style) | `int_slot(n, SLOT_LINK_FORM)` | how it was written: `LINK_INLINE`, `LINK_FULL`, `LINK_COLLAPSED`, `LINK_SHORTCUT`, `LINK_AUTOLINK` |
| label (style) | `label(n)` | for the three reference forms, the label as written: what was looked up |
| span | `span_first(n)`, `span_last(n)` | `[` (or the `!` of an image) to the last byte: the `)` of an inline link, the `]` of a reference |

An autolink (`<https://...>`) is a `NODE_LINK` too, form `LINK_AUTOLINK` ([Inlines](/reference/inlines#autolinks)).

## The forms

| Written | Form | `label` | Destination and title from |
|---|---|---|---|
| `[text](/url "title")` | `LINK_INLINE` | empty | the link itself |
| `[text][label]` | `LINK_FULL` | `label` | the definition of `label` |
| `[label][]` | `LINK_COLLAPSED` | `label` (the text) | the definition of the text |
| `[label]` | `LINK_SHORTCUT` | `label` (the text) | the definition of the text |
| `<https://x.y>`, `<a@b.c>` | `LINK_AUTOLINK` | empty | the content |

An image is the same with `!` before `[`, in the same four bracket forms. At a `]` the parser tries them in this
order: an inline link (a `(` right after `]`), then a full reference (a `[label]` right after `]`, no white space
between), then a collapsed one (`[]`), then a shortcut one. A full reference whose label has no definition is not
tried as a shortcut: `[foo][bar]` with only `foo` defined is text up to `]`, then `[bar]` is read again on its own.

### Inline links

`[text]`, then `(`, optional white space (spaces, tabs, at most one line ending), the **destination**, white space,
an optional **title**, white space, `)`:

- the destination is either between `<` and `>` (anything but a line ending or an unescaped `<` or `>`: spaces
  allowed, `<>` is empty) or **raw**: no space, no ASCII control character, parentheses only escaped or balanced (at
  most 32 levels, as cmark); a raw destination may be empty (`[a]()`);
- the title is between `"` and `"`, `'` and `'`, or `(` and `)` (no unescaped `(` inside), a backslash escaping the
  closing delimiter; it must be separated from the destination by white space: `[a](/u"t")` has the destination
  `/u"t"`;
- with anything else before the `)`, there is no inline link, and the brackets may still be a reference
  (`[foo] (/url)` with `[foo]` defined is a shortcut link followed by the text ` (/url)`).

### References

The link text of a collapsed or shortcut reference is its label; the label of a full one is in the second brackets.
A label holds at most 999 characters, no unescaped bracket, and something that is not white space: in `[foo][ ]` the
`[ ]` is no label, so `[foo]` is a shortcut reference followed by the text `[ ]` (as the spec reads it; cmark takes
`[ ]` as `[]`). A shortcut or collapsed reference is not tried when the link text holds
another bracket (`[a [b] c]`): a label cannot hold one.

A label that matches no definition makes no link: the brackets are text and the parse goes on after `]`
(`[nowhere]` stays `[nowhere]`).

## Matching a label

A label matches a definition when their **normalised** forms are equal (spec 4.7):

1. spaces, tabs and line endings at both ends are removed;
2. every run of them inside becomes one space;
3. the Unicode **full case folding** is applied (StringiFor `casefold()`): `[FOO]` matches `[foo]`, `[ẞ]` matches
   `[SS]` (both fold to `ss`), `[ΑΓΩ]` matches `[αγω]`.

Nothing else is processed: backslash escapes and character references are compared as written, so `[foo\!]` does
not match `[foo!]`, and `[&amp;]` does not match `[&]`. A label of white space only matches nothing.

When two definitions have the same normalised label, **the first one in the document wins**; the later ones are still
nodes in the tree, used by nothing.

## Reference definitions

A definition (`[label]: destination "title"`) is a block: a `link_reference_definition` node in place, holding its
label as written, its destination and its title processed ([Blocks](/reference/blocks)); the renderer writes nothing
for it. It may come before or after the references to it, in any container: the block phase reads every definition
before any inline content is parsed.

At the start of the inline phase `baruch_references` collects the definitions in document order (the tree's links,
no recursion) into a hash table of normalised labels (FNV-1a, open addressing, at most half full): building it is
linear in the labels' bytes, and a lookup costs the normalisation of its label (bounded by 999 characters) and O(1)
probes on average. A document with 10^5 definitions and as many references parses in linear time.

## Images

An image is a link with `!` before it: the same forms, the same resolution, `NODE_IMAGE`. Its children are the
**description**, inline content like a link text, links included (`![[a](/b)](/c)`); the HTML renderer writes its
**plain text** (the literals of the text and code nodes under it, line breaks as spaces, no markup) as the `alt`
attribute ([HTML](/reference/html)).

## Links do not nest

A link may not contain another link. When a link is made, every `[` still open before it is deactivated: its `]`
will be text. So `[a [b](/c)](/d)` is the text `[a `, a link `b`, then the text `](/d)`. Images are not deactivated
and may hold links. What is not a bracket keeps its precedence: a code span, an autolink or raw HTML that starts
inside the link text and ends after the `]` wins (`` [foo`](/uri)` `` is no link: the code span holds `](/uri)`).

Emphasis inside the text belongs to the text: the delimiters between `[` and `]` are matched among themselves when the
link is made, so `*[foo*](/u)` is a `*` before a link whose text is `foo*`.

## The form and the label

`SLOT_LINK_FORM` and `STRING_LABEL` are **style**: two documents differing only in them have equal trees (plan step
3.1), and the writer (step 3.2) uses them to write a link back the way it was written: `[text][label]` stays a full
reference, `[label]` a shortcut, instead of the canonical inline form. The label is kept as written (case, inner
white space) because that is what the definition is found by; a writer ignoring the style writes the inline form,
the destination and the title resolved.

| Node | `SLOT_LINK_FORM` | `label(n)` |
|---|---|---|
| `[x](/u)` | `LINK_INLINE` (1) | empty |
| `[x][Foo Bar]` | `LINK_FULL` (2) | `Foo Bar` |
| `[Foo Bar][]` | `LINK_COLLAPSED` (3) | `Foo Bar` |
| `[Foo Bar]` | `LINK_SHORTCUT` (4) | `Foo Bar` |
| `<https://x.y>` | `LINK_AUTOLINK` (5) | empty |

## Errors

Nothing about a link fails: a construct that is not a link is text. `parse` returns a nonzero `stat` only when memory
runs out ([Inlines](/reference/inlines#errors)).
