# Input

Type `markdown_document`, its source methods; the work is done by module `baruch_input` (internal: a client calls the
methods). A client reaches them through `use baruch`.

A document keeps the text it is read from, its **source**: any bytes, sanitised to UTF-8 once, at load time. Every
byte offset BARUCH gives (node [spans](/reference/document#spans), the lines below, the walk events of a later step)
is a position in this buffer, not in the caller's original bytes. Every byte sequence loads: there is no input error,
only I/O errors and an unsupported encoding name.

## Loading

| Method | |
|---|---|
| `load(text, encoding, stat, errmsg)` | `text` a `character` (any bytes) or a StringiFor `string` (unallocated: an empty text) |
| `load_file(path, encoding, stat, errmsg)` | the file read whole: stream access, a unit of `newunit=`, closed before returning |

A load replaces the source, indexes its lines and resets the tree to its root alone (as
[`initialize`](/reference/document#life-cycle)): the nodes of a previous source would have meaningless spans. On
failure the document is left exactly as it was.

## What a load does to the bytes

1. **The encoding.** `encoding=` names it (any name or alias below, case and surrounding blanks ignored). Absent or
   blank, a byte order mark decides (UTF-8, UTF-16LE, UTF-16BE, UTF-32LE, UTF-32BE; `FF FE 00 00` is UTF-32LE), and
   with none the text is UTF-8. `UTF-16` and `UTF-32` without an endianness take it from a byte order mark, big
   endian without one (RFC 2781).
2. **To UTF-8.** Another encoding is converted by StringiFor's `to_utf8`; then every ill-formed UTF-8 sequence becomes
   U+FFFD by StringiFor's `to_valid_utf8`: one U+FFFD per maximal subpart (Unicode 3.9), which is what Python's
   `bytes.decode('utf-8', 'replace')` does. In UTF-16 and UTF-32 an unpaired surrogate, a unit above U+10FFFF and an
   incomplete unit at the end each become one U+FFFD; a windows-1252 byte with no character (`81 8D 8F 90 9D`) and a
   US-ASCII byte above 127 one U+FFFD each.
3. **The byte order mark.** One leading U+FEFF is removed, whatever the encoding (a second one, or one later in the
   text, is kept: it is text).
4. **U+0000** becomes U+FFFD (CommonMark 2.3).

Nothing else changes: line endings, tabs and every well-formed character stay as they are.

| Encoding | Aliases (case ignored) |
|---|---|
| `UTF-8` | `utf8`, `unicode-1-1-utf-8`, `csutf8` |
| `US-ASCII` | `ascii`, `us`, `ansi_x3.4-1968`, `iso646-us`, `cp367`, ... |
| `ISO-8859-1` | `latin1`, `latin-1`, `l1`, `iso8859-1`, `cp819`, ... |
| `windows-1252` | `cp1252`, `x-cp1252`, `cswindows1252` |
| `UTF-16`, `UTF-16LE`, `UTF-16BE` | `utf16`, `utf16le`, `utf16be`, ... |
| `UTF-32`, `UTF-32LE`, `UTF-32BE` | `utf32`, `utf32le`, `utf32be`, ... |

The list is StringiFor's `canonical_encoding`; any other name gives `BARUCH_ERR_ARGUMENT` with the supported list in
the message.

### Checked against Python

`scripts/input_corpus.py` loads StringiFor's UTF-8 test corpus (every class of ill-formed UTF-8 of Unicode Table 3-7,
Latin-1 text, random mixes and random binary data) under no encoding and `UTF-8`, the fixed and binary cases under
every other encoding, and the well-formed ones written in UTF-16 and UTF-32 with their byte order mark; Python computes
the expected UTF-8 by the four rules above. The test `baruch_test_input` loads the 4925 cases and finds every one
byte-identical (2438 of them with a U+FFFD), with gfortran, nvfortran and ifx.

```bash
python3 -I scripts/input_corpus.py           # regenerate src/tests/data/input_corpus.txt (after fobis fetch)
python3 -I scripts/input_corpus.py --check   # is the committed data up to date with StringiFor's corpus?
```

## Line endings and lines

LF, CR and CRLF are all line endings (CommonMark 2.1), and the source **keeps them as they are**: the buffer is the
caller's text, sanitised and nothing else. The document indexes the lines instead, so that

- every span is an exact byte range of the text the caller gave (CRLF files included), with no offset map;
- each line is available without its ending, whatever the ending, which is all the block parser needs;
- the ending of each line stays known, for a writer that wants to keep it.

A line ends at LF, CR, CRLF or the end of the source; a final line ending does not start one more line, and an empty
source has none: `"a\nb\n"` and `"a\nb"` have two lines, `"\n"` one empty line.

| Method | |
|---|---|
| `source()` | the source, the sanitised UTF-8; empty if nothing was loaded |
| `source_length()` | its bytes |
| `source_slice(first, last, stat, errmsg)` | bytes `first` to `last`; empty for `last = first - 1`; a range outside the source is an error |
| `lines_number()` | the lines |
| `line(i, stat, errmsg)` | line `i` without its ending |
| `line_first(i, stat, errmsg)`, `line_last(i, stat, errmsg)` | its first and last byte, the ending excluded: `line_last(i) = line_first(i) - 1` for an empty line |
| `line_ending(i, stat, errmsg)` | its ending: LF, CR, CRLF, or empty for a last line without one |

Lines are numbered from 1; a line number outside 1 to `lines_number()` gives `BARUCH_ERR_ARGUMENT` and the default
result (an empty text, 0).

## Life cycle

- `initialize` resets the tree and **keeps** the source; `destroy` frees both.
- Assignment copies the source and its line index with the rest (a deep copy).
- `compact` does not touch the source.

## Errors

| Code | |
|---|---|
| `BARUCH_ERR_IO` | `load_file`: the file cannot be opened or read (missing, unreadable, larger than 2<sup>31</sup>-1 bytes); the message carries the processor's |
| `BARUCH_ERR_ARGUMENT` | an encoding StringiFor does not support; a line or a byte range that is not in the source |
| `BARUCH_ERR_ALLOCATION` | the document cannot be allocated |

The recipe [Read a file in another encoding](/manual/cookbook#read-a-file-in-another-encoding) shows a load in use.
