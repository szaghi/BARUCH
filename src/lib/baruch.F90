!< BARUCH, Bidirectional AST Reader and Unparser for CommonMark, Hookable: the library's entry point.
module baruch
!< BARUCH, Bidirectional AST Reader and Unparser for CommonMark, Hookable: the library's entry point.
!<
!< A client needs `use baruch` only. Exported so far: the document (`markdown_document`, flat node arrays, a string
!< pool, and the sanitised source it is loaded from with its line index), the node kinds with the slot layout and
!< style values (`NODE_*`, `SLOT_*`, `STRING_*`, ...), and the status codes (`BARUCH_OK`, `BARUCH_ERR_*`). The parser,
!< the renderer, the writers and the extensions are added behind this module, step by step.
!<
!< Kinds come from PENF and text from StringiFor, used here so that the library archive holds them; neither is
!< re-exported, a client uses them directly.
use penf,               only : I4P
use stringifor,         only : string
use baruch_document,    only : markdown_document
use baruch_kinds_nodes, only : NODE_NONE,                                                                          &
                               NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_CODE_BLOCK,             &
                               NODE_HTML_BLOCK, NODE_PARAGRAPH, NODE_HEADING, NODE_THEMATIC_BREAK,                 &
                               NODE_LINK_REFERENCE_DEFINITION,                                                     &
                               NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE, NODE_EMPH,  &
                               NODE_STRONG, NODE_LINK, NODE_IMAGE,                                                 &
                               NODE_BLOCK_FIRST, NODE_BLOCK_LAST, NODE_INLINE_FIRST, NODE_INLINE_LAST,             &
                               NODE_EXTENSION_FIRST,                                                               &
                               NODE_INT_SLOTS, NODE_STRING_SLOTS,                                                  &
                               SLOT_HEADING_LEVEL, SLOT_HEADING_FORM, SLOT_HEADING_CLOSING,                        &
                               SLOT_LIST_TYPE, SLOT_LIST_START, SLOT_LIST_TIGHT, SLOT_LIST_MARKER,                 &
                               SLOT_CODE_FORM, SLOT_CODE_FENCE_CHAR, SLOT_CODE_FENCE_LENGTH,                       &
                               SLOT_BREAK_CHAR, SLOT_EMPHASIS_CHAR, SLOT_LINK_FORM,                                &
                               STRING_LITERAL, STRING_INFO, STRING_DESTINATION, STRING_TITLE, STRING_LABEL,        &
                               HEADING_ATX, HEADING_SETEXT, LIST_BULLET, LIST_ORDERED, CODE_INDENTED, CODE_FENCED, &
                               LINK_INLINE, LINK_FULL, LINK_COLLAPSED, LINK_SHORTCUT, LINK_AUTOLINK
use baruch_status,      only : BARUCH_OK, BARUCH_ERR_ARGUMENT, BARUCH_ERR_NODE, BARUCH_ERR_KIND, BARUCH_ERR_LINK, &
                               BARUCH_ERR_ALLOCATION, BARUCH_ERR_IO
implicit none
private
! the document
public :: markdown_document
! node kinds
public :: NODE_NONE
public :: NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_CODE_BLOCK, NODE_HTML_BLOCK, NODE_PARAGRAPH
public :: NODE_HEADING, NODE_THEMATIC_BREAK, NODE_LINK_REFERENCE_DEFINITION
public :: NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE, NODE_EMPH, NODE_STRONG, NODE_LINK
public :: NODE_IMAGE
public :: NODE_BLOCK_FIRST, NODE_BLOCK_LAST, NODE_INLINE_FIRST, NODE_INLINE_LAST, NODE_EXTENSION_FIRST
! slot layout and values
public :: NODE_INT_SLOTS, NODE_STRING_SLOTS
public :: SLOT_HEADING_LEVEL, SLOT_HEADING_FORM, SLOT_HEADING_CLOSING
public :: SLOT_LIST_TYPE, SLOT_LIST_START, SLOT_LIST_TIGHT, SLOT_LIST_MARKER
public :: SLOT_CODE_FORM, SLOT_CODE_FENCE_CHAR, SLOT_CODE_FENCE_LENGTH
public :: SLOT_BREAK_CHAR, SLOT_EMPHASIS_CHAR, SLOT_LINK_FORM
public :: STRING_LITERAL, STRING_INFO, STRING_DESTINATION, STRING_TITLE, STRING_LABEL
public :: HEADING_ATX, HEADING_SETEXT, LIST_BULLET, LIST_ORDERED, CODE_INDENTED, CODE_FENCED
public :: LINK_INLINE, LINK_FULL, LINK_COLLAPSED, LINK_SHORTCUT, LINK_AUTOLINK
! status codes
public :: BARUCH_OK, BARUCH_ERR_ARGUMENT, BARUCH_ERR_NODE, BARUCH_ERR_KIND, BARUCH_ERR_LINK, BARUCH_ERR_ALLOCATION
public :: BARUCH_ERR_IO
endmodule baruch
