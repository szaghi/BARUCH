!< BARUCH, the node kinds: the CommonMark block and inline kinds, the slot layout, the style values.
module baruch_kinds_nodes
!< BARUCH, the node kinds: the CommonMark block and inline kinds, the slot layout, the style values.
!<
!< A node's kind is an integer. The core kinds, one per CommonMark node type, are fixed numbers below
!< `NODE_EXTENSION_FIRST`; an extension registers its own kinds in the document, at `NODE_EXTENSION_FIRST` or above.
!< `NODE_NONE` marks a tombstone (a node unlinked and not yet compacted away).
!<
!< Every node has `NODE_INT_SLOTS` integer slots and `NODE_STRING_SLOTS` string slots, zero (or empty) until set. What
!< a slot means depends on the kind: the `SLOT_*` and `STRING_*` constants name them. Integer slots hold the meaning
!< first (`node_semantic_slots` of them: what equality compares) and the recorded source style after (what the writer
!< reuses); a style slot left at 0 means "not recorded": the writer is canonical there. Characters are stored as their
!< ASCII code (`iachar`). Slots no constant names are free for the parser and for extension kinds.
use penf, only : I4P
implicit none
private
public :: node_kind_name, node_semantic_slots, is_core_kind, is_core_block_kind, is_core_inline_kind
! kinds
public :: NODE_NONE
public :: NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_CODE_BLOCK, NODE_HTML_BLOCK, NODE_PARAGRAPH
public :: NODE_HEADING, NODE_THEMATIC_BREAK, NODE_LINK_REFERENCE_DEFINITION
public :: NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE, NODE_EMPH, NODE_STRONG, NODE_LINK
public :: NODE_IMAGE
public :: NODE_BLOCK_FIRST, NODE_BLOCK_LAST, NODE_INLINE_FIRST, NODE_INLINE_LAST, NODE_EXTENSION_FIRST
! slot layout
public :: NODE_INT_SLOTS, NODE_STRING_SLOTS
public :: SLOT_HEADING_LEVEL, SLOT_HEADING_FORM, SLOT_HEADING_CLOSING
public :: SLOT_LIST_TYPE, SLOT_LIST_START, SLOT_LIST_TIGHT, SLOT_LIST_MARKER
public :: SLOT_CODE_FORM, SLOT_CODE_FENCE_CHAR, SLOT_CODE_FENCE_LENGTH
public :: SLOT_BREAK_CHAR
public :: SLOT_EMPHASIS_CHAR
public :: SLOT_LINK_FORM
public :: SLOT_LINEBREAK_FORM, SLOT_LINEBREAK_SPACES
public :: SLOT_CODE_TICKS
public :: STRING_LITERAL, STRING_INFO, STRING_DESTINATION, STRING_TITLE, STRING_LABEL, STRING_RAW
! slot values
public :: HEADING_ATX, HEADING_SETEXT
public :: LIST_BULLET, LIST_ORDERED
public :: CODE_INDENTED, CODE_FENCED
public :: LINK_INLINE, LINK_FULL, LINK_COLLAPSED, LINK_SHORTCUT, LINK_AUTOLINK
public :: LINEBREAK_BACKSLASH, LINEBREAK_SPACES

integer(I4P), parameter :: NODE_NONE = 0_I4P !< A tombstone: an unlinked node, until compaction.

! blocks
integer(I4P), parameter :: NODE_DOCUMENT                  = 1_I4P  !< The document, the root.
integer(I4P), parameter :: NODE_BLOCK_QUOTE               = 2_I4P  !< Block quote.
integer(I4P), parameter :: NODE_LIST                      = 3_I4P  !< List, bullet or ordered.
integer(I4P), parameter :: NODE_ITEM                      = 4_I4P  !< List item.
integer(I4P), parameter :: NODE_CODE_BLOCK                = 5_I4P  !< Code block, indented or fenced.
integer(I4P), parameter :: NODE_HTML_BLOCK                = 6_I4P  !< HTML block.
integer(I4P), parameter :: NODE_PARAGRAPH                 = 7_I4P  !< Paragraph.
integer(I4P), parameter :: NODE_HEADING                   = 8_I4P  !< Heading, ATX or setext.
integer(I4P), parameter :: NODE_THEMATIC_BREAK            = 9_I4P  !< Thematic break.
integer(I4P), parameter :: NODE_LINK_REFERENCE_DEFINITION = 10_I4P !< Link reference definition, kept in place.
! inlines
integer(I4P), parameter :: NODE_TEXT        = 11_I4P !< Text.
integer(I4P), parameter :: NODE_SOFTBREAK   = 12_I4P !< Soft line break.
integer(I4P), parameter :: NODE_LINEBREAK   = 13_I4P !< Hard line break.
integer(I4P), parameter :: NODE_CODE        = 14_I4P !< Code span.
integer(I4P), parameter :: NODE_HTML_INLINE = 15_I4P !< Raw inline HTML.
integer(I4P), parameter :: NODE_EMPH        = 16_I4P !< Emphasis.
integer(I4P), parameter :: NODE_STRONG      = 17_I4P !< Strong emphasis.
integer(I4P), parameter :: NODE_LINK        = 18_I4P !< Link (autolinks too).
integer(I4P), parameter :: NODE_IMAGE       = 19_I4P !< Image.
! ranges
integer(I4P), parameter :: NODE_BLOCK_FIRST     = NODE_DOCUMENT                  !< First core block kind.
integer(I4P), parameter :: NODE_BLOCK_LAST      = NODE_LINK_REFERENCE_DEFINITION !< Last core block kind.
integer(I4P), parameter :: NODE_INLINE_FIRST    = NODE_TEXT                      !< First core inline kind.
integer(I4P), parameter :: NODE_INLINE_LAST     = NODE_IMAGE                     !< Last core inline kind.
integer(I4P), parameter :: NODE_EXTENSION_FIRST = 1000_I4P !< First kind an extension may register (below: core).

! slot layout
integer(I4P), parameter :: NODE_INT_SLOTS    = 6_I4P !< Integer slots of every node.
integer(I4P), parameter :: NODE_STRING_SLOTS = 3_I4P !< String slots of every node.
! integer slots: heading
integer(I4P), parameter :: SLOT_HEADING_LEVEL     = 1_I4P !< Heading: level, 1 to 6.
integer(I4P), parameter :: SLOT_HEADING_FORM      = 2_I4P !< Heading (style): `HEADING_ATX` or `HEADING_SETEXT`.
integer(I4P), parameter :: SLOT_HEADING_CLOSING   = 3_I4P !< Heading (style): number of closing `#`s of an ATX heading.
! integer slots: list
integer(I4P), parameter :: SLOT_LIST_TYPE         = 1_I4P !< List: `LIST_BULLET` or `LIST_ORDERED`.
integer(I4P), parameter :: SLOT_LIST_START        = 2_I4P !< List: start number of an ordered list.
integer(I4P), parameter :: SLOT_LIST_TIGHT        = 3_I4P !< List: 1 tight, 0 loose.
integer(I4P), parameter :: SLOT_LIST_MARKER       = 4_I4P !< List (style): bullet char (`-+*`) or delimiter (`.)`).
! integer slots: code block
integer(I4P), parameter :: SLOT_CODE_FORM         = 1_I4P !< Code block (style): `CODE_INDENTED` or `CODE_FENCED`.
integer(I4P), parameter :: SLOT_CODE_FENCE_CHAR   = 2_I4P !< Code block (style): fence char (`` ` `` or `~`).
integer(I4P), parameter :: SLOT_CODE_FENCE_LENGTH = 3_I4P !< Code block (style): fence length (3 or more).
! integer slots: thematic break
integer(I4P), parameter :: SLOT_BREAK_CHAR        = 1_I4P !< Thematic break (style): char (`-`, `*` or `_`).
! integer slots: emphasis and strong emphasis
integer(I4P), parameter :: SLOT_EMPHASIS_CHAR     = 1_I4P !< Emph, strong (style): delimiter char (`*` or `_`).
! integer slots: link and image
integer(I4P), parameter :: SLOT_LINK_FORM         = 1_I4P !< Link, image (style): `LINK_INLINE`...`LINK_AUTOLINK`.
! integer slots: hard line break
integer(I4P), parameter :: SLOT_LINEBREAK_FORM    = 1_I4P !< Hard break (style): `LINEBREAK_BACKSLASH`, `LINEBREAK_SPACES`.
integer(I4P), parameter :: SLOT_LINEBREAK_SPACES  = 2_I4P !< Hard break (style): spaces before the line ending (2 or more).
! integer slots: code span
integer(I4P), parameter :: SLOT_CODE_TICKS        = 1_I4P !< Code span (style): backticks of its delimiters.
! string slots
integer(I4P), parameter :: STRING_LITERAL     = 1_I4P !< Text, code, HTML, code block, extension kinds: the content.
integer(I4P), parameter :: STRING_INFO        = 2_I4P !< Code block: the info string.
integer(I4P), parameter :: STRING_DESTINATION = 1_I4P !< Link, image, link reference definition: the destination.
integer(I4P), parameter :: STRING_TITLE       = 2_I4P !< Link, image, link reference definition: the title.
integer(I4P), parameter :: STRING_LABEL       = 3_I4P !< Link, image, link reference definition: the label.
integer(I4P), parameter :: STRING_RAW         = 2_I4P !< Code span (style): its content as written, when not the literal.

! slot values (0 always means "not recorded")
integer(I4P), parameter :: HEADING_ATX    = 1_I4P !< `# Heading`.
integer(I4P), parameter :: HEADING_SETEXT = 2_I4P !< Heading underlined by `===` or `---`.
integer(I4P), parameter :: LIST_BULLET    = 1_I4P !< Bullet list.
integer(I4P), parameter :: LIST_ORDERED   = 2_I4P !< Ordered list.
integer(I4P), parameter :: CODE_INDENTED  = 1_I4P !< Indented code block.
integer(I4P), parameter :: CODE_FENCED    = 2_I4P !< Fenced code block.
integer(I4P), parameter :: LINK_INLINE    = 1_I4P !< `[text](destination "title")`.
integer(I4P), parameter :: LINK_FULL      = 2_I4P !< `[text][label]`.
integer(I4P), parameter :: LINK_COLLAPSED = 3_I4P !< `[label][]`.
integer(I4P), parameter :: LINK_SHORTCUT  = 4_I4P !< `[label]`.
integer(I4P), parameter :: LINK_AUTOLINK  = 5_I4P !< `<destination>`.
integer(I4P), parameter :: LINEBREAK_BACKSLASH = 1_I4P !< A backslash before the line ending.
integer(I4P), parameter :: LINEBREAK_SPACES    = 2_I4P !< Two or more spaces before the line ending.

contains
   pure function node_kind_name(kind) result(name)
   !< The name of a core kind (CommonMark's XML element names, `block_quote` style); empty for any other number.
   integer(I4P), intent(in)  :: kind !< Kind.
   character(:), allocatable :: name !< Name.

   select case(kind)
   case(NODE_DOCUMENT)                  ; name = 'document'
   case(NODE_BLOCK_QUOTE)               ; name = 'block_quote'
   case(NODE_LIST)                      ; name = 'list'
   case(NODE_ITEM)                      ; name = 'item'
   case(NODE_CODE_BLOCK)                ; name = 'code_block'
   case(NODE_HTML_BLOCK)                ; name = 'html_block'
   case(NODE_PARAGRAPH)                 ; name = 'paragraph'
   case(NODE_HEADING)                   ; name = 'heading'
   case(NODE_THEMATIC_BREAK)            ; name = 'thematic_break'
   case(NODE_LINK_REFERENCE_DEFINITION) ; name = 'link_reference_definition'
   case(NODE_TEXT)                      ; name = 'text'
   case(NODE_SOFTBREAK)                 ; name = 'softbreak'
   case(NODE_LINEBREAK)                 ; name = 'linebreak'
   case(NODE_CODE)                      ; name = 'code'
   case(NODE_HTML_INLINE)               ; name = 'html_inline'
   case(NODE_EMPH)                      ; name = 'emph'
   case(NODE_STRONG)                    ; name = 'strong'
   case(NODE_LINK)                      ; name = 'link'
   case(NODE_IMAGE)                     ; name = 'image'
   case default                         ; name = ''
   endselect
   endfunction node_kind_name

   elemental function node_semantic_slots(kind) result(slots)
   !< The number of leading integer slots that hold meaning (not style) for a core kind: what equality compares.
   integer(I4P), intent(in) :: kind  !< Kind.
   integer(I4P)             :: slots !< Semantic slots.

   select case(kind)
   case(NODE_HEADING) ; slots = 1_I4P ! level
   case(NODE_LIST)    ; slots = 3_I4P ! type, start, tight
   case default       ; slots = 0_I4P
   endselect
   endfunction node_semantic_slots

   elemental function is_core_kind(kind) result(is)
   !< True for a core (CommonMark) kind.
   integer(I4P), intent(in) :: kind !< Kind.
   logical                  :: is   !< Result.

   is = is_core_block_kind(kind) .or. is_core_inline_kind(kind)
   endfunction is_core_kind

   elemental function is_core_block_kind(kind) result(is)
   !< True for a core block kind.
   integer(I4P), intent(in) :: kind !< Kind.
   logical                  :: is   !< Result.

   is = kind >= NODE_BLOCK_FIRST .and. kind <= NODE_BLOCK_LAST
   endfunction is_core_block_kind

   elemental function is_core_inline_kind(kind) result(is)
   !< True for a core inline kind.
   integer(I4P), intent(in) :: kind !< Kind.
   logical                  :: is   !< Result.

   is = kind >= NODE_INLINE_FIRST .and. kind <= NODE_INLINE_LAST
   endfunction is_core_inline_kind
endmodule baruch_kinds_nodes
