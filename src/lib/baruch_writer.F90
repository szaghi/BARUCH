!< BARUCH, the tree writer: a document's tree as Markdown, in the style the parser recorded or in a canonical one.
module baruch_writer
!< BARUCH, the tree writer: a document's tree as Markdown, in the style the parser recorded or in a canonical one.
!<
!< The writer is the inverse of the parser (settled design 2 of the plan): what it writes, the parser reads back to the
!< same tree (`markdown_document%equal`), not to the same bytes. It reuses the **style slots** the parser recorded
!< (`baruch_kinds_nodes`: emphasis character, bullet, ordered delimiter, ATX or setext and the closing `#`s, fence
!< character and length, thematic break character, link form and label, hard break form) and falls back to the
!< **canonical style** where a slot holds nothing, or everywhere when asked (`canonical`): ATX headings (setext only
!< for a heading of level 1 or 2 spanning lines), `-` bullets, `1.` numbering, backtick fences, `***`, `*` and `**`,
!< inline links (autolinks when the link is one), backslash hard breaks.
!<
!< **Escaping** is minimal and decided by the parser's rules (settled design 3): a character is escaped only where
!< reading it back would change the tree, and how is decided by its context:
!<
!< - at the start of a line of a paragraph or of a setext heading, what would open a block there (`#`, `>`, a bullet or
!<   `1.`/`1)` and a space, a fence, `=` or `-` under a paragraph, `<` and an HTML block start, a thematic break): the
!<   block scanners of `baruch_blocks_scan` are asked; leading white space (it would be stripped, or indent a code
!<   block) and trailing white space (stripped, or a hard break) are written as character references;
!< - inline: `\` before ASCII punctuation or a line end, `*` and `_` runs that could open or close emphasis (the
!<   flanking rules, the characters around them as they are written; a letter touching an emphasis's delimiter that
!<   would not flank otherwise is written as a numeric character reference), backticks when another backtick follows
!<   in the paragraph, `[` and `]` that could make a link (a pair followed by `(`, `[` or `:`, or whose content is a
!<   defined label; a lone one inside a link's text, or after a lone `[`), `!` before a link, `&` before a character
!<   reference, `<` before anything but white space, the trigger and delimiter characters of the enabled extensions;
!< - a code span takes the shortest backtick string its content does not hold, padded with a space when the content
!<   starts or ends with a backtick or with spaces at both ends; a fence is longer than any fence-like line inside;
!< - destinations go between `<` and `>` when they hold white space, titles between `"` (or `'`), each with their own
!<   escapes; raw HTML and titles spanning lines indent a continuation line that would open a block by 4 spaces (a
!<   paragraph's continuation lines lose their indentation).
!<
!< **Emphasis** delimiters are chosen by the same rules (recorded, else `*`; the other character where touching
!< delimiters would pair otherwise), and the delimiters written are then read back with the parser's algorithm
!< (`emphasis_reads_back`); when they would not read back, a bounded search of other choices runs (texts' runs escaped
!< or joined, other characters), one pass over the block's content per choice.
!<
!< **Containers** are line prefixes, kept on a stack: `> ` for a block quote, the item's marker on its first line and
!< as many spaces after it; a blank line between blocks (none between the items and the blocks of a tight list, unless
!< the second block could not follow the first without merging). The walk is iterative, over the tree's links: no
!< recursion, any depth. Link reference definitions are written in place; a reference link keeps its form when the
!< definition it would find is the same, else it is written inline.
!<
!< **Extensions** write their nodes through their mandatory `write_markdown` hook, which the writer offers every node
!< on entering and on leaving it (core kinds too, in registration order; the first that handles the event writes it).
!< For a block, the hook's text is written as lines behind the containers' prefixes; on entering a block that has
!< children, the text after its last line feed is the prefix of every line of the children (`!> ` for a container
!< like a block quote, empty for a fenced one like `::: tip`). For an inline node it is written as it is, before and
!< after the children. A node of an extension kind that no enabled extension writes is written transparently: its
!< children as if it were not there, a leaf's literal as text.
use penf,                only : I4P
use baruch_document,     only : markdown_document
use baruch_extension,    only : extension_slot
use baruch_kinds_nodes,  only : NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_CODE_BLOCK, NODE_HTML_BLOCK, &
                                NODE_PARAGRAPH, NODE_HEADING, NODE_THEMATIC_BREAK, NODE_LINK_REFERENCE_DEFINITION,      &
                                NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE, NODE_EMPH,      &
                                NODE_STRONG, NODE_LINK, NODE_IMAGE,                                                     &
                                SLOT_HEADING_FORM, SLOT_HEADING_CLOSING, SLOT_LIST_MARKER, SLOT_CODE_FORM,              &
                                SLOT_CODE_FENCE_CHAR, SLOT_CODE_FENCE_LENGTH, SLOT_BREAK_CHAR, SLOT_EMPHASIS_CHAR,      &
                                SLOT_LINK_FORM, SLOT_LINEBREAK_FORM, SLOT_LINEBREAK_SPACES, SLOT_CODE_TICKS, STRING_RAW, &
                                NODE_EXTENSION_FIRST, HEADING_SETEXT, LIST_ORDERED, CODE_INDENTED, LINK_FULL,           &
                                LINK_COLLAPSED, LINK_SHORTCUT, LINK_AUTOLINK, LINEBREAK_SPACES
use baruch_blocks_scan,  only : BACKSLASH, is_space_or_tab, is_ascii_punctuation, scan_thematic_break, scan_atx_start, &
                                scan_open_fence, scan_close_fence, scan_setext_underline, scan_list_marker,             &
                                scan_html_block_start
use baruch_inlines_scan, only : is_ascii_alnum, is_ascii_space, classify, scan_reference, decode_references, &
                                scan_autolink_uri, scan_autolink_email, scan_html_tag, code_content, unescape_string
use baruch_references,   only : reference_map
use baruch_blocks_definitions, only : scan_destination, scan_title, parse_definition, LABEL_MAX
use baruch_status
implicit none
private
public :: write_markdown_document
public :: escape_markdown

character, parameter :: LF  = achar(10) !< Line feed.
character, parameter :: CR  = achar(13) !< Carriage return.
character, parameter :: TAB = achar(9)  !< Tab.

! what follows a text run, for its escaping
integer(I4P), parameter :: NEXT_END   = 0_I4P !< The end of the block's inline content.
integer(I4P), parameter :: NEXT_LINE  = 1_I4P !< A line ending (a soft or hard line break).
integer(I4P), parameter :: NEXT_LINK  = 2_I4P !< A link (its `[`).
integer(I4P), parameter :: NEXT_OPEN  = 3_I4P !< The opening delimiter of an emphasis.
integer(I4P), parameter :: EDGE_SPACE  = 1_I4P !< A character, written: white space, or none (a line's ends).
integer(I4P), parameter :: EDGE_PUNCT  = 2_I4P !< A character, written: punctuation.
integer(I4P), parameter :: EDGE_LETTER = 3_I4P !< A character, written: neither (CommonMark's flanking rules).
integer(I4P), parameter :: NEXT_CLOSE = 4_I4P !< The closing delimiter of the emphasis the run ends.
integer(I4P), parameter :: NEXT_OTHER = 5_I4P !< Anything else.
! how the inline content of a block is written
integer(I4P), parameter :: INLINE_LINES = 1_I4P !< Lines of a paragraph or of a setext heading: line starts escaped.
integer(I4P), parameter :: INLINE_ATX   = 2_I4P !< One line after `# `: no line start.
integer(I4P), parameter :: INLINE_PLAIN = 3_I4P !< Behind an extension's prefix: no line start checked.

type :: md_buffer
   !< Output buffer, grown by doubling.
   character(:), allocatable :: chars        !< Bytes, `chars(1:used)` written.
   integer(I4P)              :: used = 0_I4P !< Bytes written.
   contains
      procedure, pass(self) :: put  !< Append bytes.
      procedure, pass(self) :: last_char !< Return the last UTF-8 character written, empty if none.
endtype md_buffer

type :: md_writer
   !< The state of one write: the output, the container prefixes, the choices made per node.
   logical                   :: canonical = .false.    !< Ignore the recorded style.
   type(md_buffer)           :: out                    !< Output.
   type(reference_map)       :: refs                   !< The document's definitions, by label.
   character(:), allocatable :: ext_chars              !< Trigger and delimiter characters of the enabled extensions.
   ! container prefixes: frame k writes first(k) on the next line once, rest(k) on the others
   character(:), allocatable :: first_buf              !< First-line prefixes, concatenated.
   character(:), allocatable :: rest_buf               !< Other-line prefixes, concatenated.
   integer(I4P), allocatable :: first_lo(:), first_hi(:) !< Bounds of each frame's first-line prefix.
   integer(I4P), allocatable :: rest_lo(:), rest_hi(:)   !< Bounds of each frame's other-line prefix.
   integer(I4P), allocatable :: bullet(:)              !< The bullet of an item frame (`iachar`), 0 otherwise.
   logical,      allocatable :: pending(:)             !< The frame's first line is still to write.
   integer(I4P)              :: depth = 0_I4P          !< Frames.
   integer(I4P)              :: pendings = 0_I4P       !< Frames whose first line is pending.
   ! per node
   integer(I4P), allocatable :: mark(:)                !< List: marker; emphasis: delimiter; link: 1 if an autolink.
   integer(I4P), allocatable :: number(:)              !< List: next item number.
   integer(I4P), allocatable :: by_ext(:)              !< Entered by an extension: 1, and a frame pushed: 2.
   ! between blocks
   logical                   :: glued = .false.        !< The current block follows an open paragraph, no blank line.
endtype md_writer

type :: inline_state
   !< The state of the inline content of one block.
   type(md_buffer)           :: buf                    !< The content, lines joined by LF, no prefix.
   integer(I4P)              :: mode = INLINE_LINES    !< `INLINE_LINES`, `INLINE_ATX` or `INLINE_PLAIN`.
   logical                   :: line_start = .true.    !< Nothing written on the current line yet.
   logical                   :: first_line = .true.    !< The current line is the block's first.
   integer(I4P)              :: ticks = 0_I4P          !< Backticks still to be written in the block.
   integer(I4P), allocatable :: lone(:)                !< Buffer position of each active `[` written as text.
   integer(I4P)              :: lones = 0_I4P          !< Active `[` written as text, outside link texts.
   integer(I4P)              :: depth = 0_I4P          !< Link and image texts open.
   integer(I4P)              :: delim_end = -1_I4P     !< Buffer position of the last emphasis delimiter written.
   logical                   :: delim_open = .false.   !< That delimiter opened its emphasis.
   integer(I4P), allocatable :: link_start(:)          !< Buffer position of each open link text's `[`.
   integer(I4P)              :: openers(2) = 0_I4P     !< Runs of `*`, `_` written unescaped that can open.
   integer(I4P)              :: inside(2) = 0_I4P      !< Emphases delimited by `*`, `_` open around the position.
   integer(I4P), allocatable :: saved(:,:)             !< `openers` and `inside` outside each open link text.
   integer(I4P), allocatable :: emph(:,:)              !< Each open emphasis: its delimiter, `openers` at its start.
   integer(I4P)              :: emphs = 0_I4P          !< Open emphases.
   logical                   :: both_open(2) = .false. !< An emphasis of `*`, `_` opens with a run that can close.
   integer(I4P), allocatable :: delims(:,:)            !< Delimiters written: position, length, `iachar`, emphasis
                                                       !< (0: a text's run), link text (0: none).
   integer(I4P)              :: ndelims = 0_I4P        !< Delimiters written.
   integer(I4P), allocatable :: seg(:)                 !< Number of each open link text.
   integer(I4P)              :: segs = 0_I4P           !< Link texts opened.
   logical                   :: escape_first = .false. !< Escape a `[` starting the content (a definition otherwise).
   logical                   :: paranoid = .false.     !< Escape every text's run of `*`, `_` that can open or close.
   logical                   :: merge_runs = .false.   !< Leave a text's run touching a delimiter of its character.
   integer(I4P)              :: indent = 0_I4P         !< Spaces to write before the next line's first byte.
endtype inline_state

contains
   subroutine write_markdown_document(doc, slots, canonical, markdown, ierr, msg)
   !< Write a document's tree as Markdown with the enabled extensions of `slots`: the recorded style, or the canonical
   !< one. `ierr` (0 on success) and `msg` report an extension hook failing; `markdown` then holds what was written
   !< before it.
   type(markdown_document),   intent(in)    :: doc       !< Document.
   type(extension_slot),      intent(in)    :: slots(:)  !< Extensions of the options.
   logical,                   intent(in)    :: canonical !< Ignore the recorded style.
   character(:), allocatable, intent(out)   :: markdown  !< Markdown.
   integer(I4P),              intent(out)   :: ierr      !< Status.
   character(:), allocatable, intent(inout) :: msg       !< Message, set with a nonzero `ierr`.
   type(md_writer)                          :: w         !< Writer.
   integer(I4P)                             :: root      !< Root.
   integer(I4P)                             :: node      !< Current node.
   integer(I4P)                             :: last      !< Last node of a run of inlines at block level.
   integer(I4P)                             :: s         !< Counter.
   logical                                  :: descend   !< Visit the children.

   ierr = BARUCH_OK
   markdown = ''
   root = doc%root()
   if (root == 0_I4P) return
   w%canonical = canonical
   allocate(character(max(1024_I4P, 2_I4P * doc%source_length())) :: w%out%chars)
   allocate(character(256) :: w%first_buf, w%rest_buf)
   allocate(w%first_lo(16), w%first_hi(16), w%rest_lo(16), w%rest_hi(16), w%bullet(16), w%pending(16))
   allocate(w%mark(doc%last_node()), w%number(doc%last_node()), w%by_ext(doc%last_node()))
   w%mark = 0_I4P
   w%number = 0_I4P
   w%by_ext = 0_I4P
   call w%refs%build(doc)
   w%ext_chars = ''
   do s=1, size(slots, kind=I4P)
      if (.not. slots(s)%enabled) cycle
      w%ext_chars = w%ext_chars//slots(s)%ext%inline_triggers()//slots(s)%ext%delimiter_chars()
   enddo
   node = doc%first_child(root)
   do while (node /= 0_I4P)
      if (doc%kind_is_inline(doc%node_kind(node))) then ! inlines at block level (a tree built by hand): a paragraph
         last = node
         do while (doc%next_sibling(last) /= 0_I4P)
            if (.not. doc%kind_is_inline(doc%node_kind(doc%next_sibling(last)))) exit
            last = doc%next_sibling(last)
         enddo
         if (doc%prev_sibling(node) /= 0_I4P) call separate(w, doc, doc%prev_sibling(node), node)
         call write_inline_lines(w, doc, slots, doc%parent(node), node, last, INLINE_LINES, '', ierr, msg)
         node = last
         descend = .false.
      else
         call enter_block(w, doc, slots, node, descend, ierr, msg)
      endif
      if (ierr /= BARUCH_OK) exit
      if (descend .and. doc%first_child(node) /= 0_I4P) then
         node = doc%first_child(node)
         cycle
      endif
      do
         if (.not. doc%kind_is_inline(doc%node_kind(node))) call leave_block(w, doc, slots, node, ierr, msg)
         if (ierr /= BARUCH_OK) exit
         if (doc%next_sibling(node) /= 0_I4P) then
            node = doc%next_sibling(node)
            exit
         endif
         node = doc%parent(node)
         if (node == root .or. node == 0_I4P) then
            node = 0_I4P
            exit
         endif
      enddo
      if (ierr /= BARUCH_OK) exit
   enddo
   if (ierr == BARUCH_OK .and. .not. canonical) call source_end(w, doc)
   markdown = w%out%chars(1:w%out%used)
   endsubroutine write_markdown_document

   subroutine source_end(w, doc)
   !< End the output as the source ends (recorded style): the blank lines after its last block, or no line ending after
   !< it.
   type(md_writer),         intent(inout) :: w    !< Writer.
   type(markdown_document), intent(in)    :: doc  !< Document.
   character(:), allocatable              :: tail !< The source after the last block.
   integer(I4P)                           :: last !< Last block of the document.
   integer(I4P)                           :: ends !< Line endings in the tail.
   integer(I4P)                           :: k    !< Position.

   last = doc%last_child(doc%root())
   if (last == 0_I4P .or. w%out%used == 0_I4P) return
   if (ends_unclosed_html(doc, last)) return ! its literal holds the blank lines after it
   if (doc%span_last(last) < 1_I4P .or. doc%span_last(last) > doc%source_length()) return
   tail = doc%source_slice(doc%span_last(last) + 1_I4P, doc%source_length())
   if (verify(tail, ' '//TAB//LF//CR) > 0) return ! the last block's span stops before its end (a fence left open)
   ends = 0_I4P
   do k=1, len(tail, kind=I4P)
      if (tail(k:k) == LF) then
         ends = ends + 1_I4P
      elseif (tail(k:k) == CR) then
         ends = ends + 1_I4P
         if (k < len(tail, kind=I4P)) then
            if (tail(k+1:k+1) == LF) ends = ends - 1_I4P
         endif
      endif
   enddo
   if (ends == 0_I4P) then
      if (w%out%chars(w%out%used:w%out%used) == LF) w%out%used = w%out%used - 1_I4P
   else
      do k=2, ends
         call w%out%put(LF)
      enddo
   endif
   endsubroutine source_end

   function escape_markdown(text, line_start, first_line) result(escaped)
   !< Return a text escaped so that, written inside a paragraph, it reads back as that text, whatever is written
   !< around it: as the writer escapes a text node, its neighbours unknown (the most cautious reading of each rule). With
   !< `line_start` (default false) the text starts a line, the first of a paragraph with `first_line` (default true):
   !< what would open a block there is escaped too. For the streaming writer and for clients composing Markdown.
   character(*), intent(in)           :: text       !< Text.
   logical,      intent(in), optional :: line_start !< The text starts a line.
   logical,      intent(in), optional :: first_line !< That line is a paragraph's first.
   character(:), allocatable          :: escaped    !< Escaped text.
   type(md_writer)                    :: w          !< A writer with nothing defined.
   type(inline_state)                 :: s          !< Inline state.

   allocate(character(max(64, 2 * len(text))) :: s%buf%chars)
   w%ext_chars = ''
   s%mode = INLINE_LINES
   s%line_start = .false.
   if (present(line_start)) s%line_start = line_start
   s%first_line = .true.
   if (present(first_line)) s%first_line = first_line
   s%ticks = 1000000000_I4P     ! every backtick escaped
   s%depth = 1_I4P               ! every lone bracket escaped
   s%inside = 1_I4P              ! every run that can open or close emphasis escaped
   allocate(s%lone(1), s%emph(3, 1))
   s%emphs = 1_I4P
   s%emph(:, 1) = [0_I4P, 0_I4P, 0_I4P]
   call escape_text(w, s, text, NEXT_OTHER, 'a', .false.)
   escaped = s%buf%chars(1:s%buf%used)
   endfunction escape_markdown

   ! blocks
   subroutine enter_block(w, doc, slots, node, descend, ierr, msg)
   !< Write what comes before a block's children (all of a leaf block); `descend` tells whether its children are
   !< blocks still to visit.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Block.
   logical,                   intent(out)   :: descend  !< Visit its children.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< An extension's Markdown.
   character(:), allocatable                :: marker   !< An item's marker.
   integer(I4P)                             :: list     !< An item's list.
   integer(I4P)                             :: indent   !< Spaces before the block's first line.
   logical                                  :: handled  !< An extension wrote the event.

   descend = .false.
   w%glued = .false.
   if (doc%prev_sibling(node) /= 0_I4P) call separate(w, doc, doc%prev_sibling(node), node)
   call offer(doc, slots, node, .true., piece, handled, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (handled) then
      w%by_ext(node) = 1_I4P
      call block_piece(w, doc, slots, node, piece, descend, ierr, msg)
      return
   endif
   indent = source_indent(w, doc, node)
   select case(doc%node_kind(node))
   case(NODE_DOCUMENT)
      descend = .true.
   case(NODE_BLOCK_QUOTE)
      call push(w, repeat(' ', indent)//'> ', '> ', 0_I4P)
      descend = .true.
   case(NODE_LIST)
      call choose_list_marker(w, doc, node)
      descend = .true.
   case(NODE_ITEM)
      list = doc%parent(node)
      if (doc%list_type(list) == LIST_ORDERED) then
         marker = istr(w%number(list))//char1(w%mark(list))
         w%number(list) = w%number(list) + 1_I4P
      else
         marker = char1(w%mark(list))
      endif
      marker = repeat(' ', indent)//marker//repeat(' ', item_padding(w, doc, node, len(marker, kind=I4P)))
      call push(w, marker, repeat(' ', len(marker)), merge(0_I4P, w%mark(list), doc%list_type(list) == LIST_ORDERED))
      descend = .true.
   case(NODE_PARAGRAPH)
      ! after a definition, a line starting with raw HTML that could open an HTML block: indented by 4, a continuation
      ! line of the definition's paragraph, which loses the indentation
      if (glued_html(w, doc, node)) indent = 4_I4P
      call push(w, repeat(' ', indent), '', 0_I4P)
      call write_inline_lines(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_LINES, '', &
                              ierr, msg)
      call pop(w)
   case(NODE_HEADING)
      if (glued_html(w, doc, node)) indent = 4_I4P ! as a paragraph's, below
      call push(w, repeat(' ', indent), '', 0_I4P)
      call write_heading(w, doc, slots, node, ierr, msg)
      call pop(w)
   case(NODE_CODE_BLOCK)
      call write_code_block(w, doc, node)
   case(NODE_HTML_BLOCK)
      if (doc%node_kind(doc%parent(node)) == NODE_ITEM .and. doc%prev_sibling(node) == 0_I4P .and. &
          index(doc%literal(node), ' ') == 1) call emit_line(w, '') ! its indentation, behind the marker, would be lost
      call emit_text(w, doc%literal(node), .false.)
   case(NODE_THEMATIC_BREAK)
      call emit_line(w, repeat(' ', indent)//break_line(w, doc, node))
   case(NODE_LINK_REFERENCE_DEFINITION)
      call emit_text(w, repeat(' ', indent)//'['//doc%label(node)//']: '//destination_text(doc%destination(node), &
                     .true.)//title_text(doc%title(node)), .true.)
   case default ! an extension kind no extension writes: transparent
      if (doc%first_child(node) == 0_I4P) then
         if (len(doc%literal(node)) > 0) call emit_text(w, escape_markdown(doc%literal(node), .true.), .true.)
      elseif (doc%kind_is_inline(doc%node_kind(doc%first_child(node)))) then
         call write_inline_lines(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_LINES, &
                                 '', ierr, msg)
      else
         descend = .true.
      endif
   endselect
   endsubroutine enter_block

   function line_indent(w, doc, block, node) result(indent)
   !< Return the spaces before an inline node that starts a line of a paragraph or heading of the document's own, in
   !< the source (recorded style: a continuation line's indentation, which the parser strips); 0 otherwise.
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: block  !< The paragraph or heading.
   integer(I4P),            intent(in) :: node   !< The node after a line break.
   integer(I4P)                        :: indent !< Spaces.
   integer(I4P)                        :: k      !< Position.

   indent = 0_I4P
   if (w%canonical .or. node == 0_I4P) return
   if (doc%parent(block) /= doc%root()) return
   k = doc%span_first(node) - 1_I4P
   if (k < 1_I4P .or. k > doc%source_length()) return
   do while (k >= 1_I4P)
      if (doc%source_slice(k, k) /= ' ') exit
      k = k - 1_I4P
   enddo
   if (k < 1_I4P) return
   if (doc%source_slice(k, k) /= LF .and. doc%source_slice(k, k) /= CR) return
   indent = doc%span_first(node) - 1_I4P - k
   endfunction line_indent

   function source_indent(w, doc, node) result(indent)
   !< Return the spaces before a block's first line in the source (recorded style), 0 to 3, for a block of the
   !< document's own (a list's items for a list of the document's): a paragraph, a heading, a thematic break, a
   !< definition, a block quote, an item. 0 otherwise, and 0 for a block after a list that so indented would be read
   !< in the list's last item (its marker's padding written differently).
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: node   !< Block.
   integer(I4P)                        :: indent !< Spaces.
   integer(I4P)                        :: list   !< A list before the block.
   integer(I4P)                        :: item   !< Its last item.
   integer(I4P)                        :: width  !< Its marker's bytes.

   indent = raw_indent(w, doc, node)
   if (indent > 0_I4P .and. doc%node_kind(node) /= NODE_ITEM .and. doc%prev_sibling(node) /= 0_I4P) then
      list = doc%prev_sibling(node)
      if (doc%node_kind(list) == NODE_LIST .and. doc%last_child(list) /= 0_I4P) then ! not into its last item
         item = doc%last_child(list)
         if (doc%first_child(item) /= 0_I4P) then ! (an empty item ends at the blank line after it)
            width = 1_I4P
            if (doc%list_type(list) == LIST_ORDERED) width = len(istr(w%number(list) - 1_I4P), kind=I4P) + 1_I4P
            if (indent >= raw_indent(w, doc, item) + width + item_padding(w, doc, item, width)) indent = 0_I4P
         endif
      endif
   endif
   endfunction source_indent

   function raw_indent(w, doc, node) result(indent)
   !< Return the spaces before a block's first line in the source (recorded style), 0 to 3, for a block of the
   !< document's own (a list's items for a list of the document's); 0 otherwise.
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: node   !< Block.
   integer(I4P)                        :: indent !< Spaces.
   integer(I4P)                        :: up     !< The document, or the list of an item.
   integer(I4P)                        :: k      !< Position.

   indent = 0_I4P
   if (w%canonical) return
   select case(doc%node_kind(node))
   case(NODE_PARAGRAPH, NODE_HEADING, NODE_THEMATIC_BREAK, NODE_LINK_REFERENCE_DEFINITION, NODE_BLOCK_QUOTE)
      up = doc%parent(node)
   case(NODE_ITEM)
      up = doc%parent(doc%parent(node))
   case default
      return
   endselect
   if (up /= doc%root()) return
   k = doc%span_first(node) - 1_I4P
   if (k < 0_I4P .or. k > doc%source_length()) return
   do while (k >= 1_I4P)
      if (doc%source_slice(k, k) /= ' ') exit
      k = k - 1_I4P
   enddo
   if (k >= 1_I4P) then
      if (doc%source_slice(k, k) /= LF .and. doc%source_slice(k, k) /= CR) return
   endif
   indent = doc%span_first(node) - 1_I4P - k
   if (indent > 3_I4P) indent = 0_I4P
   endfunction raw_indent

   function item_padding(w, doc, item, width) result(pad)
   !< Return the spaces between an item's marker (`width` bytes) and its content: as in the source (recorded style), 1
   !< to 4 columns (tabs expanded) on the marker's line before a first block that is not an indented code block or an
   !< indented HTML block; else 1. The last item of a list followed by an HTML block whose literal is indented takes
   !< more, so that the block is not read as its own.
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: item  !< Item.
   integer(I4P),            intent(in) :: width !< Bytes of its marker.
   integer(I4P)                        :: pad   !< Spaces.
   character(:), allocatable           :: gap   !< The source between marker and content.
   integer(I4P)                        :: child !< First block.
   integer(I4P)                        :: a, b  !< Bounds.
   integer(I4P)                        :: k     !< Position.
   logical                             :: indented !< The first block keeps an indentation of its own.

   pad = 1_I4P
   child = doc%first_child(item)
   if (.not. w%canonical .and. child /= 0_I4P) then
      indented = doc%node_kind(child) == NODE_CODE_BLOCK
      if (indented) indented = written_indented(w, doc, child)
      if (doc%node_kind(child) == NODE_HTML_BLOCK) indented = index(doc%literal(child), ' ') == 1
      if (.not. indented) then
         a = doc%span_first(item) + width
         b = doc%span_first(child) - 1_I4P
         if (a >= 2_I4P .and. b >= a .and. b - a + 1_I4P <= 4_I4P .and. b <= doc%source_length()) then
            gap = doc%source_slice(a, b)
            if (verify(gap, ' '//TAB) == 0) then
               k = column(doc, b + 1_I4P) - column(doc, a)
               if (k >= 1_I4P .and. k <= 4_I4P) pad = k
            endif
         endif
      endif
   endif
   k = html_after(doc, item) - raw_indent(w, doc, item)
   if (k >= width + pad) pad = k - width + 1_I4P
   endfunction item_padding

   function html_after(doc, item) result(k)
   !< Return the spaces an HTML block right after an item's list starts its literal with, when the item is the list's
   !< last (the block must not be read as the item's); 0 otherwise.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: item !< Item.
   integer(I4P)                        :: k    !< Spaces.
   integer(I4P)                        :: n    !< The block after the list.

   k = 0_I4P
   if (doc%node_kind(item) /= NODE_ITEM .or. doc%next_sibling(item) /= 0_I4P) return
   n = doc%next_sibling(doc%parent(item))
   if (n == 0_I4P) return
   if (doc%node_kind(n) /= NODE_HTML_BLOCK) return
   k = verify(doc%literal(n), ' ') - 1_I4P
   if (k < 0_I4P) k = 0_I4P
   endfunction html_after

   function column(doc, x) result(col)
   !< Return the column (from 0) of a byte of the source on its line, tabs to the next multiple of 4.
   type(markdown_document), intent(in) :: doc !< Document.
   integer(I4P),            intent(in) :: x   !< Byte.
   integer(I4P)                        :: col !< Column.
   integer(I4P)                        :: ls  !< The line's first byte.
   character(:), allocatable           :: line !< The line up to the byte.
   integer(I4P)                        :: k   !< Position.

   ls = x
   do while (ls > 1_I4P)
      if (doc%source_slice(ls - 1_I4P, ls - 1_I4P) == LF .or. doc%source_slice(ls - 1_I4P, ls - 1_I4P) == CR) exit
      ls = ls - 1_I4P
   enddo
   col = 0_I4P
   if (x <= ls) return
   line = doc%source_slice(ls, x - 1_I4P)
   do k=1_I4P, len(line, kind=I4P)
      if (line(k:k) == TAB) then
         col = (col / 4_I4P + 1_I4P) * 4_I4P
      elseif (iand(iachar(line(k:k), kind=I4P), 192_I4P) /= 128_I4P) then
         col = col + 1_I4P
      endif
   enddo
   endfunction column

   subroutine leave_block(w, doc, slots, node, ierr, msg)
   !< Write what comes after a block's children: close its container prefix (an empty block quote or item writes its
   !< marker alone), then what an extension writes on leaving it.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Block.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< An extension's Markdown.
   logical                                  :: handled  !< An extension wrote the event.

   if (w%by_ext(node) == 0_I4P) then
      select case(doc%node_kind(node))
      case(NODE_BLOCK_QUOTE, NODE_ITEM)
         if (w%pending(w%depth)) call emit_line(w, '')
         if (quote_closes(w, doc, node)) call emit_line(w, '') ! `>`: its paragraph closed, the next block no lazy line
         call pop(w)
      endselect
   elseif (w%by_ext(node) == 2_I4P) then
      call pop(w)
   endif
   call offer(doc, slots, node, .false., piece, handled, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (handled .and. len(piece) > 0) call emit_text(w, piece, .false.)
   endsubroutine leave_block

   subroutine block_piece(w, doc, slots, node, piece, descend, ierr, msg)
   !< Write an extension's Markdown for entering a block: its lines; after the last line feed, the prefix of the lines
   !< of the block's children (or, for a leaf, its last line).
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Block.
   character(*),              intent(in)    :: piece    !< The extension's Markdown.
   logical,                   intent(out)   :: descend  !< Visit its children.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: k        !< Last line feed of `piece`.

   descend = .false.
   if (doc%first_child(node) == 0_I4P) then
      if (len(piece) > 0) call emit_text(w, piece, .false.)
      return
   endif
   k = index(piece, LF, back=.true.)
   if (k > 0) call emit_text(w, piece(1:k), .false.)
   if (doc%kind_is_inline(doc%node_kind(doc%first_child(node)))) then
      call write_inline_lines(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_PLAIN, &
                              piece(k+1:), ierr, msg)
   else
      if (len(piece) > k) then
         call push(w, piece(k+1:), piece(k+1:), 0_I4P)
         w%by_ext(node) = 2_I4P
      endif
      descend = .true.
   endif
   endsubroutine block_piece

   subroutine separate(w, doc, prev, node)
   !< Write what separates a block from its previous sibling: a blank line, or nothing between the items and the
   !< blocks of a tight list (unless the block could not follow the previous one without merging with it) and between
   !< two link reference definitions.
   type(md_writer),         intent(inout) :: w     !< Writer.
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: prev  !< Previous sibling.
   integer(I4P),            intent(in)    :: node  !< Block.
   integer(I4P)                           :: up    !< Parent.
   integer(I4P)                           :: lines !< Blank lines.
   logical                                :: blank !< A blank line.

   up = doc%parent(node)
   blank = .true.
   lines = 1_I4P
   if (ends_unclosed_html(doc, prev)) then ! a blank line would be its own, read back: an item is ended by indentation
      blank = .false.
   elseif (doc%node_kind(up) == NODE_LIST) then
      blank = .not. doc%list_tight(up)
   elseif (doc%node_kind(up) == NODE_ITEM) then
      if (doc%list_tight(doc%parent(up))) blank = needs_blank(w, doc, prev, node)
   elseif (w%canonical .or. source_blank(doc, prev, node) < 0_I4P) then
      if (doc%node_kind(prev) == NODE_LINK_REFERENCE_DEFINITION .and. &
          doc%node_kind(node) == NODE_LINK_REFERENCE_DEFINITION) blank = .false.
      ! a paragraph starting with raw HTML follows a definition on the next line: after a blank line, the HTML would
      ! open an HTML block (a paragraph so read comes from the definition's own paragraph)
      if (doc%node_kind(prev) == NODE_LINK_REFERENCE_DEFINITION .and. (doc%node_kind(node) == NODE_PARAGRAPH .or. &
          doc%node_kind(node) == NODE_HEADING)) then
         if (doc%first_child(node) /= 0_I4P) then
            if (doc%node_kind(doc%first_child(node)) == NODE_HTML_INLINE) blank = needs_blank(w, doc, prev, node)
         endif
      endif
   elseif (source_blank(doc, prev, node) == 0_I4P) then
      blank = needs_blank(w, doc, prev, node)
   else
      lines = source_blank(doc, prev, node)
   endif
   if (blank) then
      do while (lines > 0_I4P)
         call emit_line(w, '')
         lines = lines - 1_I4P
      enddo
   else
      w%glued = doc%node_kind(prev) == NODE_PARAGRAPH .or. doc%node_kind(prev) == NODE_LINK_REFERENCE_DEFINITION
   endif
   endsubroutine separate

   function source_blank(doc, prev, node) result(blank)
   !< Return the blank lines the source has between two sibling blocks, -1 if unknown (a block without a span, built or
   !< moved by hand).
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: prev  !< Previous sibling.
   integer(I4P),            intent(in) :: node  !< Block.
   integer(I4P)                        :: blank !< Result.
   character(:), allocatable           :: gap   !< The source between them.
   integer(I4P)                        :: a, b  !< Their bounds.
   integer(I4P)                        :: k     !< Position.
   integer(I4P)                        :: ends  !< Line endings in the gap.

   blank = -1_I4P
   a = doc%span_last(prev)
   b = doc%span_first(node)
   if (a < 1_I4P .or. b <= a .or. b > doc%source_length() + 1_I4P) return
   gap = doc%source_slice(a + 1_I4P, b - 1_I4P)
   ends = 0_I4P
   do k=1, len(gap, kind=I4P)
      if (gap(k:k) == LF) then
         ends = ends + 1_I4P
      elseif (gap(k:k) == CR) then
         ends = ends + 1_I4P
         if (k < len(gap, kind=I4P)) then
            if (gap(k+1:k+1) == LF) ends = ends - 1_I4P
         endif
      endif
   enddo
   blank = max(0_I4P, ends - 1_I4P)
   endfunction source_blank

   function needs_blank(w, doc, prev, node) result(blank)
   !< Return true if a block cannot follow its previous sibling directly (no blank line) and be read as a new block: an
   !< open paragraph before it that it cannot interrupt, an HTML block that only a blank line ends, a definition.
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: prev  !< Previous sibling.
   integer(I4P),            intent(in) :: node  !< Block.
   logical                             :: blank !< Result.
   integer(I4P)                        :: t     !< HTML block type.
   integer(I4P)                        :: item  !< First item of a list.

   blank = .false.
   select case(doc%node_kind(prev))
   case(NODE_HTML_BLOCK)
      t = html_type(doc%literal(prev))
      blank = t == 6_I4P .or. t == 7_I4P .or. t == 0_I4P
      return
   case(NODE_LINK_REFERENCE_DEFINITION) ! read as paragraph lines: a paragraph after it may not look like a title
      select case(doc%node_kind(node))
      case(NODE_LINK_REFERENCE_DEFINITION)
         return
      case(NODE_HEADING) ! underlined, its lines are the rest of the definition's paragraph
         if (written_setext(w, doc, node)) then
            blank = len(doc%title(prev)) == 0
            if (blank) blank = index('"''(', first_text_char(doc, node)) > 0
            return
         endif
      case(NODE_PARAGRAPH)
         blank = len(doc%title(prev)) == 0
         if (blank) blank = index('"''(', first_text_char(doc, node)) > 0
         return
      endselect
   case(NODE_BLOCK_QUOTE)
      blank = doc%node_kind(node) == NODE_BLOCK_QUOTE
      if (blank) return
      if (quote_closes(w, doc, prev)) return ! its paragraph closed by an empty `>` line
   endselect
   if (.not. (ends_open_paragraph(doc, prev) .or. doc%node_kind(prev) == NODE_LINK_REFERENCE_DEFINITION)) return
   if (doc%node_kind(prev) /= NODE_PARAGRAPH .and. doc%node_kind(prev) /= NODE_LINK_REFERENCE_DEFINITION) then
      ! the open paragraph is in a quote or an item of `prev`, not in the block's container: any block start is read
      ! as one, only what a paragraph would take as a lazy continuation line needs the blank line
      blank = lazy_after(w, doc, node)
      return
   endif
   select case(doc%node_kind(node)) ! can it interrupt a paragraph?
   case(NODE_HEADING)
      blank = written_setext(w, doc, node)
   case(NODE_CODE_BLOCK)
      blank = written_indented(w, doc, node)
   case(NODE_HTML_BLOCK)
      t = html_type(doc%literal(node))
      blank = t == 7_I4P .or. t == 0_I4P
   case(NODE_THEMATIC_BREAK, NODE_BLOCK_QUOTE)
      blank = .false.
   case(NODE_LIST)
      item = doc%first_child(node)
      blank = item == 0_I4P
      if (.not. blank) blank = doc%first_child(item) == 0_I4P
      if (.not. blank) then ! an item starting with a blank line (an indented HTML block first) cannot interrupt either
         if (doc%node_kind(doc%first_child(item)) == NODE_HTML_BLOCK) &
            blank = index(doc%literal(doc%first_child(item)), ' ') == 1
      endif
      if (.not. blank .and. doc%list_type(node) == LIST_ORDERED) blank = doc%list_start(node) /= 1_I4P
   case default
      blank = .true.
   endselect
   endfunction needs_blank

   function glued_html(w, doc, node) result(glued)
   !< Return true if a paragraph (or a setext heading) follows a definition on the next line and starts with raw HTML:
   !< its first line, a continuation line of the definition's paragraph, is indented by 4 (which it loses), so that the
   !< HTML opens no HTML block.
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Paragraph or heading.
   logical                             :: glued !< Result.

   glued = .false.
   if (.not. w%glued .or. doc%prev_sibling(node) == 0_I4P .or. doc%first_child(node) == 0_I4P) return
   if (doc%node_kind(doc%prev_sibling(node)) /= NODE_LINK_REFERENCE_DEFINITION) return
   if (doc%node_kind(doc%first_child(node)) /= NODE_HTML_INLINE) return
   if (doc%node_kind(node) == NODE_HEADING) then
      if (.not. written_setext(w, doc, node)) return
   endif
   glued = .true.
   endfunction glued_html

   function lazy_after(w, doc, node) result(lazy)
   !< Return true if a block's first line, after an open paragraph of another container, would be read as a lazy
   !< continuation line of it: a paragraph, a definition, a setext heading, an indented code block, an HTML block of
   !< type 7 (it starts no block where a paragraph could go on).
   type(md_writer),         intent(in) :: w    !< Writer.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Block.
   logical                             :: lazy !< Result.
   integer(I4P)                        :: t    !< HTML block type.

   select case(doc%node_kind(node))
   case(NODE_PARAGRAPH, NODE_LINK_REFERENCE_DEFINITION)
      lazy = .true.
   case(NODE_HEADING)
      lazy = written_setext(w, doc, node)
   case(NODE_CODE_BLOCK)
      lazy = written_indented(w, doc, node)
   case(NODE_HTML_BLOCK)
      t = html_type(doc%literal(node))
      lazy = t == 7_I4P .or. t == 0_I4P
   case default
      lazy = doc%node_kind(node) >= NODE_EXTENSION_FIRST
   endselect
   endfunction lazy_after

   function quote_closes(w, doc, quote) result(closes)
   !< Return true if a block quote ending with an open paragraph is closed by an empty `>` line before the block after
   !< it, in an item of a tight list: that block would otherwise be read as a lazy continuation line, and a blank line
   !< there would make the list loose.
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: quote  !< Block quote.
   logical                             :: closes !< Result.
   integer(I4P)                        :: up     !< Its parent.

   closes = .false.
   if (doc%node_kind(quote) /= NODE_BLOCK_QUOTE .or. doc%next_sibling(quote) == 0_I4P) return
   up = doc%parent(quote)
   if (doc%node_kind(up) /= NODE_ITEM) return
   if (.not. doc%list_tight(doc%parent(up))) return
   if (.not. ends_open_paragraph(doc, quote)) return
   if (doc%node_kind(doc%next_sibling(quote)) == NODE_BLOCK_QUOTE) return
   closes = lazy_after(w, doc, doc%next_sibling(quote))
   endfunction quote_closes

   function first_text_char(doc, node) result(c)
   !< Return the first character of a block's first text, `a` if it does not start with a text.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Block.
   character                           :: c    !< Character.
   character(:), allocatable           :: t    !< Literal.

   c = 'a'
   if (doc%first_child(node) == 0_I4P) return
   if (doc%node_kind(doc%first_child(node)) /= NODE_TEXT) return
   t = doc%literal(doc%first_child(node))
   if (len(t) > 0) c = t(1:1)
   endfunction first_text_char

   function ends_open_paragraph(doc, node) result(open_)
   !< Return true if a block ends with a paragraph left open (itself, or the last block of its last items and quotes; a
   !< definition there too, read from a paragraph): a line after it would continue that paragraph lazily.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Block.
   logical                             :: open_ !< Result.
   integer(I4P)                        :: n     !< Current block.

   n = node
   do
      select case(doc%node_kind(n))
      case(NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM)
         if (doc%last_child(n) == 0_I4P) exit
         n = doc%last_child(n)
      case default
         exit
      endselect
   enddo
   open_ = doc%node_kind(n) == NODE_PARAGRAPH .or. (n /= node .and. doc%node_kind(n) == NODE_LINK_REFERENCE_DEFINITION)
   endfunction ends_open_paragraph

   subroutine choose_list_marker(w, doc, list)
   !< Choose a list's bullet or delimiter (recorded, else `-` and `.`) and its first number: a list right after another
   !< of the same type takes another marker, else the two would be one list.
   type(md_writer),         intent(inout) :: w      !< Writer.
   type(markdown_document), intent(in)    :: doc    !< Document.
   integer(I4P),            intent(in)    :: list   !< List.
   character(*), parameter                :: BULLETS = '-*+' !< Bullets, canonical first.
   integer(I4P)                           :: prev   !< Previous sibling.
   integer(I4P)                           :: marker !< Marker.
   integer(I4P)                           :: k      !< Counter.

   marker = 0_I4P
   if (.not. w%canonical) marker = doc%int_slot(list, SLOT_LIST_MARKER)
   prev = doc%prev_sibling(list)
   if (prev /= 0_I4P) then
      if (doc%node_kind(prev) /= NODE_LIST) then
         prev = 0_I4P
      elseif (doc%list_type(prev) /= doc%list_type(list)) then
         prev = 0_I4P
      endif
   endif
   if (doc%list_type(list) == LIST_ORDERED) then
      if (marker /= iachar('.') .and. marker /= iachar(')')) marker = iachar('.')
      if (prev /= 0_I4P) then
         if (w%mark(prev) == marker) marker = merge(iachar(')'), iachar('.'), marker == iachar('.'))
      endif
      w%number(list) = doc%list_start(list)
   else
      if (index(BULLETS, char1(max(0_I4P, min(127_I4P, marker)))) == 0 .or. marker == 0_I4P) marker = iachar('-')
      if (prev /= 0_I4P) then
         if (w%mark(prev) == marker) then
            do k=1, len(BULLETS)
               if (iachar(BULLETS(k:k)) /= marker) exit
            enddo
            marker = iachar(BULLETS(k:k))
         endif
      endif
      ! an empty item behind two bullets of the same character on its line would make it a thematic break (`- - -`)
      if (doc%first_child(list) /= 0_I4P) then
         if (doc%first_child(doc%first_child(list)) == 0_I4P .and. count(w%pending(1:w%depth) .and. &
             w%bullet(1:w%depth) == marker) >= 2) then
            do k=1, len(BULLETS) ! one not twice on the line (`+` never makes a break), not the list before's
               if (count(w%pending(1:w%depth) .and. w%bullet(1:w%depth) == iachar(BULLETS(k:k))) >= 2) cycle
               if (prev /= 0_I4P) then
                  if (iachar(BULLETS(k:k)) == w%mark(prev)) cycle
               endif
               exit
            enddo
            marker = iachar(BULLETS(min(k, len(BULLETS, kind=I4P)):min(k, len(BULLETS, kind=I4P))))
         endif
      endif
   endif
   w%mark(list) = marker
   endsubroutine choose_list_marker

   function break_char(w, doc, node) result(c)
   !< Return a thematic break's character: recorded (else `*`), unless it would make the line something else: `-` under
   !< an open paragraph (a setext underline), the bullet of an item whose marker is on the same line (one break).
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Thematic break.
   character                           :: c     !< Character.
   character(4)                        :: order !< Candidates.
   integer(I4P)                        :: k, f  !< Counters.
   logical                             :: ok    !< A candidate is safe.

   order = '*-_*'
   if (.not. w%canonical) then
      k = doc%int_slot(node, SLOT_BREAK_CHAR)
      if (k == iachar('-') .or. k == iachar('_') .or. k == iachar('*')) order(1:1) = achar(k)
   endif
   do k=1, 4
      c = order(k:k)
      ok = .not. (c == '-' .and. w%glued)
      do f=1, w%depth
         if (w%pending(f) .and. w%bullet(f) == iachar(c)) ok = .false.
      enddo
      if (ok) return
   enddo
   c = order(1:1)
   endfunction break_char

   function break_line(w, doc, node) result(line)
   !< Return a thematic break's line: its line in the source (recorded style) when it is one of the character chosen,
   !< else three of the character.
   type(md_writer),         intent(in) :: w    !< Writer.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Thematic break.
   character(:), allocatable           :: line !< Line.
   character                           :: c    !< Character.
   integer(I4P)                        :: code !< Character of the source's line.
   integer(I4P)                        :: fail !< Where its scan failed.

   c = break_char(w, doc, node)
   line = repeat(c, 3)
   if (w%canonical) return
   line = source_line(doc, doc%span_first(node), doc%span_last(node))
   call scan_thematic_break(line, 1_I4P, code, fail)
   if (code /= iachar(c, kind=I4P)) line = repeat(c, 3)
   endfunction break_line

   function source_line(doc, first, last) result(line)
   !< Return the last line of a span of the source without its leading spaces, empty for no span.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: first !< First byte.
   integer(I4P),            intent(in) :: last  !< Last byte.
   character(:), allocatable           :: line  !< Line.
   integer(I4P)                        :: k     !< Position.

   line = ''
   if (first < 1_I4P .or. last < first .or. last > doc%source_length()) return
   line = doc%source_slice(first, last)
   k = max(index(line, LF, back=.true.), index(line, CR, back=.true.))
   line = line(k+1:)
   k = verify(line, ' ')
   if (k == 0) then
      line = ''
   else
      line = line(k:)
   endif
   endfunction source_line

   subroutine write_heading(w, doc, slots, node, ierr, msg)
   !< Write a heading, ATX or setext.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Heading.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: text     !< Its content.
   character(:), allocatable                :: line     !< The ATX line.
   integer(I4P)                             :: level    !< Level.
   integer(I4P)                             :: closing  !< Closing `#`s.
   integer(I4P)                             :: k, j     !< Positions.
   integer(I4P)                             :: chars    !< Characters of the last line.

   level = doc%heading_level(node)
   if (written_setext(w, doc, node)) then
      call inline_content(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_LINES, text, &
                          ierr, msg)
      if (ierr /= BARUCH_OK) return
      call emit_text(w, text, .false.)
      k = index(text, LF, back=.true.)
      chars = 0_I4P
      do j=k+1, len(text, kind=I4P)
         if (iand(iachar(text(j:j)), 192) /= 128) chars = chars + 1_I4P
      enddo
      line = repeat(merge('=', '-', level == 1_I4P), max(1_I4P, chars))
      if (.not. w%canonical) then ! the underline of the source
         text = source_line(doc, doc%span_first(node), doc%span_last(node))
         if (scan_setext_underline(text, 1_I4P) == level) line = text
      endif
      call emit_line(w, line)
      return
   endif
   call inline_content(w, doc, slots, node, doc%first_child(node), doc%last_child(node), INLINE_ATX, text, ierr, msg)
   if (ierr /= BARUCH_OK) return
   closing = 0_I4P
   if (.not. w%canonical) closing = max(0_I4P, doc%int_slot(node, SLOT_HEADING_CLOSING))
   line = repeat('#', max(1_I4P, min(6_I4P, level)))
   if (len(text) > 0) then
      if (closing == 0_I4P) then ! a closing sequence of the content: escape its first `#`
         k = len(text, kind=I4P)
         do while (k >= 1_I4P)
            if (text(k:k) /= '#') exit
            k = k - 1_I4P
         enddo
         if (k < len(text, kind=I4P)) then
            if (k == 0_I4P) then
               text = BACKSLASH//text
            elseif (is_space_or_tab(text(k:k))) then
               text = text(1:k)//BACKSLASH//text(k+1:)
            endif
         endif
      endif
      line = line//' '//text
   endif
   if (closing > 0_I4P) line = line//' '//repeat('#', closing)
   call emit_line(w, line)
   endsubroutine write_heading

   function written_setext(w, doc, node) result(setext)
   !< Return true if a heading is written setext: level 1 or 2 with content, recorded so or spanning lines.
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: node   !< Heading.
   logical                             :: setext !< Result.
   integer(I4P)                        :: n      !< Node of its content.

   setext = .false.
   if (doc%heading_level(node) > 2_I4P .or. doc%first_child(node) == 0_I4P) return
   n = doc%first_child(node)
   do while (n /= 0_I4P .and. n /= node) ! a line break anywhere in it, a title or raw HTML spanning lines
      select case(doc%node_kind(n))
      case(NODE_SOFTBREAK, NODE_LINEBREAK)
         setext = .true.
      case(NODE_LINK, NODE_IMAGE)
         setext = scan(doc%title(n), LF//CR) > 0
      case(NODE_HTML_INLINE)
         setext = scan(doc%literal(n), LF//CR) > 0
      endselect
      if (setext) return
      n = next_in(doc, n, node)
   enddo
   ! recorded so, unless it follows a paragraph in a tight list's item: underlined, it would need a blank line there
   if (.not. w%canonical) setext = doc%int_slot(node, SLOT_HEADING_FORM) == HEADING_SETEXT .and. &
                                   .not. glued_to_paragraph(doc, node)
   endfunction written_setext

   function glued_to_paragraph(doc, node) result(glued)
   !< Return true if a block follows an open paragraph (or a definition) in an item of a tight list: no blank line may
   !< come between them, so it must be a block that can interrupt a paragraph.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Block.
   logical                             :: glued !< Result.
   integer(I4P)                        :: up    !< Parent.
   integer(I4P)                        :: prev  !< Previous sibling.

   glued = .false.
   up = doc%parent(node)
   prev = doc%prev_sibling(node)
   if (up == 0_I4P .or. prev == 0_I4P) return
   if (doc%node_kind(up) /= NODE_ITEM) return
   if (.not. doc%list_tight(doc%parent(up))) return
   glued = ends_open_paragraph(doc, prev) .or. doc%node_kind(prev) == NODE_LINK_REFERENCE_DEFINITION
   endfunction glued_to_paragraph

   subroutine write_code_block(w, doc, node)
   !< Write a code block, indented or fenced.
   type(md_writer),         intent(inout) :: w     !< Writer.
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: node  !< Code block.
   character(:), allocatable              :: text  !< Literal.
   character(:), allocatable              :: info  !< Info string.
   character                              :: c     !< Fence character.
   integer(I4P)                           :: n     !< Fence length.
   integer(I4P)                           :: i, j  !< Bounds of a line.
   integer(I4P)                           :: k, r  !< Position, run.

   text = doc%literal(node)
   if (written_indented(w, doc, node)) then
      i = 1_I4P
      do while (i <= len(text, kind=I4P))
         j = index(text(i:), LF) + i - 1_I4P
         if (j < i) j = len(text, kind=I4P) + 1_I4P
         if (j > i) then
            call emit_line(w, '    '//text(i:j-1))
         else
            call emit_line(w, '')
         endif
         i = j + 1_I4P
      enddo
      return
   endif
   info = doc%info(node)
   c = '`'
   n = 3_I4P
   if (.not. w%canonical) then
      if (doc%int_slot(node, SLOT_CODE_FENCE_CHAR) == iachar('~')) c = '~'
      n = max(3_I4P, doc%int_slot(node, SLOT_CODE_FENCE_LENGTH))
   endif
   if (index(info, '`') > 0) c = '~'
   i = 1_I4P
   do while (i <= len(text, kind=I4P)) ! longer than any line inside that would close it
      j = index(text(i:), LF) + i - 1_I4P
      if (j < i) j = len(text, kind=I4P) + 1_I4P
      k = i
      do while (k < j .and. k - i < 3_I4P) ! (a tab too: in an item, it may expand to less than 4 columns)
         if (text(k:k) /= ' ' .and. text(k:k) /= TAB) exit
         k = k + 1_I4P
      enddo
      r = 0_I4P
      do while (k + r < j)
         if (text(k+r:k+r) /= c) exit
         r = r + 1_I4P
      enddo
      if (r >= n) then
         if (len_trim(text(k+r:j-1)) == 0 .or. verify(text(k+r:j-1), ' '//TAB) == 0) n = r + 1_I4P
      endif
      i = j + 1_I4P
   enddo
   info = info_text(info)
   if (len(info) > 0) then ! an info string starting with the fence character would lengthen the fence
      if (info(1:1) == c) info = ' '//info
   endif
   call emit_line(w, fence_line(w, doc, node, c, n, .true., repeat(c, n)//info))
   if (len(text) > 0) then
      if (text(len(text):len(text)) /= LF) text = text//LF
      call emit_text(w, text, .false.)
   endif
   call emit_line(w, fence_line(w, doc, node, c, n, .false., repeat(c, n)))
   endsubroutine write_code_block

   function fence_line(w, doc, node, c, n, opening, default) result(line)
   !< Return a fence of a code block: its line in the source (recorded style) when it is the fence chosen (the opening
   !< one with the same info string, the closing one at least as long), else `default`.
   type(md_writer),         intent(in) :: w       !< Writer.
   type(markdown_document), intent(in) :: doc     !< Document.
   integer(I4P),            intent(in) :: node    !< Code block.
   character,               intent(in) :: c       !< Fence character chosen.
   integer(I4P),            intent(in) :: n       !< Fence length chosen.
   logical,                 intent(in) :: opening !< The opening fence (else the closing one).
   character(*),            intent(in) :: default !< The fence written otherwise.
   character(:), allocatable           :: line    !< Fence.
   character(:), allocatable           :: span    !< The block's source.
   character(:), allocatable           :: info    !< An info string read back.
   integer(I4P)                        :: k       !< Position.

   line = default
   if (w%canonical) return
   if (doc%span_first(node) < 1_I4P .or. doc%span_last(node) > doc%source_length() .or. &
       doc%span_last(node) < doc%span_first(node)) return
   span = doc%source_slice(doc%span_first(node), doc%span_last(node))
   if (opening) then
      k = scan(span, LF//CR)
      if (k > 0) span = span(1:k-1)
      if (scan_open_fence(span, 1_I4P) /= n .or. span(1:1) /= c) return
      info = span(n+1:)
      k = verify(info, ' '//TAB)
      if (k == 0) then
         info = ''
      else
         info = info(k:verify(info, ' '//TAB, back=.true.))
      endif
      info = unescape_string(info)
      if (info == doc%info(node) .and. len(info) == len(doc%info(node))) line = span
   else
      k = scan(span, LF//CR, back=.true.)
      if (k == 0) return ! one line: no closing fence in the source
      span = span(k+1:)
      k = verify(span, ' ')
      if (k == 0 .or. k > 4) return
      span = span(k:)
      if (span(1:1) /= c) return
      if (scan_close_fence(span, 1_I4P, c) >= n) line = span
   endif
   endfunction fence_line

   function written_indented(w, doc, node) result(indented)
   !< Return true if a code block is written indented: recorded so, with no info string, its first and last lines not
   !< blank, and no code block or list just before it (they would take its lines).
   type(md_writer),         intent(in) :: w        !< Writer.
   type(markdown_document), intent(in) :: doc      !< Document.
   integer(I4P),            intent(in) :: node     !< Code block.
   logical                             :: indented !< Result.
   character(:), allocatable           :: text     !< Literal.
   integer(I4P)                        :: prev     !< Previous sibling.
   integer(I4P)                        :: j        !< Position.

   indented = .false.
   if (w%canonical .or. doc%int_slot(node, SLOT_CODE_FORM) /= CODE_INDENTED .or. len(doc%info(node)) > 0) return
   if (glued_to_paragraph(doc, node)) return ! indented, it would need a blank line in a tight list
   if (doc%node_kind(doc%parent(node)) == NODE_ITEM .and. doc%prev_sibling(node) == 0_I4P) then
      ! on the line of its item's marker, behind bullets: `- -     -` would be a thematic break
      if (scan(doc%literal(node), '-*_') > 0) return
      ! first in the last item, an indented HTML block after the list: the item's marker must be followed by more spaces
      if (html_after(doc, doc%parent(node)) > 0_I4P) return
   endif
   prev = doc%prev_sibling(node)
   if (prev /= 0_I4P) then ! an indented code block or a list before it would take its lines
      if (doc%node_kind(prev) == NODE_LIST) return
      if (doc%node_kind(prev) == NODE_CODE_BLOCK) then
         if (doc%int_slot(prev, SLOT_CODE_FORM) == CODE_INDENTED) return
      endif
   endif
   text = doc%literal(node)
   if (len(text) == 0) return
   j = index(text, LF)
   if (j == 0) j = len(text) + 1
   if (verify(text(1:j-1), ' '//TAB) == 0) return
   if (text(len(text):len(text)) == LF) text = text(1:len(text)-1)
   j = index(text, LF, back=.true.)
   if (verify(text(j+1:), ' '//TAB) == 0) return
   indented = .true.
   endfunction written_indented

   ! inlines
   subroutine write_inline_lines(w, doc, slots, block, first, last, mode, prefix, ierr, msg)
   !< Write the inline content of a block (its nodes `first` to `last`) as lines, each behind `prefix` (an extension's,
   !< for `INLINE_PLAIN`).
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: first    !< First inline node.
   integer(I4P),              intent(in)    :: last     !< Last inline node, a sibling of `first`.
   integer(I4P),              intent(in)    :: mode     !< How the content is written.
   character(*),              intent(in)    :: prefix   !< Prefix of every line.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: text     !< The content.

   call inline_content(w, doc, slots, block, first, last, mode, text, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (len(prefix) > 0) call push(w, prefix, prefix, 0_I4P)
   if (len(text) > 0 .or. len(prefix) > 0) call emit_text(w, text, .false.)
   if (len(prefix) > 0) call pop(w)
   endsubroutine write_inline_lines

   subroutine inline_content(w, doc, slots, block, first, last, mode, text, ierr, msg)
   !< Return the inline content of a block (its nodes `first` to `last`), lines joined by LF, no prefix. The emphases'
   !< delimiters chosen (`prepass`), the content is written, and its delimiters are read back as the parser reads them
   !< (`emphasis_reads_back`); when they would pair otherwise, the texts' runs of `*` and `_` that can open or close
   !< are escaped, or a text's run touching a delimiter of its character left to join the delimiter's run; then every
   !< emphasis takes its recorded delimiter (not in the canonical style), all `*`, all `_`; then one emphasis, two,
   !< all, all but one take the other delimiter character, until they read back (some trees have no such choice: the
   !< first is kept). The tries are bounded (`SINGLES_MAX`, `PAIRS_MAX`), each a pass over the block's content.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: first    !< First inline node.
   integer(I4P),              intent(in)    :: last     !< Last inline node, a sibling of `first`.
   integer(I4P),              intent(in)    :: mode     !< How the content is written.
   character(:), allocatable, intent(out)   :: text     !< The content.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P), parameter                  :: SINGLES_MAX = 64_I4P !< Emphases changed alone, at most.
   integer(I4P), parameter                  :: PAIRS_MAX = 256_I4P  !< Pairs of emphases tried at most.
   type(inline_state)                       :: s        !< State.
   integer(I4P), allocatable                :: emphs(:) !< The block's emphases.
   integer(I4P), allocatable                :: first_marks(:) !< Their delimiters, first chosen.
   integer                                  :: c        !< Candidate.
   integer(I4P)                             :: node     !< Current node.
   integer(I4P)                             :: k        !< Emphases.
   integer(I4P)                             :: i, j     !< Counters.
   integer(I4P)                             :: tries    !< Pairs tried.
   logical                                  :: found    !< A choice reads back.
   logical                                  :: paranoid !< The texts' runs that can open or close escaped.
   logical                                  :: merged   !< A text's run joins a delimiter's run.

   text = ''
   if (first == 0_I4P) return
   paranoid = .false.
   merged = .false.
   call inline_pass(w, doc, slots, block, first, last, mode, .true., s, ierr, msg)
   if (ierr /= BARUCH_OK) return
   found = emphasis_reads_back(s)
   if (.not. found) call attempt
   if (ierr /= BARUCH_OK) return
   if (.not. found) then
      k = 0_I4P
      allocate(emphs(8))
      node = first
      do while (node /= 0_I4P)
         if (doc%node_kind(node) == NODE_EMPH .or. doc%node_kind(node) == NODE_STRONG) then
            k = k + 1_I4P
            if (k > size(emphs, kind=I4P)) call grow(emphs)
            emphs(k) = node
         endif
         node = next_in_range(doc, node, first, last)
      enddo
      allocate(first_marks(k))
      first_marks = w%mark(emphs(1:k))
      do c=1, 3 ! every emphasis as recorded (the source's choice), all `*`, all `_`
         if (c == 1 .and. w%canonical) cycle
         do i=1_I4P, k
            select case(c)
            case(1)
               w%mark(emphs(i)) = merge(iachar('_', kind=I4P), iachar('*', kind=I4P), &
                                        doc%int_slot(emphs(i), SLOT_EMPHASIS_CHAR) == iachar('_'))
            case(2)
               w%mark(emphs(i)) = iachar('*', kind=I4P)
            case default
               w%mark(emphs(i)) = iachar('_', kind=I4P)
            endselect
         enddo
         call attempt
         if (ierr /= BARUCH_OK) return
         if (found) exit
      enddo
      if (.not. found) w%mark(emphs(1:k)) = first_marks
      do i=1_I4P, min(k, SINGLES_MAX)
         if (found) exit
         call flip(emphs(i))
         call attempt
         if (ierr /= BARUCH_OK) return
         if (found) exit
         call flip(emphs(i))
      enddo
      tries = 0_I4P
      if (k > SINGLES_MAX / 2_I4P) tries = PAIRS_MAX ! many emphases: no pairs (each try is a pass)
      pairs: do i=1_I4P, k - 1_I4P
         if (found) exit pairs
         do j=i + 1_I4P, k
            tries = tries + 1_I4P
            if (tries > PAIRS_MAX) exit pairs
            call flip(emphs(i))
            call flip(emphs(j))
            call attempt
            if (ierr /= BARUCH_OK) return
            if (found) exit pairs
            call flip(emphs(i))
            call flip(emphs(j))
         enddo
      enddo pairs
      if (.not. found) then ! every one the other character, then all but one
         w%mark(emphs(1:k)) = first_marks
         do i=0_I4P, min(k, SINGLES_MAX)
            do j=1_I4P, k
               if (j /= i) call flip(emphs(j))
            enddo
            call attempt
            if (ierr /= BARUCH_OK) return
            if (found) exit
            w%mark(emphs(1:k)) = first_marks
         enddo
      endif
      if (.not. found) then ! the first choice, the texts' runs escaped
         w%mark(emphs(1:k)) = first_marks
         paranoid = .true.
         merged = .false.
         call inline_pass(w, doc, slots, block, first, last, mode, .false., s, ierr, msg, paranoid=paranoid)
         if (ierr /= BARUCH_OK) return
      endif
   endif
   ! a paragraph that would read as a link reference definition, written (escapes, references, raw HTML and code
   ! spans of later nodes, which the text's own check cannot see): its first `[`, a text's, escaped
   if (mode == INLINE_LINES .and. (doc%node_kind(block) == NODE_PARAGRAPH .or. doc%node_kind(block) == NODE_HEADING) &
       .and. s%buf%used > 0_I4P) then
      if (s%buf%chars(1:1) == '[' .and. reads_as_definition(s%buf%chars(1:s%buf%used))) then
         call inline_pass(w, doc, slots, block, first, last, mode, .false., s, ierr, msg, escape_first=.true., &
                          paranoid=paranoid, merge_runs=merged)
         if (ierr /= BARUCH_OK) return
      endif
   endif
   text = s%buf%chars(1:s%buf%used)

   contains
      subroutine flip(n)
      !< Give an emphasis the other delimiter character.
      integer(I4P), intent(in) :: n !< Emphasis.

      w%mark(n) = merge(iachar('*', kind=I4P), iachar('_', kind=I4P), w%mark(n) == iachar('_'))
      endsubroutine flip

      subroutine attempt
      !< Write the content with the delimiters as they are, the texts' runs escaped, then joining the delimiters' runs;
      !< `found` if one reads back.
      integer :: m !< Mode.

      do m=1, 2
         paranoid = m == 1
         merged = m == 2
         call inline_pass(w, doc, slots, block, first, last, mode, .false., s, ierr, msg, paranoid=paranoid, &
                          merge_runs=merged)
         if (ierr /= BARUCH_OK) return
         found = emphasis_reads_back(s)
         if (found) return
      enddo
      endsubroutine attempt
   endsubroutine inline_content

   subroutine inline_pass(w, doc, slots, block, first, last, mode, choose, s, ierr, msg, escape_first, paranoid, &
                          merge_runs)
   !< Write the inline content of a block once, into a new state: the delimiters chosen first (`choose`), or as they
   !< are; with `escape_first`, a `[` of a text starting the content escaped; with `paranoid`, every text's run of `*`
   !< and `_` that can open or close escaped; with `merge_runs`, a text's run touching a delimiter of its character left
   !< to join it (as `***a**` reads, and as a source may have it).
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: first    !< First inline node.
   integer(I4P),              intent(in)    :: last     !< Last inline node, a sibling of `first`.
   integer(I4P),              intent(in)    :: mode     !< How the content is written.
   logical,                   intent(in)    :: choose   !< Choose the emphases' delimiters.
   type(inline_state),        intent(out)   :: s        !< State.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   logical, optional,         intent(in)    :: escape_first !< Escape a text's `[` starting the content.
   logical, optional,         intent(in)    :: paranoid !< Escape every text's run that can open or close.
   logical, optional,         intent(in)    :: merge_runs !< Leave a text's run touching a delimiter unescaped.
   integer(I4P)                             :: node     !< Current node.
   integer(I4P)                             :: skip     !< Last node of a text run.
   logical                                  :: descend  !< Visit the children.

   if (present(escape_first)) s%escape_first = escape_first
   if (present(paranoid)) s%paranoid = paranoid
   if (present(merge_runs)) s%merge_runs = merge_runs
   allocate(character(256) :: s%buf%chars)
   allocate(s%link_start(8), s%saved(4, 8), s%lone(8), s%emph(3, 8), s%delims(5, 16), s%seg(8))
   s%mode = mode
   s%line_start = mode == INLINE_LINES
   call prepass(w, doc, s, first, last, choose)
   node = first
   do while (node /= 0_I4P)
      call enter_inline(w, doc, slots, s, block, node, descend, skip, ierr, msg)
      if (ierr /= BARUCH_OK) return
      if (skip /= 0_I4P) node = skip
      if (descend .and. doc%first_child(node) /= 0_I4P) then
         node = doc%first_child(node)
         cycle
      endif
      do
         call leave_inline(w, doc, slots, s, block, node, ierr, msg)
         if (ierr /= BARUCH_OK) return
         if (node == last) then
            node = 0_I4P
            exit
         endif
         if (doc%next_sibling(node) /= 0_I4P) then
            node = doc%next_sibling(node)
            exit
         endif
         node = doc%parent(node)
         if (node == block .or. node == 0_I4P) then
            node = 0_I4P
            exit
         endif
      enddo
   enddo
   endsubroutine inline_pass

   pure subroutine add_delimiter(s, length, c, owner)
   !< Record a delimiter about to be written at the end of the content: an emphasis's, or a text's run (`owner` 0).
   type(inline_state), intent(inout) :: s      !< State.
   integer(I4P),       intent(in)    :: length !< Its length.
   integer(I4P),       intent(in)    :: c      !< Its character, `iachar`.
   integer(I4P),       intent(in)    :: owner  !< Its emphasis, 0 for a text's run.
   integer(I4P)                      :: g      !< Its link text.

   if (.not. allocated(s%delims)) return ! no record (`escape_markdown`)
   s%ndelims = s%ndelims + 1_I4P
   if (s%ndelims > size(s%delims, 2, kind=I4P)) call grow_matrix(s%delims)
   g = 0_I4P
   if (s%depth > 0_I4P) g = s%seg(s%depth)
   s%delims(:, s%ndelims) = [s%buf%used + 1_I4P, length, c, owner, g]
   if (s%line_start .and. s%indent > 0_I4P) s%delims(1, s%ndelims) = s%delims(1, s%ndelims) + s%indent ! `put_raw`'s
   endsubroutine add_delimiter

   function emphasis_reads_back(s) result(ok)
   !< Return true if the delimiters written read back as the emphases they were written for: the parser's delimiter
   !< algorithm (CommonMark 6.2, "process emphasis": flanking, the rule of 3, matching from the inside) run over them,
   !< each link text apart, every emphasis's opener must pair with its own closer, whole (one for an emphasis, two for
   !< strong), and no text's run may pair. Runs are the delimiters written next to each other with one character.
   type(inline_state), intent(in) :: s       !< State, its content written.
   logical                        :: ok      !< Result.
   integer(I4P), parameter        :: RUNS_MAX = 512_I4P !< Runs of a link text checked at most (quadratic search).
   integer(I4P), allocatable      :: own(:)  !< Emphasis of each delimiter character, in order.
   integer(I4P), allocatable      :: olen(:) !< Length of its delimiter.
   integer(I4P), allocatable      :: rc(:)   !< Character of each run.
   integer(I4P), allocatable      :: rfirst(:), rlast(:) !< Its bytes.
   integer(I4P), allocatable      :: rlo(:), rhi(:) !< Its characters left, in `own`.
   integer(I4P), allocatable      :: rorig(:) !< Its length.
   logical,      allocatable      :: ropen(:), rclose(:), ractive(:) !< It can open, close; it is on the stack.
   character(:), allocatable      :: before, after !< Characters around a run.
   integer(I4P)                   :: g       !< Link text.
   integer(I4P)                   :: d       !< Delimiter.
   integer(I4P)                   :: nr, no  !< Runs, characters.
   integer(I4P)                   :: r, o, p, cur, m, node !< Runs, match length, emphasis.
   logical                        :: newrun  !< A delimiter starts a run.

   ok = .true.
   if (s%ndelims == 0_I4P) return
   allocate(rc(s%ndelims), rfirst(s%ndelims), rlast(s%ndelims), rlo(s%ndelims), rhi(s%ndelims), rorig(s%ndelims))
   allocate(ropen(s%ndelims), rclose(s%ndelims), ractive(s%ndelims))
   allocate(own(sum(s%delims(2, 1:s%ndelims))), olen(sum(s%delims(2, 1:s%ndelims))))
   do g=0_I4P, s%segs
      nr = 0_I4P
      no = 0_I4P
      do d=1_I4P, s%ndelims
         if (s%delims(5, d) /= g) cycle
         newrun = .true.
         if (nr > 0_I4P) newrun = s%delims(3, d) /= rc(nr) .or. s%delims(1, d) /= rlast(nr) + 1_I4P
         if (newrun) then
            nr = nr + 1_I4P
            rc(nr) = s%delims(3, d)
            rfirst(nr) = s%delims(1, d)
            rlo(nr) = no + 1_I4P
         endif
         rlast(nr) = s%delims(1, d) + s%delims(2, d) - 1_I4P
         own(no+1_I4P:no+s%delims(2, d)) = s%delims(4, d)
         olen(no+1_I4P:no+s%delims(2, d)) = s%delims(2, d)
         no = no + s%delims(2, d)
         rhi(nr) = no
      enddo
      if (nr == 0_I4P .or. nr > RUNS_MAX) cycle
      do r=1_I4P, nr
         before = ''
         after = ''
         if (rfirst(r) > 1_I4P) before = s%buf%chars(prev_char_start(s%buf%chars(1:rfirst(r)-1), rfirst(r)):rfirst(r)-1)
         if (rlast(r) < s%buf%used) after = utf8_at(s%buf%chars(1:s%buf%used), rlast(r) + 1_I4P)
         call run_flanking(char1(rc(r)), before, after, ropen(r), rclose(r))
         rorig(r) = rhi(r) - rlo(r) + 1_I4P
         ractive(r) = .true.
      enddo
      cur = 1_I4P
      do while (cur <= nr)
         if (.not. (ractive(cur) .and. rclose(cur) .and. rhi(cur) >= rlo(cur))) then
            cur = cur + 1_I4P
            cycle
         endif
         o = 0_I4P
         do p=cur - 1_I4P, 1_I4P, -1_I4P
            if (.not. ractive(p) .or. rhi(p) < rlo(p) .or. rc(p) /= rc(cur) .or. .not. ropen(p)) cycle
            if ((rclose(p) .or. ropen(cur)) .and. mod(rorig(p) + rorig(cur), 3_I4P) == 0_I4P .and. &
                .not. (mod(rorig(p), 3_I4P) == 0_I4P .and. mod(rorig(cur), 3_I4P) == 0_I4P)) cycle
            o = p
            exit
         enddo
         if (o == 0_I4P) then
            if (.not. ropen(cur)) ractive(cur) = .false.
            cur = cur + 1_I4P
            cycle
         endif
         m = 1_I4P
         if (rhi(o) - rlo(o) >= 1_I4P .and. rhi(cur) - rlo(cur) >= 1_I4P) m = 2_I4P
         node = own(rhi(o))
         if (node == 0_I4P .or. olen(rhi(o)) /= m .or. any(own(rhi(o)-m+1_I4P:rhi(o)) /= node) .or. &
             any(own(rlo(cur):rlo(cur)+m-1_I4P) /= node)) then
            ok = .false.
            return
         endif
         rhi(o) = rhi(o) - m
         rlo(cur) = rlo(cur) + m
         ractive(o+1_I4P:cur-1_I4P) = .false.
         if (rhi(o) < rlo(o)) ractive(o) = .false.
         if (rhi(cur) < rlo(cur)) then
            ractive(cur) = .false.
            cur = cur + 1_I4P
         endif
      enddo
      do r=1_I4P, nr ! every emphasis's delimiter paired
         if (rhi(r) >= rlo(r)) then
            if (any(own(rlo(r):rhi(r)) /= 0_I4P)) then
               ok = .false.
               return
            endif
         endif
      enddo
   enddo

   endfunction emphasis_reads_back

   subroutine prepass(w, doc, s, first, last, choose)
   !< Before writing a block's inline content: count its backticks (a backtick of a text is escaped when another
   !< follows) and choose the delimiter of each emphasis (recorded, else `*`; the other one when the same character
   !< would touch the delimiter of the emphasis before it or of the one around it).
   type(md_writer),         intent(inout) :: w      !< Writer.
   type(markdown_document), intent(in)    :: doc    !< Document.
   type(inline_state),      intent(inout) :: s      !< State.
   integer(I4P),            intent(in)    :: first  !< First inline node.
   integer(I4P),            intent(in)    :: last   !< Last inline node.
   logical,                 intent(in)    :: choose !< Choose the emphases' delimiters (else keep them).
   integer(I4P)                           :: n      !< Current node.
   integer(I4P)                           :: up     !< Its parent.
   integer(I4P)                           :: prev   !< Its previous sibling.
   integer(I4P)                           :: c, alt !< Delimiters.
   integer(I4P)                           :: kind   !< Its kind.

   s%ticks = 0_I4P
   n = first
   do while (n /= 0_I4P)
      kind = doc%node_kind(n)
      select case(kind)
      case(NODE_TEXT, NODE_HTML_INLINE)
         s%ticks = s%ticks + count_char(doc%literal(n), '`')
      case(NODE_CODE)
         s%ticks = s%ticks + count_char(doc%literal(n), '`') + 2_I4P
      case(NODE_LINK, NODE_IMAGE)
         s%ticks = s%ticks + count_char(doc%destination(n)//doc%title(n)//doc%label(n), '`')
         if (kind == NODE_LINK) then
            if (is_autolink(w, doc, n)) w%mark(n) = 1_I4P
         endif
      case(NODE_EMPH, NODE_STRONG)
         if (.not. choose) then
            n = next_in_range(doc, n, first, last)
            cycle
         endif
         c = 0_I4P
         if (.not. w%canonical) c = doc%int_slot(n, SLOT_EMPHASIS_CHAR)
         if (c /= iachar('_')) c = iachar('*')
         alt = merge(iachar('*'), iachar('_'), c == iachar('_'))
         up = doc%parent(n)
         prev = doc%prev_sibling(n)
         do while (prev /= 0_I4P) ! an empty text writes nothing: the emphasis before it touches this one
            if (doc%node_kind(prev) /= NODE_TEXT) exit
            if (len(doc%literal(prev)) > 0) exit
            prev = doc%prev_sibling(prev)
         enddo
         if (delimiter_clash(w, doc, n, up, prev, c) .or. ancestor_clash(w, doc, n, c)) then
            if (.not. (delimiter_clash(w, doc, n, up, prev, alt) .or. ancestor_clash(w, doc, n, alt))) then
               c = alt
            elseif (prev /= 0_I4P) then ! both clash: the emphasis before it may take its other character
               if (doc%node_kind(prev) == NODE_EMPH .or. doc%node_kind(prev) == NODE_STRONG) then
                  if (flipped(w, doc, prev)) then
                     if (delimiter_clash(w, doc, n, up, prev, c) .or. ancestor_clash(w, doc, n, c)) c = alt
                  endif
               endif
            endif
         endif
         w%mark(n) = c
      case default
         if (kind >= NODE_EXTENSION_FIRST) s%ticks = s%ticks + 1_I4P
      endselect
      n = next_in_range(doc, n, first, last)
   enddo
   ! an opening run that can also close pairs with a text's run before it that can open: those are escaped
   s%both_open = .false.
   n = first
   do while (n /= 0_I4P)
      if (doc%node_kind(n) == NODE_EMPH .or. doc%node_kind(n) == NODE_STRONG) then
         c = w%mark(n)
         if (.not. in_parent_run(w, doc, n)) then
            if (both_flanking(c, sibling_edge(w, doc, n, .false.), run_after(w, doc, n), .false.)) &
               s%both_open(merge(1, 2, c == iachar('*'))) = .true.
         endif
      endif
      n = next_in_range(doc, n, first, last)
   enddo
   endsubroutine prepass

   function flipped(w, doc, node) result(done)
   !< Give an emphasis its other delimiter if it clashes with nothing around it then, nor its first and last
   !< emphases with it; return true if done (else it is left as it was).
   type(md_writer),         intent(inout) :: w    !< Writer.
   type(markdown_document), intent(in)    :: doc  !< Document.
   integer(I4P),            intent(in)    :: node !< Emphasis, its delimiter and its content's chosen.
   logical                                :: done !< Result.
   integer(I4P)                           :: c    !< Its delimiter.
   integer(I4P)                           :: alt  !< The other one.
   integer(I4P)                           :: prev !< The emphasis before it, or another node.
   integer(I4P)                           :: k    !< A child.

   done = .false.
   c = w%mark(node)
   alt = merge(iachar('*'), iachar('_'), c == iachar('_'))
   prev = doc%prev_sibling(node)
   do while (prev /= 0_I4P)
      if (doc%node_kind(prev) /= NODE_TEXT) exit
      if (len(doc%literal(prev)) > 0) exit
      prev = doc%prev_sibling(prev)
   enddo
   if (delimiter_clash(w, doc, node, doc%parent(node), prev, alt) .or. ancestor_clash(w, doc, node, alt)) return
   w%mark(node) = alt
   k = first_written_child(doc, node)
   if (k /= 0_I4P) then
      if (doc%node_kind(k) == NODE_EMPH .or. doc%node_kind(k) == NODE_STRONG) then
         if (delimiter_clash(w, doc, k, node, 0_I4P, w%mark(k)) .or. ancestor_clash(w, doc, k, w%mark(k))) then
            w%mark(node) = c
            return
         endif
      endif
   endif
   k = doc%last_child(node)
   do while (k /= 0_I4P)
      if (doc%node_kind(k) /= NODE_TEXT) exit
      if (len(doc%literal(k)) > 0) exit
      k = doc%prev_sibling(k)
   enddo
   if (k /= 0_I4P) then
      if (doc%node_kind(k) == NODE_EMPH .or. doc%node_kind(k) == NODE_STRONG) then
         prev = doc%prev_sibling(k)
         do while (prev /= 0_I4P)
            if (doc%node_kind(prev) /= NODE_TEXT) exit
            if (len(doc%literal(prev)) > 0) exit
            prev = doc%prev_sibling(prev)
         enddo
         if (delimiter_clash(w, doc, k, node, prev, w%mark(k))) then
            w%mark(node) = c
            return
         endif
      endif
   endif
   done = .true.
   endfunction flipped

   function ancestor_clash(w, doc, node, c) result(clash)
   !< Return true if the opening run of an emphasis delimited by `c` can also close (both flanking) and an emphasis
   !< around it, outside the run, has the same delimiter: the run would close it (`_*_(x_*_`, three emphases).
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Emphasis.
   integer(I4P),            intent(in) :: c     !< Delimiter.
   logical                             :: clash !< Result.
   integer(I4P)                        :: n     !< The run's outermost emphasis, then the ones around it.
   integer(I4P)                        :: up    !< Its parent.
   integer(I4P)                        :: l, a  !< Lengths of the run and of the ancestor's run.
   integer(I4P)                        :: k, m  !< Emphases of the ancestor's run.

   clash = .false.
   n = node
   do ! the run: the emphases around it it is first in, with the same delimiter
      up = doc%parent(n)
      if (up == 0_I4P) exit
      if (doc%node_kind(up) /= NODE_EMPH .and. doc%node_kind(up) /= NODE_STRONG) exit
      if (w%mark(up) /= c .or. .not. written_first(doc, n)) exit
      n = up
   enddo
   if (.not. both_flanking(c, sibling_edge(w, doc, n, .false.), content_edge(doc, node, .false.), .false.)) return
   l = run_length(doc, n, node)
   up = doc%parent(n)
   do while (up /= 0_I4P)
      if (doc%node_kind(up) /= NODE_EMPH .and. doc%node_kind(up) /= NODE_STRONG) exit ! a link's text is apart
      if (w%mark(up) == c) then
         ! the rule of 3 (CommonMark 6.2, rule 9): runs of lengths summing to a multiple of 3 do not pair
         a = up
         do while (in_parent_run(w, doc, a))
            a = doc%parent(a)
         enddo
         k = a
         do
            m = first_written_child(doc, k)
            if (m == 0_I4P) exit
            if (doc%node_kind(m) /= NODE_EMPH .and. doc%node_kind(m) /= NODE_STRONG) exit
            if (w%mark(m) /= c) exit
            k = m
         enddo
         a = run_length(doc, a, k)
         clash = mod(a + l, 3_I4P) /= 0_I4P .or. (mod(a, 3_I4P) == 0_I4P .and. mod(l, 3_I4P) == 0_I4P)
         return
      endif
      up = doc%parent(up)
   enddo
   endfunction ancestor_clash

   function run_length(doc, top, bottom) result(l)
   !< Return the length of a run of delimiters: those of the emphases from `top` down to `bottom` (1 for an emphasis, 2
   !< for strong).
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: top    !< Outermost emphasis of the run.
   integer(I4P),            intent(in) :: bottom !< Innermost, `top` or below it.
   integer(I4P)                        :: l      !< Length.
   integer(I4P)                        :: n      !< An emphasis.

   l = 0_I4P
   n = bottom
   do while (n /= 0_I4P)
      l = l + merge(1_I4P, 2_I4P, doc%node_kind(n) == NODE_EMPH)
      if (n == top) exit
      n = doc%parent(n)
   enddo
   endfunction run_length

   function first_written_child(doc, node) result(child)
   !< Return the first child of a node that writes something (empty texts write nothing), 0 if none.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Node.
   integer(I4P)                        :: child !< Child.

   child = doc%first_child(node)
   do while (child /= 0_I4P)
      if (doc%node_kind(child) /= NODE_TEXT) return
      if (len(doc%literal(child)) > 0) return
      child = doc%next_sibling(child)
   enddo
   endfunction first_written_child

   function in_parent_run(w, doc, node) result(in_run)
   !< Return true if an emphasis's opening delimiter is in its parent's run: first in it, the same delimiter.
   type(md_writer),         intent(in) :: w      !< Writer.
   type(markdown_document), intent(in) :: doc    !< Document.
   integer(I4P),            intent(in) :: node   !< Emphasis.
   logical                             :: in_run !< Result.
   integer(I4P)                        :: up     !< Its parent.

   in_run = .false.
   up = doc%parent(node)
   if (up == 0_I4P) return
   if (doc%node_kind(up) /= NODE_EMPH .and. doc%node_kind(up) /= NODE_STRONG) return
   in_run = w%mark(up) == w%mark(node) .and. written_first(doc, node)
   endfunction in_parent_run

   function run_after(w, doc, node) result(edge)
   !< Return the class (`EDGE_*`) of the character after the opening run an emphasis starts: in the content of the
   !< innermost emphasis of the run.
   type(md_writer),         intent(in) :: w    !< Writer.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Emphasis.
   integer(I4P)                        :: edge !< Class.
   integer(I4P)                        :: n    !< The run's innermost emphasis.
   integer(I4P)                        :: k    !< Its first written child.

   n = node
   do
      k = first_written_child(doc, n)
      if (k == 0_I4P) exit
      if (doc%node_kind(k) /= NODE_EMPH .and. doc%node_kind(k) /= NODE_STRONG) exit
      if (w%mark(k) /= w%mark(node)) exit
      n = k
   enddo
   edge = content_edge(doc, n, .false.)
   endfunction run_after

   function delimiter_clash(w, doc, node, up, prev, c) result(clash)
   !< Return true if an emphasis delimited by `c` would touch a delimiter of the same character it must not touch: the
   !< emphasis just before it; the emphasis around it, of the same kind, when it is first or last in it (`**x*y*` reads
   !< as `**x` and an emphasis); strong around an emphasis that is all of its content (`***x***` is an emphasis around
   !< strong).
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Emphasis.
   integer(I4P),            intent(in) :: up    !< Its parent.
   integer(I4P),            intent(in) :: prev  !< Its previous sibling.
   integer(I4P),            intent(in) :: c     !< Delimiter.
   logical                             :: clash !< Result.
   logical                             :: first, last !< Nothing written before, after it in its parent.

   clash = .false.
   if (prev /= 0_I4P) then
      if (doc%node_kind(prev) == NODE_EMPH .or. doc%node_kind(prev) == NODE_STRONG) clash = w%mark(prev) == c
   endif
   if (clash .or. up == 0_I4P) return
   if (doc%node_kind(up) == NODE_EMPH .or. doc%node_kind(up) == NODE_STRONG) then
      if (w%mark(up) /= c) return
      first = written_first(doc, node)
      last = written_last(doc, node)
      if (first .and. last) then ! `**x**` is strong, `***x***` an emphasis around strong (`****x****` is right)
         clash = doc%node_kind(node) == NODE_EMPH
      elseif (doc%node_kind(up) == doc%node_kind(node)) then
         ! first, its opener is in its parent's run (`__x_ y_`): that run and the two closers must not both open and
         ! close (`**x*y*`: the rule of 3 keeps a run of 2 from one of 1); last, the same of the closing run and the
         ! openers (`*y*x**`)
         if (first) clash = &
            both_flanking(c, sibling_edge(w, doc, up, .false.), content_edge(doc, node, .false.), .false.) .or. &
            both_flanking(c, content_edge(doc, node, .true.), sibling_edge(w, doc, node, .true.), .true.) .or.   &
            both_flanking(c, content_edge(doc, up, .true.), sibling_edge(w, doc, up, .true.), .true.)
         if (last) clash = &
            both_flanking(c, content_edge(doc, node, .true.), sibling_edge(w, doc, up, .true.), .true.) .or.        &
            both_flanking(c, sibling_edge(w, doc, node, .false.), content_edge(doc, node, .false.), .false.) .or. &
            both_flanking(c, sibling_edge(w, doc, up, .false.), content_edge(doc, up, .false.), .false.)
      endif
   endif
   endfunction delimiter_clash

   function both_flanking(c, before, after, closer) result(both)
   !< Return true if a delimiter run of `c` (`iachar` of `*` or `_`) between characters of the classes `before` and
   !< `after` (`EDGE_*`) can both open and close emphasis (CommonMark 6.2), after the writer's references: a letter
   !< touching a closer of `_`, or a closer after punctuation, is written as a reference (`closer`), and so is a letter
   !< before an opener of `_`, or an opener before punctuation.
   integer(I4P), intent(in) :: c      !< Delimiter.
   integer(I4P), intent(in) :: before !< Class of the character before.
   integer(I4P), intent(in) :: after  !< Class of the character after.
   logical,      intent(in) :: closer !< The run closes its emphasis (else it opens it).
   logical                  :: both   !< Result.
   integer(I4P)             :: b, a   !< Classes, references applied.
   logical                  :: left   !< Left-flanking.
   logical                  :: right  !< Right-flanking.

   b = before
   a = after
   if (closer .and. a == EDGE_LETTER .and. (c == iachar('_') .or. b == EDGE_PUNCT)) a = EDGE_PUNCT
   if (.not. closer .and. b == EDGE_LETTER .and. (c == iachar('_') .or. a == EDGE_PUNCT)) b = EDGE_PUNCT
   left = a /= EDGE_SPACE .and. (a /= EDGE_PUNCT .or. b /= EDGE_LETTER)
   right = b /= EDGE_SPACE .and. (b /= EDGE_PUNCT .or. a /= EDGE_LETTER)
   if (c == iachar('_')) then
      both = left .and. right .and. b == EDGE_PUNCT .and. a == EDGE_PUNCT
   else
      both = left .and. right
   endif
   endfunction both_flanking

   function content_edge(doc, node, last) result(edge)
   !< Return the class (`EDGE_*`) of the first (or the `last`) character written in an emphasis's content: white space
   !< there is written as a reference, punctuation.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Emphasis.
   logical,                 intent(in) :: last !< The last character (else the first).
   integer(I4P)                        :: edge !< Class.
   character(:), allocatable           :: t    !< A text's literal.
   integer(I4P)                        :: n    !< A child.
   logical                             :: bs, bp !< White space, punctuation.

   edge = EDGE_PUNCT
   n = merge(doc%last_child(node), doc%first_child(node), last)
   do while (n /= 0_I4P)
      if (doc%node_kind(n) /= NODE_TEXT) return
      t = doc%literal(n)
      if (len(t) > 0) then
         call classify(t, merge(prev_char_start(t, len(t, kind=I4P) + 1_I4P), 1_I4P, last), bs, bp)
         if (.not. (bs .or. bp)) edge = EDGE_LETTER
         return
      endif
      n = merge(doc%prev_sibling(n), doc%next_sibling(n), last)
   enddo
   endfunction content_edge

   function sibling_edge(w, doc, node, after) result(edge)
   !< Return the class (`EDGE_*`) of the character written right after (or before) a node in its parent: its parent's
   !< delimiter when nothing is (punctuation).
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Node.
   logical,                 intent(in) :: after !< After it (else before it).
   integer(I4P)                        :: edge  !< Class.
   character(:), allocatable           :: t     !< A text's literal.
   integer(I4P)                        :: n     !< A sibling.
   logical                             :: bs, bp !< White space, punctuation.

   edge = EDGE_PUNCT
   n = merge(doc%next_sibling(node), doc%prev_sibling(node), after)
   do while (n /= 0_I4P)
      select case(doc%node_kind(n))
      case(NODE_SOFTBREAK)
         edge = EDGE_SPACE
         return
      case(NODE_LINEBREAK) ! after a node: a backslash, or spaces; before it: the line's start
         edge = EDGE_SPACE
         if (after .and. (w%canonical .or. doc%int_slot(n, SLOT_LINEBREAK_FORM) /= LINEBREAK_SPACES)) edge = EDGE_PUNCT
         return
      case(NODE_TEXT)
         t = doc%literal(n)
         if (len(t) > 0) then
            call classify(t, merge(1_I4P, prev_char_start(t, len(t, kind=I4P) + 1_I4P), after), bs, bp)
            if (bs) then
               edge = EDGE_SPACE
               ! one white space character, at an emphasis's edge too: written as a reference
               if (utf8_length(t(1:1)) == len(t, kind=I4P) .and. &
                   (doc%node_kind(doc%parent(n)) == NODE_EMPH .or. doc%node_kind(doc%parent(n)) == NODE_STRONG)) then
                  if ((after .and. written_last(doc, n)) .or. (.not. after .and. written_first(doc, n))) &
                     edge = EDGE_PUNCT
               endif
            elseif (.not. bp) then
               edge = EDGE_LETTER
            endif
            return
         endif
      case default
         return
      endselect
      n = merge(doc%next_sibling(n), doc%prev_sibling(n), after)
   enddo
   endfunction sibling_edge

   function written_first(doc, node) result(first)
   !< Return true if nothing is written before a node in its parent (only empty texts precede it).
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Node.
   logical                             :: first !< Result.
   integer(I4P)                        :: n     !< A sibling.

   first = .false.
   n = doc%prev_sibling(node)
   do while (n /= 0_I4P)
      if (doc%node_kind(n) /= NODE_TEXT) return
      if (len(doc%literal(n)) > 0) return
      n = doc%prev_sibling(n)
   enddo
   first = .true.
   endfunction written_first

   function written_last(doc, node) result(last)
   !< Return true if nothing is written after a node in its parent (only empty texts follow it).
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Node.
   logical                             :: last !< Result.
   integer(I4P)                        :: n    !< A sibling.

   last = .false.
   n = doc%next_sibling(node)
   do while (n /= 0_I4P)
      if (doc%node_kind(n) /= NODE_TEXT) return
      if (len(doc%literal(n)) > 0) return
      n = doc%next_sibling(n)
   enddo
   last = .true.
   endfunction written_last

   subroutine enter_inline(w, doc, slots, s, block, node, descend, skip, ierr, msg)
   !< Write what comes before an inline node's children (all of a leaf); a text node writes the whole run of text nodes
   !< it starts, and `skip` is the run's last node.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   type(inline_state),        intent(inout) :: s        !< State.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(out)   :: descend  !< Visit its children.
   integer(I4P),              intent(out)   :: skip     !< Last node of a text run, 0 if none.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< An extension's Markdown.
   character(:), allocatable                :: text     !< A run's text.
   character(:), allocatable                :: nc       !< The character after a run.
   integer(I4P)                             :: n        !< A node of a run.
   integer(I4P)                             :: next     !< What follows a run.
   integer(I4P)                             :: k        !< Counter.
   logical                                  :: handled  !< An extension wrote the event.
   logical                                  :: ref_last !< Write the run's last character as a reference.

   descend = .false.
   skip = 0_I4P
   call offer(doc, slots, node, .true., piece, handled, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (handled) then
      w%by_ext(node) = 1_I4P
      call put_raw(s, piece)
      descend = .true.
      return
   endif
   select case(doc%node_kind(node))
   case(NODE_TEXT)
      text = doc%literal(node)
      n = node
      do while (doc%next_sibling(n) /= 0_I4P)
         if (doc%node_kind(doc%next_sibling(n)) /= NODE_TEXT) exit
         if (size(slots) > 0) then
            call offer(doc, slots, doc%next_sibling(n), .true., piece, handled, ierr, msg)
            if (ierr /= BARUCH_OK) return
            if (handled) exit
         endif
         n = doc%next_sibling(n)
         text = text//doc%literal(n)
      enddo
      skip = n
      call what_follows(w, doc, block, n, next, nc)
      ! an opener after a letter flanks only when its content starts with no punctuation and it is not `_`
      ref_last = .false.
      if (next == NEXT_OPEN) ref_last = w%mark(doc%next_sibling(n)) == iachar('_') .or. &
                                        starts_with_punctuation(w, doc, doc%next_sibling(n))
      call escape_text(w, s, text, next, nc, is_last_in_block(doc, block, n), ref_last)
   case(NODE_SOFTBREAK)
      call put_raw(s, LF)
      s%indent = line_indent(w, doc, block, doc%next_sibling(node))
   case(NODE_LINEBREAK)
      if (.not. w%canonical .and. doc%int_slot(node, SLOT_LINEBREAK_FORM) == LINEBREAK_SPACES) then
         call put_raw(s, repeat(' ', max(2_I4P, doc%int_slot(node, SLOT_LINEBREAK_SPACES)))//LF)
      else
         call put_raw(s, BACKSLASH//LF)
      endif
      s%indent = line_indent(w, doc, block, doc%next_sibling(node))
   case(NODE_CODE)
      call write_code_span(w, doc, s, node)
   case(NODE_HTML_INLINE)
      text = doc%literal(node)
      s%ticks = s%ticks - count_char(text, '`')
      call put_guarded(s, text)
   case(NODE_EMPH, NODE_STRONG)
      k = merge(1_I4P, 2_I4P, doc%node_kind(node) == NODE_EMPH)
      call add_delimiter(s, k, w%mark(node), node)
      call put_raw(s, repeat(char1(w%mark(node)), k))
      s%delim_end = s%buf%used
      s%delim_open = .true.
      k = merge(1_I4P, 2_I4P, w%mark(node) == iachar('*'))
      s%inside(k) = s%inside(k) + 1_I4P
      s%emphs = s%emphs + 1_I4P
      if (s%emphs > size(s%emph, 2, kind=I4P)) call grow_matrix(s%emph)
      s%emph(:, s%emphs) = [w%mark(node), s%openers(1), s%openers(2)]
      descend = .true.
   case(NODE_LINK, NODE_IMAGE)
      if (w%mark(node) == 1_I4P .and. doc%node_kind(node) == NODE_LINK) then
         s%ticks = s%ticks - count_char(doc%destination(node)//doc%title(node)//doc%label(node), '`')
         text = doc%literal(doc%first_child(node))
         s%ticks = s%ticks - count_char(text, '`')
         call put_raw(s, '<'//text//'>')
         return
      endif
      if (doc%node_kind(node) == NODE_IMAGE) then
         call put_raw(s, '![')
      else
         call put_raw(s, '[')
      endif
      s%depth = s%depth + 1_I4P
      if (s%depth > size(s%link_start, kind=I4P)) then
         call grow(s%link_start)
         call grow(s%seg)
         call grow_matrix(s%saved)
      endif
      s%link_start(s%depth) = s%buf%used
      s%segs = s%segs + 1_I4P
      s%seg(s%depth) = s%segs
      s%saved(1:2, s%depth) = s%openers ! a link's text is read apart: its delimiters pair among themselves
      s%saved(3:4, s%depth) = s%inside
      s%openers = 0_I4P
      s%inside = 0_I4P
      descend = .true.
   case default ! an extension kind no extension writes: transparent
      descend = doc%first_child(node) /= 0_I4P
      if (.not. descend) then
         text = doc%literal(node)
         s%ticks = s%ticks - 1_I4P
         if (len(text) > 0) then
            call what_follows(w, doc, block, node, next, nc)
            call escape_text(w, s, text, next, nc, is_last_in_block(doc, block, node))
         endif
      endif
   endselect
   endsubroutine enter_inline

   subroutine leave_inline(w, doc, slots, s, block, node, ierr, msg)
   !< Write what comes after an inline node's children: a closing delimiter, the end of a link.
   type(md_writer),           intent(inout) :: w        !< Writer.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   type(inline_state),        intent(inout) :: s        !< State.
   integer(I4P),              intent(in)    :: block    !< Block.
   integer(I4P),              intent(in)    :: node     !< Node.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   character(:), allocatable                :: piece    !< An extension's Markdown.
   integer(I4P)                             :: k        !< Delimiter index.
   logical                                  :: handled  !< An extension wrote the event.

   if (doc%node_kind(node) == NODE_TEXT .and. w%by_ext(node) == 0_I4P) return
   call offer(doc, slots, node, .false., piece, handled, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (w%by_ext(node) /= 0_I4P) then ! an extension entered it: it leaves it
      if (handled) call put_raw(s, piece)
      return
   endif
   if (doc%node_kind(node) == NODE_LINK .or. doc%node_kind(node) == NODE_IMAGE) then ! written after its text
      if (w%mark(node) /= 1_I4P) s%ticks = s%ticks - &
                                 count_char(doc%destination(node)//doc%title(node)//doc%label(node), '`')
   endif
   if (handled) then ! the core entered it, an extension writes its end
      call put_raw(s, piece)
      if (doc%node_kind(node) == NODE_LINK .or. doc%node_kind(node) == NODE_IMAGE) then
         if (w%mark(node) /= 1_I4P) call close_link_text(s)
      endif
      return
   endif
   select case(doc%node_kind(node))
   case(NODE_EMPH, NODE_STRONG)
      call add_delimiter(s, merge(1_I4P, 2_I4P, doc%node_kind(node) == NODE_EMPH), w%mark(node), node)
      call put_raw(s, repeat(char1(w%mark(node)), merge(1, 2, doc%node_kind(node) == NODE_EMPH)))
      s%delim_end = s%buf%used
      s%delim_open = .false.
      k = merge(1_I4P, 2_I4P, w%mark(node) == iachar('*'))
      s%inside(k) = s%inside(k) - 1_I4P
      if (s%emphs > 0_I4P) then ! the delimiters between its opener and its closer are gone
         s%openers = s%emph(2:3, s%emphs)
         s%emphs = s%emphs - 1_I4P
      endif
   case(NODE_LINK, NODE_IMAGE)
      if (doc%node_kind(node) == NODE_LINK .and. w%mark(node) == 1_I4P) return ! an autolink, written whole
      call finish_link(w, doc, s, block, node)
   endselect
   endsubroutine leave_inline

   subroutine finish_link(w, doc, s, block, node)
   !< Write the end of a link or an image: a reference (full, collapsed, shortcut) when recorded and the definition its
   !< label finds is the link's, else inline.
   type(md_writer),         intent(inout) :: w      !< Writer.
   type(markdown_document), intent(in)    :: doc    !< Document.
   type(inline_state),      intent(inout) :: s      !< State.
   integer(I4P),            intent(in)    :: block  !< Block.
   integer(I4P),            intent(in)    :: node   !< Link or image.
   character(:), allocatable              :: raw    !< Its text as written.
   character(:), allocatable              :: dest   !< Destination.
   character(:), allocatable              :: title  !< Title.
   character(:), allocatable              :: label  !< Label as recorded.
   character(:), allocatable              :: nc     !< The character after it.
   integer(I4P)                           :: form   !< Recorded form.
   integer(I4P)                           :: next   !< What follows it.
   logical                                :: by_text  !< Its text finds its definition.
   logical                                :: by_label !< Its label finds its definition.

   raw = s%buf%chars(s%link_start(s%depth)+1:s%buf%used)
   call close_link_text(s)
   if (doc%node_kind(node) == NODE_LINK) call deactivate_brackets(s) ! a link deactivates the `[` before it
   dest = doc%destination(node)
   title = doc%title(node)
   form = 0_I4P
   if (.not. w%canonical) form = doc%int_slot(node, SLOT_LINK_FORM)
   if (form == LINK_FULL .or. form == LINK_COLLAPSED .or. form == LINK_SHORTCUT) then
      label = doc%label(node)
      by_label = .false.
      if (len(label) > 0) by_label = defines(doc, w%refs%lookup(label), dest, title)
      by_text = defines(doc, w%refs%lookup(raw), dest, title)
      if (form == LINK_SHORTCUT .and. by_text) then
         call what_follows(w, doc, block, node, next, nc)
         if (next /= NEXT_LINK .and. .not. tail_follows(doc, node, nc)) then
            call put_raw(s, ']')
            return
         endif
      endif
      if (form /= LINK_FULL .and. by_text) then
         call put_raw(s, '][]')
         return
      endif
      if (by_label) then
         call put_raw(s, ']['//label//']')
         return
      endif
      if (by_text) then
         call put_raw(s, '][]')
         return
      endif
   endif
   if (len(dest) == 0 .and. len(title) == 0) then
      call put_raw(s, ']()')
   else
      call put_guarded(s, ']('//destination_text(dest, .false.)//title_text(title)//')')
   endif
   endsubroutine finish_link

   function tail_follows(doc, node, nc) result(tail)
   !< Return true if what follows a link written `[label]` would make it another link: an inline link's tail (`(` in
   !< the text after it), a reference (`[`), a definition (`:`, when the link starts its paragraph).
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Link.
   character(*),            intent(in) :: nc   !< The character after it.
   logical                             :: tail !< Result.
   character(:), allocatable           :: t    !< The text after it.

   tail = nc == '(' .or. nc == '[' .or. nc == ':' ! `[`: any label after it is read as the link's (cmark)
   if (.not. tail .or. doc%next_sibling(node) == 0_I4P) return
   if (doc%node_kind(doc%next_sibling(node)) /= NODE_TEXT) return
   t = doc%literal(doc%next_sibling(node))
   select case(nc)
   case('(')
      tail = inline_tail(t, 1_I4P, .true.)
   case(':')
      tail = doc%prev_sibling(node) == 0_I4P
   endselect
   endfunction tail_follows

   pure subroutine deactivate_brackets(s)
   !< Deactivate the active `[` written as text before a link (CommonMark 6.3): not those of a `![`, an image may hold
   !< links. An inactive one stays on the stack (negative): the next `]` pops it and is text.
   type(inline_state), intent(inout) :: s !< State.
   integer(I4P)                      :: k !< A `[`.

   do k=1_I4P, s%lones
      if (s%lone(k) < 0_I4P) cycle ! inactive already
      if (s%lone(k) >= 1_I4P) then
         if (s%buf%chars(s%lone(k):s%lone(k)) == '!') cycle
      endif
      s%lone(k) = -s%lone(k) - 1_I4P ! inactive, on the stack still: a `]` after it pops it, and is text
   enddo
   endsubroutine deactivate_brackets

   pure subroutine close_link_text(s)
   !< Close the innermost link text: the emphasis counts of outside it come back.
   type(inline_state), intent(inout) :: s !< State.

   s%openers = s%saved(1:2, s%depth)
   s%inside = s%saved(3:4, s%depth)
   s%depth = s%depth - 1_I4P
   endsubroutine close_link_text

   function defines(doc, definition, dest, title) result(same)
   !< Return true if a definition (0: none) gives a destination and a title.
   type(markdown_document), intent(in) :: doc        !< Document.
   integer(I4P),            intent(in) :: definition !< Definition node.
   character(*),            intent(in) :: dest       !< Destination.
   character(*),            intent(in) :: title      !< Title.
   logical                             :: same       !< Result.

   same = .false.
   if (definition == 0_I4P) return
   same = doc%destination(definition) == dest .and. len(doc%destination(definition)) == len(dest) .and. &
          doc%title(definition) == title .and. len(doc%title(definition)) == len(title)
   endfunction defines

   function is_autolink(w, doc, node) result(auto)
   !< Return true if a link is written as an autolink: recorded so (or canonical), with one text child that is its
   !< destination (or its email address, the destination `mailto:` it), read back as such, and no title.
   type(md_writer),         intent(in) :: w    !< Writer.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: node !< Link.
   logical                             :: auto !< Result.
   character(:), allocatable           :: text !< Its text.
   character(:), allocatable           :: dest !< Its destination.
   integer(I4P)                        :: t    !< Its child.

   auto = .false.
   if (.not. w%canonical .and. doc%int_slot(node, SLOT_LINK_FORM) /= LINK_AUTOLINK) return
   t = doc%first_child(node)
   if (t == 0_I4P .or. len(doc%title(node)) > 0) return
   if (doc%node_kind(t) /= NODE_TEXT .or. doc%next_sibling(t) /= 0_I4P) return
   text = doc%literal(t)
   dest = doc%destination(node)
   if (len(text) == 0) return
   if (decode_references(text) /= text) return
   if (dest == text .and. len(dest) == len(text)) then
      auto = scan_autolink_uri('<'//text//'>') == len(text, kind=I4P) + 2_I4P
   elseif (dest == 'mailto:'//text .and. len(dest) == len(text) + 7) then
      auto = scan_autolink_email('<'//text//'>') == len(text, kind=I4P) + 2_I4P
   endif
   endfunction is_autolink

   subroutine write_code_span(w, doc, s, node)
   !< Write a code span: its content as written (recorded: padding and line endings), else its literal with a space
   !< inside each end when it starts or ends with a backtick, or with a space at both ends (the parser strips one
   !< there); between backtick strings of the recorded length, else the shortest the content does not hold.
   type(md_writer),         intent(in)    :: w    !< Writer.
   type(markdown_document), intent(in)    :: doc  !< Document.
   type(inline_state),      intent(inout) :: s    !< State.
   integer(I4P),            intent(in)    :: node !< Code span.
   character(:), allocatable              :: code !< Content as written.
   character(:), allocatable              :: raw  !< Content as recorded.
   integer(I4P)                           :: n    !< Backticks of the delimiters.
   logical                                :: pad  !< Pad with spaces.

   code = doc%literal(node)
   n = 0_I4P
   raw = ''
   if (.not. w%canonical) then
      raw = doc%string_slot(node, STRING_RAW)
      n = doc%int_slot(node, SLOT_CODE_TICKS)
   endif
   pad = .false.
   if (len(raw) > 0) then
      if (raw(1:1) /= '`' .and. raw(len(raw):len(raw)) /= '`' .and. code_content(raw) == code .and. &
          len(code_content(raw)) == len(code)) then
         code = raw
      else
         raw = ''
      endif
   endif
   if (len(raw) == 0 .and. len(code) > 0) then
      if (code(1:1) == '`' .or. code(len(code):len(code)) == '`') pad = .true.
      if (code(1:1) == ' ' .and. code(len(code):len(code)) == ' ' .and. verify(code, ' ') > 0) pad = .true.
   endif
   if (n < 1_I4P .or. has_tick_run(code, n)) then
      n = 1_I4P
      do while (has_tick_run(code, n))
         n = n + 1_I4P
      enddo
   endif
   s%ticks = s%ticks - count_char(code, '`') - 2_I4P
   if (pad) code = ' '//code//' '
   call put_guarded(s, repeat('`', n)//code//repeat('`', n))
   endsubroutine write_code_span

   subroutine what_follows(w, doc, block, node, next, nc)
   !< Return what is written right after an inline node: its class (`NEXT_*`) and its first character (empty at the end
   !< of the block).
   type(md_writer),           intent(in)  :: w     !< Writer.
   type(markdown_document),   intent(in)  :: doc   !< Document.
   integer(I4P),              intent(in)  :: block !< Block.
   integer(I4P),              intent(in)  :: node  !< Node.
   integer(I4P),              intent(out) :: next  !< Class.
   character(:), allocatable, intent(out) :: nc    !< First character.
   integer(I4P)                           :: n     !< The next node, or the parent.

   n = doc%next_sibling(node)
   if (n /= 0_I4P) then
      next = NEXT_OTHER
      select case(doc%node_kind(n))
      case(NODE_SOFTBREAK)
         next = NEXT_LINE
         nc = LF
      case(NODE_LINEBREAK)
         next = NEXT_LINE
         nc = ' '
         if (w%canonical .or. doc%int_slot(n, SLOT_LINEBREAK_FORM) /= LINEBREAK_SPACES) nc = BACKSLASH
      case(NODE_CODE)
         nc = '`'
      case(NODE_HTML_INLINE)
         nc = '<'
      case(NODE_EMPH, NODE_STRONG)
         next = NEXT_OPEN
         nc = char1(w%mark(n))
      case(NODE_LINK)
         if (w%mark(n) == 1_I4P) then
            nc = '<'
         else
            next = NEXT_LINK
            nc = '['
         endif
      case(NODE_IMAGE)
         nc = '!'
      case(NODE_TEXT)
         nc = doc%literal(n)
         if (len(nc) > 0) nc = nc(1:1)
      case default
         nc = 'a'
      endselect
      return
   endif
   n = doc%parent(node)
   if (n == block .or. n == 0_I4P) then
      next = NEXT_END
      nc = ''
      return
   endif
   select case(doc%node_kind(n))
   case(NODE_EMPH, NODE_STRONG)
      next = NEXT_CLOSE
      nc = char1(w%mark(n))
   case(NODE_LINK, NODE_IMAGE)
      next = NEXT_OTHER
      nc = ']'
   case default
      next = NEXT_OTHER
      nc = 'a'
   endselect
   endsubroutine what_follows

   function is_last_in_block(doc, block, node) result(is)
   !< Return true if nothing is written after an inline node in its block (an emphasis's closer, a link's end are).
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: block !< Block.
   integer(I4P),            intent(in) :: node  !< Node.
   logical                             :: is    !< Result.
   integer(I4P)                        :: n     !< Ancestor.

   is = .false.
   n = node
   do while (n /= block .and. n /= 0_I4P)
      if (doc%next_sibling(n) /= 0_I4P) return
      n = doc%parent(n)
      if (n == block .or. n == 0_I4P) exit
      select case(doc%node_kind(n)) ! its closing delimiter, a link's `](...)`, follows
      case(NODE_EMPH, NODE_STRONG, NODE_LINK, NODE_IMAGE)
         return
      endselect
   enddo
   is = .true.
   endfunction is_last_in_block

   subroutine escape_text(w, s, text, next, nc, at_end, ref_last)
   !< Write a run of text escaped: what follows it is `next` (a `NEXT_*` class) and starts with `nc`; `at_end` when
   !< nothing follows it in the block. A letter (a character neither white space nor punctuation) touching an
   !< emphasis's delimiter from outside is written as a numeric character reference where the delimiter would not
   !< flank otherwise (CommonMark 6.2: a reference is read as its character, but the delimiter run sees `&` or `;`,
   !< punctuation): the first character after a closer of `_` or of a content ending with punctuation, and, with
   !< `ref_last`, the last character before an opener of `_` or of a content starting with punctuation.
   type(md_writer),    intent(in)    :: w      !< Writer.
   type(inline_state), intent(inout) :: s      !< State.
   character(*),       intent(in)    :: text   !< Text.
   integer(I4P),       intent(in)    :: next   !< What follows: class.
   character(*),       intent(in)    :: nc     !< What follows: first character.
   logical,            intent(in)    :: at_end !< Nothing follows in the block.
   logical, optional,  intent(in)    :: ref_last !< An opener follows that needs punctuation before it.
   character(:), allocatable         :: probe  !< The line the run starts.
   character(:), allocatable         :: pc     !< The character before a delimiter run.
   character(:), allocatable         :: ac     !< The character after it.
   integer(I4P)                      :: n      !< Length.
   integer(I4P)                      :: i, k   !< Positions.
   integer(I4P)                      :: e      !< Position to escape at a line start, 0 if none.
   integer(I4P)                      :: l      !< Bytes of a character.
   integer(I4P)                      :: d      !< Index of a delimiter character (1 `*`, 2 `_`).
   integer(I4P)                      :: r      !< Length of a backtick string.
   integer(I4P)                      :: last_gt !< Position of the text's last `>`, 0 if none.
   integer(I4P), allocatable         :: mate(:) !< The bracket matching each bracket, 0 if none.
   integer(I4P)                      :: closer_after(2) !< Start of the text's last run of `*`, `_` that can close.
   logical                           :: line_end   !< A line end follows the run.
   logical                           :: open_start !< The run starts an emphasis's content.
   logical                           :: esc    !< Escape.
   logical                           :: bs, bp       !< Space and punctuation of a character.
   logical                           :: can_open, can_close !< A delimiter run can open, close emphasis.
   character                         :: c      !< Current byte.
   integer(I4P)                      :: ref_first_at !< The first character, as a reference (1), or not (0).
   integer(I4P)                      :: ref_last_at  !< The last character's first byte, as a reference, or 0.

   n = len(text, kind=I4P)
   if (n == 0_I4P) return
   call reference_ends(s, text, ref_first_at, ref_last_at)
   if (present(ref_last)) then
      if (.not. ref_last) ref_last_at = 0_I4P
   else
      ref_last_at = 0_I4P
   endif
   if (ref_last_at == ref_first_at) ref_last_at = 0_I4P
   line_end = next == NEXT_END .or. next == NEXT_LINE
   open_start = s%delim_open .and. s%delim_end == s%buf%used
   s%ticks = s%ticks - count_char(text, '`')
   e = 0_I4P
   if (s%mode == INLINE_LINES .and. s%line_start .and. s%indent < 4_I4P .and. .not. is_space_or_tab(text(1:1))) then
      k = index(text, LF)
      if (k > 0) then
         probe = text(1:k-1)
      elseif (line_end) then
         probe = text
      else
         probe = text//'a'
      endif
      e = block_start_escape(probe, .not. s%first_line)
      if (e == 0_I4P .and. s%first_line) e = bullets_break(w, probe)
   endif
   call match_brackets(text, mate)
   call last_closers(text, s%buf%last_char(), nc, closer_after)
   last_gt = index(text, '>', back=.true.)
   i = 1_I4P
   do while (i <= n)
      c = text(i:i)
      if (i == ref_first_at .or. i == ref_last_at) then
         l = min(utf8_length(c), n - i + 1_I4P)
         call put_raw(s, '&#'//istr(utf8_code(text(i:i+l-1)))//';')
         i = i + l
         cycle
      endif
      if (i == e) then
         call put_raw(s, BACKSLASH//c)
         i = i + 1_I4P
         cycle
      endif
      select case(c)
      case(' ', TAB)
         esc = .false.
         if (s%line_start .and. s%mode == INLINE_LINES) esc = .true.
         if (i == 1_I4P .and. open_start) esc = .true.
         if (i == n .and. next == NEXT_CLOSE) esc = .true.
         if (line_end .and. (s%mode == INLINE_LINES .or. s%mode == INLINE_ATX)) then
            if (verify(text(i:), ' '//TAB) == 0) esc = .true.
         endif
         if (s%mode == INLINE_ATX .and. s%buf%used == 0_I4P) esc = .true.
         if (esc) then
            call put_raw(s, '&#'//istr(iachar(c, kind=I4P))//';')
         else
            call put_raw(s, c)
         endif
      case(LF, CR)
         call put_raw(s, '&#'//istr(iachar(c, kind=I4P))//';')
      case(BACKSLASH)
         if (i < n) then
            ! before punctuation, or a character that may be written as a reference (white space, a letter as one)
            call classify(text, i + 1_I4P, bs, bp)
            esc = is_ascii_punctuation(text(i+1:i+1)) .or. i + 1_I4P == ref_last_at .or. bs
         else
            esc = next == NEXT_LINE
            if (len(nc) > 0) esc = esc .or. is_ascii_punctuation(nc(1:1))
         endif
         if (esc) then
            call put_raw(s, BACKSLASH//BACKSLASH)
         else
            call put_raw(s, BACKSLASH)
         endif
      case('*', '_')
         k = i
         do while (k <= n)
            if (text(k:k) /= c) exit
            k = k + 1_I4P
         enddo
         pc = s%buf%last_char()
         if (k <= n) then
            ac = utf8_at(text, k)
            if (k == ref_last_at) ac = '&' ! written as a reference, the run sees `&`
            ! white space written as a reference after the run: the run sees `&`
            if (ac == ' ' .or. ac == TAB) then
               if ((k == n .and. next == NEXT_CLOSE) .or. (line_end .and. (s%mode == INLINE_LINES .or. &
                   s%mode == INLINE_ATX) .and. verify(text(k:), ' '//TAB) == 0)) ac = '&'
            endif
         else
            ac = nc
         endif
         call run_flanking(c, pc, ac, can_open, can_close)
         d = merge(1_I4P, 2_I4P, c == '*')
         ! a closer pairs with an opener before it (a text's, or an emphasis's still open); an opener with a closer
         ! after it: of the text's later runs (each checks the openers before it) or of an emphasis around it
         esc = can_close .and. (s%openers(d) > 0_I4P .or. s%inside(d) > 0_I4P)
         if (can_open .and. s%both_open(d) .and. .not. s%merge_runs) esc = .true. ! an emphasis's opener could close it
         if (s%paranoid .and. (can_open .or. can_close)) esc = .true.
         if (can_open .and. s%emphs > 0_I4P) esc = esc .or. s%emph(1, s%emphs) == iachar(c) .or. &
                                                    s%emph(1, s%emphs) == 0_I4P
         if (.not. s%merge_runs) then ! touching a delimiter of its character, it would join the delimiter's run
            if (i == 1_I4P .and. s%delim_end == s%buf%used .and. len(pc) > 0) esc = esc .or. pc == c
            if (k > n .and. (next == NEXT_OPEN .or. next == NEXT_CLOSE)) esc = esc .or. nc == c
         endif
         if (index(w%ext_chars, c) > 0) esc = .true.
         if (.not. esc .and. can_open) then ! a closer later in the text: escape the opener rather than the closer
            if (closer_after(d) >= k) then
               esc = .true.
            else
               s%openers(d) = s%openers(d) + 1_I4P
            endif
         endif
         if (.not. esc) call add_delimiter(s, k - i, iachar(c, kind=I4P), 0_I4P)
         do while (i < k)
            if (esc) then
               call put_raw(s, BACKSLASH//c)
            else
               call put_raw(s, c)
            endif
            i = i + 1_I4P
         enddo
         cycle
      case('`')
         k = i
         do while (k <= n)
            if (text(k:k) /= c) exit
            k = k + 1_I4P
         enddo
         r = k - i
         esc = s%ticks > 0_I4P .or. tick_closed(text(k:), r)
         pc = s%buf%last_char()
         if (pc == '`') esc = .true.
         if (k > n .and. nc == '`') esc = .true.
         do while (i < k)
            if (esc) then
               call put_raw(s, BACKSLASH//c)
            else
               call put_raw(s, c)
            endif
            i = i + 1_I4P
         enddo
         cycle
      case('[')
         esc = s%depth > 0_I4P
         if (mate(i) > 0_I4P) then
            esc = pair_links(w, text, i, mate(i), next, nc, s%buf%used == 0_I4P .and. s%mode == INLINE_LINES)
            if (esc) mate(mate(i)) = 0_I4P ! its `]` is now a lone one
         endif
         if (.not. esc .and. s%escape_first .and. s%buf%used == 0_I4P) then
            esc = .true.
            if (mate(i) > 0_I4P) mate(mate(i)) = 0_I4P
         endif
         if (.not. esc .and. s%depth == 0_I4P) then ! active: a `]` after it may close it
            s%lones = s%lones + 1_I4P
            if (s%lones > size(s%lone, kind=I4P)) call grow(s%lone)
            s%lone(s%lones) = s%buf%used
            if (mate(i) > 0_I4P) mate(mate(i)) = 0_I4P
         endif
         if (esc) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case(']')
         if (mate(i) > 0_I4P) then ! a pair inside a link's text that makes no link
            esc = .false.
         elseif (s%depth > 0_I4P) then ! it would end the link's text
            esc = .true.
         elseif (s%lones > 0_I4P) then ! it closes the last active `[`: a link, or text and that `[` is gone
            if (s%lone(s%lones) < 0_I4P) then ! an inactive one: popped, the `]` is text
               esc = .false.
            else
               esc = closes_link(w, s, text, i, next, nc)
            endif
            if (.not. esc) s%lones = s%lones - 1_I4P
         else
            esc = .false.
         endif
         if (esc) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case('!')
         if (i == n .and. next == NEXT_LINK) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case('&')
         esc = scan_entity_like(text, i)
         if (esc) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case('<')
         esc = opens_html(text(i:), at_end, last_gt > i)
         if (esc) then
            call put_raw(s, BACKSLASH//c)
         else
            call put_raw(s, c)
         endif
      case default
         l = utf8_length(text(i:i))
         if (l == 1_I4P) then
            if (index(w%ext_chars, c) > 0 .and. is_ascii_punctuation(c)) then
               call put_raw(s, BACKSLASH//c)
            else
               call put_raw(s, c)
            endif
         else
            l = min(l, n - i + 1_I4P)
            esc = .false.
            if ((i == 1_I4P .and. open_start) .or. (i + l - 1_I4P == n .and. next == NEXT_CLOSE)) then
               call classify(text(i:i+l-1), 1_I4P, bs, bp)
               esc = bs
            endif
            if (esc) then
               call put_raw(s, '&#'//istr(utf8_code(text(i:i+l-1)))//';')
            else
               call put_raw(s, text(i:i+l-1))
            endif
            i = i + l
            cycle
         endif
      endselect
      i = i + 1_I4P
   enddo
   endsubroutine escape_text

   subroutine reference_ends(s, text, first_at, last_at)
   !< Return which ends of a text are letters (neither white space nor punctuation) that would keep an emphasis's
   !< delimiter next to them from flanking: `first_at` 1 when the text follows a closer of `_`, or a closer whose
   !< content ends with punctuation (the buffer ends with the closer); `last_at`, the first byte of the last character
   !< when it is a letter (the caller knows what follows). 0 for none.
   type(inline_state), intent(in)  :: s        !< State.
   character(*),       intent(in)  :: text     !< Text, not empty.
   integer(I4P),       intent(out) :: first_at !< The first character, 1, or 0.
   integer(I4P),       intent(out) :: last_at  !< The last character's first byte, or 0.
   character                       :: c        !< The closer's character.
   integer(I4P)                    :: k        !< Start of the closing run.
   integer(I4P)                    :: u        !< Bytes written.
   logical                         :: bs, bp   !< White space, punctuation.

   first_at = 0_I4P
   last_at = prev_char_start(text, len(text, kind=I4P) + 1_I4P)
   call classify(text, last_at, bs, bp)
   if (bs .or. bp) last_at = 0_I4P
   u = s%buf%used
   if (u < 2_I4P .or. s%delim_open .or. s%delim_end /= u) return
   c = s%buf%chars(u:u)
   if (c /= '*' .and. c /= '_') return
   call classify(text, 1_I4P, bs, bp)
   if (bs .or. bp) return
   k = u
   do while (k > 1_I4P)
      if (s%buf%chars(k-1:k-1) /= c) exit
      k = k - 1_I4P
   enddo
   if (k == 1_I4P) return
   call classify(s%buf%chars(1:k-1), prev_char_start(s%buf%chars(1:k-1), k), bs, bp)
   if (c == '_' .or. bp) first_at = 1_I4P
   endsubroutine reference_ends

   function starts_with_punctuation(w, doc, node) result(punct)
   !< Return true if what follows the opening delimiter run of an emphasis is written starting with punctuation (or
   !< white space, written as a reference): anything but a text starting with a letter. Empty texts write nothing and
   !< are skipped; an emphasis first in the content with the same delimiter is in the same run, its content is read;
   !< a text of one letter before an opener that needs punctuation before it is written as a reference (no
   !< recursion: the chain of such openers is followed).
   type(md_writer),         intent(in) :: w     !< Writer.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: node  !< Emphasis.
   logical                             :: punct !< Result.
   character(:), allocatable           :: t     !< A text's literal.
   integer(I4P)                        :: m     !< The emphasis whose content is read.
   integer(I4P)                        :: n     !< A child.
   integer(I4P)                        :: e     !< The node after a text.
   logical                             :: bs, bp !< White space, punctuation.

   punct = .true.
   m = node
   n = doc%first_child(m)
   do while (n /= 0_I4P)
      if ((doc%node_kind(n) == NODE_EMPH .or. doc%node_kind(n) == NODE_STRONG) .and. w%mark(n) == w%mark(m)) then
         m = n
         n = doc%first_child(n)
         cycle
      endif
      if (doc%node_kind(n) /= NODE_TEXT) return
      t = doc%literal(n)
      if (len(t) == 0) then
         n = doc%next_sibling(n)
         cycle
      endif
      call classify(t, 1_I4P, bs, bp)
      punct = bs .or. bp
      if (punct .or. utf8_length(t(1:1)) /= len(t, kind=I4P)) return
      ! one letter before an opener that needs punctuation before it: written as a reference
      e = doc%next_sibling(n)
      do while (e /= 0_I4P) ! (empty texts write nothing)
         if (doc%node_kind(e) /= NODE_TEXT) exit
         if (len(doc%literal(e)) > 0) exit
         e = doc%next_sibling(e)
      enddo
      if (e == 0_I4P) return
      if (doc%node_kind(e) /= NODE_EMPH .and. doc%node_kind(e) /= NODE_STRONG) return
      punct = .true.
      if (w%mark(e) == iachar('_')) return
      m = e
      n = doc%first_child(m)
   enddo
   endfunction starts_with_punctuation

   function closes_link(w, s, text, j, next, nc) result(links)
   !< Return true if a `]` of a text, `text(j)`, would close the last active `[` into a link (the bytes between them
   !< already written).
   type(md_writer),    intent(in) :: w     !< Writer.
   type(inline_state), intent(in) :: s     !< State.
   character(*),       intent(in) :: text  !< Text.
   integer(I4P),       intent(in) :: j     !< The `]`.
   integer(I4P),       intent(in) :: next  !< What follows the text: class.
   character(*),       intent(in) :: nc    !< What follows the text: first character.
   logical                        :: links !< Result.
   character(:), allocatable      :: label !< The bytes between the brackets.
   integer(I4P)                   :: open_ !< Position of the `[`.

   open_ = s%lone(s%lones)
   if (s%buf%used - open_ > 4_I4P * LABEL_MAX) then ! no label is that long: no reference, no definition
      label = repeat('a', LABEL_MAX + 1_I4P)
   else
      label = s%buf%chars(open_+2:s%buf%used) ! the `[` is at `open_ + 1`
   endif
   links = after_bracket(w, text, j, next, nc, label, open_ == 0_I4P .and. s%mode == INLINE_LINES)
   endfunction closes_link

   function after_bracket(w, text, j, next, nc, label, block_start) result(links)
   !< Return true if what follows a `]` at `text(j)` makes a link of the brackets holding `label`: an inline link's
   !< tail, a reference to a defined label, the label itself defined (a shortcut), a link node; a definition's `:` at
   !< the start of a paragraph. What cannot be told within the text counts as a link.
   type(md_writer), intent(in) :: w           !< Writer.
   character(*),    intent(in) :: text        !< Text.
   integer(I4P),    intent(in) :: j           !< The `]`.
   integer(I4P),    intent(in) :: next        !< What follows the text: class.
   character(*),    intent(in) :: nc          !< What follows the text: first character.
   character(*),    intent(in) :: label       !< The bytes between the brackets, as written.
   logical,         intent(in) :: block_start !< The `[` starts a paragraph.
   logical                     :: links       !< Result.
   integer(I4P)                :: k           !< Position.

   links = .false.
   if (len(label) > 0) links = w%refs%lookup(label) /= 0_I4P
   if (links) return
   if (j == len(text)) then
      links = next == NEXT_LINK .or. nc == '(' .or. nc == '['
      return
   endif
   select case(text(j+1:j+1))
   case('(')
      links = inline_tail(text, j + 1_I4P, next /= NEXT_END)
   case('[')
      k = index(text(j+2:), ']')
      if (k == 0) then
         links = .true.
      elseif (k > 1) then
         links = w%refs%lookup(text(j+2:j+k)) /= 0_I4P
      endif
   case(':')
      if (block_start) links = defines_there('['//label//text(j:), next)
   endselect
   endfunction after_bracket

   function reads_as_definition(text) result(defines)
   !< Return true if a paragraph written as `text` would start with a link reference definition.
   character(*), intent(in)  :: text        !< The paragraph's content, as written.
   logical                   :: defines     !< Result.
   character(:), allocatable :: destination !< Destination.
   character(:), allocatable :: title       !< Title.
   integer(I4P)              :: after       !< Position after a definition, 0 if none.
   integer(I4P)              :: a, b        !< Bounds of its label.

   call parse_definition(text//LF, 1_I4P, after, a, b, destination, title)
   defines = after > 0_I4P
   endfunction reads_as_definition

   function defines_there(text, next) result(defines)
   !< Return true if a paragraph starting with `text` would start with a link reference definition: when more follows
   !< on later lines (`next` not `NEXT_END`), whenever it could.
   character(*), intent(in)  :: text        !< The paragraph's text, from its start.
   integer(I4P), intent(in)  :: next        !< What follows the text: class.
   logical                   :: defines     !< Result.
   character(:), allocatable :: destination !< Destination.
   character(:), allocatable :: title       !< Title.
   integer(I4P)              :: after       !< Position after a definition, 0 if none.
   integer(I4P)              :: a, b        !< Bounds of its label.

   defines = next /= NEXT_END
   if (defines) return
   call parse_definition(text//LF, 1_I4P, after, a, b, destination, title)
   defines = after > 0_I4P
   endfunction defines_there

   function inline_tail(text, k, more) result(tail)
   !< Return true if an inline link's tail, `(destination "title")`, starts at `text(k)`, or might (the text ends first).
   !< A `<` the writer escapes is read as an ordinary character: each reading is tried, the text as it is, its
   !< backslashes escaped as written, its first `<` escaped (a plain destination), the other `<` of a pointy
   !< destination escaped.
   character(*), intent(in)  :: text  !< Text.
   integer(I4P), intent(in)  :: k     !< Position of the `(`.
   logical,      intent(in)  :: more  !< Something is written after the text, in its block.
   logical                   :: tail  !< Result.
   character(:), allocatable :: other !< Another reading.
   integer(I4P)              :: pos   !< Position.
   integer(I4P)              :: i     !< Counter.

   tail = tail_at(text, k, more)
   if (tail) return
   if (index(text(k:), BACKSLASH) > 0) then ! the backslashes escaped: a `\)` no longer escapes the parenthesis
      other = backslashes_written(text(k:))
      tail = tail_at(other, 1_I4P, more)
      if (tail) return
   endif
   pos = skip_white(text, k + 1_I4P)
   if (pos > len(text, kind=I4P)) return
   if (text(pos:pos) /= '<') return
   other = text
   other(pos:pos) = 'a'
   tail = tail_at(other, k, more)
   if (tail) return
   other = text
   do i=pos + 1_I4P, len(other, kind=I4P)
      if (other(i:i) == '<') other(i:i) = 'a'
   enddo
   tail = tail_at(other, k, more)
   endfunction inline_tail

   function backslashes_written(text) result(written)
   !< A text with its backslashes as the writer writes them: doubled before ASCII punctuation and at the end.
   character(*), intent(in)  :: text    !< Text.
   character(:), allocatable :: written !< Result.
   integer(I4P)              :: i       !< Position.

   written = ''
   do i=1_I4P, len(text, kind=I4P)
      written = written//text(i:i)
      if (text(i:i) /= BACKSLASH) cycle
      if (i == len(text, kind=I4P)) then
         written = written//BACKSLASH
      elseif (is_ascii_punctuation(text(i+1:i+1))) then
         written = written//BACKSLASH
      endif
   enddo
   endfunction backslashes_written

   function tail_at(text, k, more) result(tail)
   !< Return true if an inline link's tail starts at `text(k)` as the text reads, or might (the text ends first).
   character(*), intent(in) :: text  !< Text.
   integer(I4P), intent(in) :: k     !< Position of the `(`.
   logical,      intent(in) :: more  !< Something is written after the text, in its block.
   logical                  :: tail  !< Result.
   integer(I4P)             :: pos   !< Position.
   integer(I4P)             :: f, l  !< Bounds of the destination.
   integer(I4P)             :: t     !< Bytes of the title.
   integer(I4P)             :: n     !< Length.
   integer(I4P)             :: i     !< Counter.
   logical                  :: ok    !< A destination.

   tail = .true.
   n = len(text, kind=I4P)
   pos = skip_white(text, k + 1_I4P)
   if (pos > n) return
   if (text(pos:pos) == ')') return
   call scan_destination(text, pos, .true., f, l, ok)
   if (.not. ok) then ! a destination running to the text's end (no white space, no control) may end after it
      if (.not. more) then
         tail = index(text(k:), ')') == 0
         return
      endif
      do i=pos, n
         if (iachar(text(i:i)) <= 32 .or. iachar(text(i:i)) == 127) then
            tail = .false.
            return
         endif
      enddo
      return
   endif
   pos = skip_white(text, pos)
   if (pos > n) return
   if (text(pos:pos) == ')') return
   t = scan_title(text, pos)
   if (t == 0_I4P) then ! a title the text does not close may close in what follows
      tail = .false.
      if (index('"''(', text(pos:pos)) > 0) &
         tail = index(text(pos+1:), merge(')', text(pos:pos), text(pos:pos) == '(')) == 0
      return
   endif
   pos = skip_white(text, pos + t)
   if (pos > n) return
   tail = text(pos:pos) == ')'
   endfunction tail_at

   pure function skip_white(text, k) result(pos)
   !< Return the first position from `k` that is not a space or a tab.
   character(*), intent(in) :: text !< Text.
   integer(I4P), intent(in) :: k    !< Position.
   integer(I4P)             :: pos  !< Result.

   pos = k
   do while (pos <= len(text, kind=I4P))
      if (.not. is_space_or_tab(text(pos:pos))) exit
      pos = pos + 1_I4P
   enddo
   endfunction skip_white

   pure function opens_html(text, at_end, gt) result(opens)
   !< Return true if a `<`, `text(1)`, would start raw HTML or an autolink: one of them is there whole, or it could end
   !< in what is written after the text (`at_end` false); a comment, a processing instruction, a declaration or a CDATA
   !< section is always taken as one.
   character(*), intent(in) :: text   !< Text from the `<`.
   logical,      intent(in) :: at_end !< Nothing follows the text in its block.
   logical,      intent(in) :: gt     !< A `>` follows in the text.
   logical                  :: opens  !< Result.

   opens = .false.
   if (len(text) < 2) then
      opens = .not. at_end
      return
   endif
   if (is_ascii_space(text(2:2)) .or. text(2:2) == '<') return
   if (text(2:2) == '!' .or. text(2:2) == '?') then
      opens = .true.
   elseif (.not. gt) then
      opens = .not. at_end
   else
      opens = scan_autolink_uri(text) > 0_I4P .or. scan_autolink_email(text) > 0_I4P .or. scan_html_tag(text) > 0_I4P
      ! a tag the text holds the start of, ended by what follows (a quoted value running on: `<a f=">` then `">`)
      if (.not. (opens .or. at_end)) opens = scan_html_tag(text//'">') > 0_I4P .or. scan_html_tag(text//"'>") > 0_I4P
   endif
   endfunction opens_html

   subroutine run_flanking(c, before, after, can_open, can_close)
   !< Whether a run of `c` (`*` or `_`) between the characters `before` and `after` (empty: the start, the end) can open
   !< and can close emphasis (CommonMark 6.2, as the parser decides).
   character,    intent(in)  :: c         !< Delimiter character.
   character(*), intent(in)  :: before    !< The character before the run.
   character(*), intent(in)  :: after     !< The character after it.
   logical,      intent(out) :: can_open  !< It can open.
   logical,      intent(out) :: can_close !< It can close.
   logical                   :: bs, bp    !< White space, punctuation before.
   logical                   :: as_, ap   !< White space, punctuation after.
   logical                   :: left      !< Left-flanking.
   logical                   :: right     !< Right-flanking.

   call classify(before, merge(1_I4P, 0_I4P, len(before) > 0), bs, bp)
   call classify(after, merge(1_I4P, 0_I4P, len(after) > 0), as_, ap)
   left = .not. as_ .and. (.not. ap .or. bs .or. bp)
   right = .not. bs .and. (.not. bp .or. as_ .or. ap)
   if (c == '_') then
      can_open = left .and. (.not. right .or. bp)
      can_close = right .and. (.not. left .or. ap)
   else
      can_open = left
      can_close = right
   endif
   endsubroutine run_flanking

   subroutine last_closers(text, before, after, closer)
   !< Return the start of the last run of `*` (`closer(1)`) and of `_` (`closer(2)`) of a text that can close emphasis
   !< (0 if none), its characters read as they stand in the text, `before` and `after` around it.
   character(*), intent(in)  :: text      !< Text.
   character(*), intent(in)  :: before    !< The character before the text.
   character(*), intent(in)  :: after     !< The character after it.
   integer(I4P), intent(out) :: closer(2) !< Last closing run of each character.
   character(:), allocatable :: pc, ac    !< Characters around a run.
   integer(I4P)              :: i, k      !< Bounds of a run.
   logical                   :: can_open, can_close !< Flanking.

   closer = 0_I4P
   i = 1_I4P
   do while (i <= len(text, kind=I4P))
      if (text(i:i) /= '*' .and. text(i:i) /= '_') then
         i = i + 1_I4P
         cycle
      endif
      k = i
      do while (k <= len(text, kind=I4P))
         if (text(k:k) /= text(i:i)) exit
         k = k + 1_I4P
      enddo
      if (i == 1_I4P) then
         pc = before
      else
         pc = text(prev_char_start(text, i):i-1)
      endif
      if (k <= len(text, kind=I4P)) then
         ac = utf8_at(text, k)
      else
         ac = after
      endif
      call run_flanking(text(i:i), pc, ac, can_open, can_close)
      if (can_close) closer(merge(1, 2, text(i:i) == '*')) = i
      i = k
   enddo
   endsubroutine last_closers

   pure function prev_char_start(text, i) result(first)
   !< Return the first byte of the UTF-8 character ending at `i - 1`.
   character(*), intent(in) :: text  !< Text.
   integer(I4P), intent(in) :: i     !< Position after the character.
   integer(I4P)             :: first !< Its first byte.

   first = i - 1_I4P
   do while (first > 1_I4P)
      if (iand(iachar(text(first:first)), 192) /= 128) exit
      first = first - 1_I4P
   enddo
   endfunction prev_char_start

   pure function tick_closed(text, r) result(closed)
   !< Return true if a backtick string of `r` backticks would be closed by one written later in a text: a later string
   !< of `r` backticks written as it is, or, for `r` = 1, a later string escaped (`\`\``: single backticks). A later
   !< string is escaped when one of its length is written after it (the rule applied from the text's end).
   character(*), intent(in) :: text   !< The text after the string.
   integer(I4P), intent(in) :: r      !< Its length.
   logical                  :: closed !< Result.
   integer(I4P), allocatable :: lens(:) !< Lengths of the later strings, in order.
   logical,      allocatable :: escs(:) !< Each one escaped.
   integer(I4P)             :: m      !< Strings.
   integer(I4P)             :: i, k   !< Positions.
   integer(I4P)             :: j, q   !< Strings.

   closed = .false.
   m = 0_I4P
   do i=1, len(text, kind=I4P)
      if (text(i:i) == '`') then
         if (i == 1_I4P) then
            m = m + 1_I4P
         elseif (text(i-1:i-1) /= '`') then
            m = m + 1_I4P
         endif
      endif
   enddo
   if (m == 0_I4P) return
   allocate(lens(m), escs(m))
   j = 0_I4P
   i = 1_I4P
   do while (i <= len(text, kind=I4P))
      if (text(i:i) /= '`') then
         i = i + 1_I4P
         cycle
      endif
      k = i
      do while (k <= len(text, kind=I4P))
         if (text(k:k) /= '`') exit
         k = k + 1_I4P
      enddo
      j = j + 1_I4P
      lens(j) = k - i
      i = k
   enddo
   do j=m, 1_I4P, -1_I4P
      escs(j) = .false.
      do q=j+1_I4P, m
         if ((escs(q) .and. lens(j) == 1_I4P) .or. (.not. escs(q) .and. lens(q) == lens(j))) escs(j) = .true.
      enddo
   enddo
   do q=1_I4P, m
      if ((escs(q) .and. r == 1_I4P) .or. (.not. escs(q) .and. lens(q) == r)) closed = .true.
   enddo
   endfunction tick_closed

   pure function has_tick_run(text, r) result(has)
   !< Return true if a text holds a backtick string of exactly `r` backticks (it would close a code span `r` opens).
   character(*), intent(in) :: text !< Text.
   integer(I4P), intent(in) :: r    !< Length.
   logical                  :: has  !< Result.
   integer(I4P)             :: i, k !< Positions.

   has = .false.
   i = 1_I4P
   do while (i <= len(text, kind=I4P))
      if (text(i:i) == '`') then
         k = i
         do while (k <= len(text, kind=I4P))
            if (text(k:k) /= '`') exit
            k = k + 1_I4P
         enddo
         if (k - i == r) then
            has = .true.
            return
         endif
         i = k
      else
         i = i + 1_I4P
      endif
   enddo
   endfunction has_tick_run

   function pair_links(w, text, i, j, next, nc, block_start) result(links)
   !< Return true if a pair of brackets of a text, `text(i)` and `text(j)`, could make a link when read back: followed by
   !< `(` or `[`, a link, or holding a defined label; or a definition, followed by `:` at the start of a paragraph.
   type(md_writer), intent(in) :: w     !< Writer.
   character(*),    intent(in) :: text  !< Text.
   integer(I4P),    intent(in) :: i     !< The `[`.
   integer(I4P),    intent(in) :: j     !< The matching `]`.
   integer(I4P),    intent(in) :: next  !< What follows the text: class.
   character(*),    intent(in) :: nc    !< What follows the text: first character.
   logical,         intent(in) :: block_start !< The `[` starts a paragraph.
   logical                     :: links !< Result.

   links = after_bracket(w, text, j, next, nc, text(i+1:j-1), block_start .and. i == 1)
   endfunction pair_links

   pure subroutine match_brackets(text, mate)
   !< Pair the brackets of a text, innermost first: `mate(k)` is the position of the bracket matching the one at `k`, 0
   !< for a bracket with no mate (and for any other character).
   character(*),              intent(in)  :: text    !< Text.
   integer(I4P), allocatable, intent(out) :: mate(:) !< Mates.
   integer(I4P), allocatable              :: stack(:) !< Open brackets.
   integer(I4P)                           :: top     !< Stack top.
   integer(I4P)                           :: k       !< Position.

   allocate(mate(len(text)), stack(len(text)))
   mate = 0_I4P
   top = 0_I4P
   do k=1, len(text, kind=I4P)
      if (text(k:k) == '[') then
         top = top + 1_I4P
         stack(top) = k
      elseif (text(k:k) == ']' .and. top > 0_I4P) then
         mate(k) = stack(top)
         mate(stack(top)) = k
         top = top - 1_I4P
      endif
   enddo
   endsubroutine match_brackets

   pure function scan_entity_like(text, i) result(is)
   !< Return true if a character reference starts at `text(i)`, an `&`.
   character(*), intent(in)  :: text  !< Text.
   integer(I4P), intent(in)  :: i     !< Position of the `&`.
   logical                   :: is    !< Result.
   character(:), allocatable :: bytes !< Its character.
   integer(I4P)              :: l     !< Its length.

   call scan_reference(text, i, bytes, l)
   is = l > 0_I4P
   endfunction scan_entity_like

   function bullets_break(w, line) result(e)
   !< Return 1 if the first line of a block, behind the bullets of the items it starts (pending on that line), would
   !< make a thematic break with them (`* **`, `- - -`): its first character, a break's, is then escaped; 0 otherwise.
   type(md_writer), intent(in) :: w    !< Writer.
   character(*),    intent(in) :: line !< The line, as written, without its prefix.
   integer(I4P)                :: e    !< Position to escape, 0 for none.
   character(:), allocatable   :: probe !< The line behind some bullets.
   integer(I4P)                :: f    !< Frame.
   integer(I4P)                :: k, length !< A thematic break.

   e = 0_I4P
   if (len(line) == 0) return
   if (index('*-_', line(1:1)) == 0) return
   probe = line
   do f=w%depth, 1_I4P, -1_I4P
      if (.not. w%pending(f)) exit
      if (w%bullet(f) == 0_I4P) then ! a frame of spaces (a block's indentation) is no bullet, but no end either
         if (verify(w%first_buf(w%first_lo(f):w%first_hi(f)), ' ') == 0) cycle
         exit
      endif
      probe = char1(w%bullet(f))//' '//probe
      call scan_thematic_break(probe, 1_I4P, k, length)
      if (k > 0_I4P) then
         e = 1_I4P
         return
      endif
   enddo
   endfunction bullets_break

   function block_start_escape(line, continuation) result(e)
   !< Return the position to escape in a line of a paragraph so that it opens no block (0 if it opens none): its first
   !< character, or the delimiter of an ordered list marker. A `continuation` line can only open what interrupts a
   !< paragraph, and a setext underline.
   character(*), intent(in) :: line         !< The line, as written, without its prefix.
   logical,      intent(in) :: continuation !< Not the paragraph's first line.
   integer(I4P)             :: e            !< Position to escape, 0 for none.
   integer(I4P)             :: k            !< Counter.
   integer(I4P)             :: marker, start, length !< A list marker.
   logical                  :: ordered      !< An ordered marker.

   e = 1_I4P
   if (len(line) == 0) then
      e = 0_I4P
      return
   endif
   if (line(1:1) == '>') return
   call scan_atx_start(line, 1_I4P, k, length)
   if (k > 0_I4P) return
   if (scan_open_fence(line, 1_I4P) > 0_I4P) return
   call scan_thematic_break(line, 1_I4P, k, length)
   if (k > 0_I4P) return
   if (continuation) then
      if (scan_setext_underline(line, 1_I4P) > 0_I4P) return
   endif
   if (scan_html_block_start(line, 1_I4P, .not. continuation) > 0_I4P) return
   call scan_list_marker(line, 1_I4P, continuation, ordered, marker, start, length)
   if (length > 0_I4P) then
      if (ordered) e = length ! the delimiter, after the digits
      return
   endif
   e = 0_I4P
   endfunction block_start_escape

   subroutine put_guarded(s, text)
   !< Write text whose bytes are not escaped (raw HTML, the end of an inline link): a line it starts or continues that
   !< would open a block is indented by 4 spaces, which a paragraph's continuation line loses.
   type(inline_state), intent(inout) :: s    !< State.
   character(*),       intent(in)    :: text !< Text.
   integer(I4P)                      :: i, j !< Bounds of a line.

   i = 1_I4P
   do while (i <= len(text, kind=I4P))
      j = index(text(i:), LF) + i - 1_I4P
      if (j < i) j = len(text, kind=I4P) + 1_I4P
      if (s%mode == INLINE_LINES .and. s%line_start .and. .not. s%first_line .and. j > i) then
         if (block_start_escape(text(i:j-1), .true.) > 0_I4P .or. text(i:i) == ' ') call put_raw(s, '    ')
      endif
      if (j <= len(text, kind=I4P)) then
         call put_raw(s, text(i:j))
      else
         call put_raw(s, text(i:j-1))
      endif
      i = j + 1_I4P
   enddo
   endsubroutine put_guarded

   subroutine put_raw(s, text)
   !< Write text as it is, keeping the line state (a line's recorded indentation before its first byte).
   type(inline_state), intent(inout) :: s    !< State.
   character(*),       intent(in)    :: text !< Text.

   if (len(text) == 0) return
   if (s%line_start .and. s%indent > 0_I4P) call s%buf%put(repeat(' ', s%indent))
   s%indent = 0_I4P
   call s%buf%put(text)
   if (index(text, LF) > 0) s%first_line = .false.
   s%line_start = text(len(text):len(text)) == LF
   endsubroutine put_raw

   ! strings
   function destination_text(dest, definition) result(text)
   !< Return a destination as written in a link or a definition: between `<` and `>` when empty (a definition), when
   !< it holds white space or control characters, unbalanced parentheses or starts with `<`; else as it is. Backslashes
   !< before punctuation and `&` before a character reference are escaped.
   character(*), intent(in)  :: dest       !< Destination.
   logical,      intent(in)  :: definition !< Of a link reference definition.
   character(:), allocatable :: text       !< As written.
   integer(I4P)              :: i          !< Position.
   integer(I4P)              :: depth      !< Parentheses open.
   logical                   :: pointy     !< Between `<` and `>`.
   logical                   :: balanced   !< Its parentheses are balanced.

   pointy = len(dest) == 0
   balanced = .true.
   depth = 0_I4P
   do i=1, len(dest, kind=I4P)
      if (iachar(dest(i:i)) <= 32 .or. iachar(dest(i:i)) == 127) pointy = .true.
      if (dest(i:i) == '(') depth = depth + 1_I4P
      if (dest(i:i) == ')') depth = depth - 1_I4P
      if (depth < 0_I4P) balanced = .false.
   enddo
   if (depth /= 0_I4P) balanced = .false.
   if (len(dest) > 0) then
      if (dest(1:1) == '<') pointy = .true.
   endif
   if (pointy .or. .not. balanced) then
      text = '<'//line_ends_as_references(escape_string(dest, '<>'))//'>'
   else
      text = escape_string(dest, '')
   endif
   if (definition .and. len(dest) == 0) text = '<>'
   endfunction destination_text

   pure function line_ends_as_references(text) result(refs)
   !< A text with its line feeds and carriage returns as numeric character references.
   character(*), intent(in)  :: text !< Text.
   character(:), allocatable :: refs !< Result.
   integer(I4P)              :: k    !< Position.

   refs = ''
   do k=1_I4P, len(text, kind=I4P)
      if (text(k:k) == LF) then
         refs = refs//'&#10;'
      elseif (text(k:k) == CR) then
         refs = refs//'&#13;'
      else
         refs = refs//text(k:k)
      endif
   enddo
   endfunction line_ends_as_references

   function info_text(info) result(text)
   !< Return an info string as written after a fence: escaped, its line endings and the white space at its ends (the
   !< parser strips it) as references.
   character(*), intent(in)  :: info !< Info string.
   character(:), allocatable :: text !< As written.
   character(:), allocatable :: raw  !< Escaped.
   integer(I4P)              :: k    !< Position.
   integer(I4P)              :: i, j !< Its first and last characters that are not white space.

   raw = escape_string(info, '')
   i = verify(raw, ' '//TAB)
   j = verify(raw, ' '//TAB, back=.true.)
   text = ''
   do k=1_I4P, len(raw, kind=I4P)
      if (raw(k:k) == LF .or. raw(k:k) == CR .or. ((raw(k:k) == ' ' .or. raw(k:k) == TAB) .and. (k < i .or. k > j))) then
         text = text//'&#'//istr(iachar(raw(k:k), kind=I4P))//';'
      else
         text = text//raw(k:k)
      endif
   enddo
   endfunction info_text

   function title_text(title) result(text)
   !< Return a title as written after a destination (with the space before it), empty for none: between `"`, the `"`
   !< inside escaped, the white space starting a line after the first as references.
   character(*), intent(in)  :: title !< Title.
   character(:), allocatable :: text  !< As written.
   character                 :: q     !< Quote.
   character(:), allocatable :: raw   !< Escaped.
   integer(I4P)              :: k     !< Position.
   logical                   :: at_start !< At the start of a line after the first.

   text = ''
   if (len(title) == 0) return
   q = '"'
   raw = escape_string(title, q)
   at_start = .false.
   do k=1, len(raw, kind=I4P) ! a line's leading white space would be stripped (a paragraph's line): as references
      if (at_start .and. (raw(k:k) == ' ' .or. raw(k:k) == TAB)) then
         text = text//'&#'//istr(iachar(raw(k:k), kind=I4P))//';'
      else
         text = text//raw(k:k)
         at_start = raw(k:k) == LF
      endif
   enddo
   text = ' '//q//text//q
   endfunction title_text

   pure function escape_string(text, specials) result(escaped)
   !< Return a string (an info string, a destination, a title) escaped for a place where backslash escapes and
   !< character references are read: a backslash before punctuation or at the end, `&` before a reference, and the
   !< characters of `specials`.
   character(*), intent(in)  :: text     !< String.
   character(*), intent(in)  :: specials !< Characters escaped wherever they are.
   character(:), allocatable :: escaped  !< Escaped string.
   integer(I4P)              :: i        !< Position.
   character(:), allocatable :: buf      !< Escaped bytes.
   integer(I4P)              :: n        !< Bytes written.
   logical                   :: esc      !< Escape.

   allocate(character(2 * len(text)) :: buf)
   n = 0_I4P
   do i=1, len(text, kind=I4P)
      esc = index(specials, text(i:i)) > 0
      if (text(i:i) == BACKSLASH) then
         esc = i == len(text, kind=I4P)
         if (.not. esc) esc = is_ascii_punctuation(text(i+1:i+1))
      elseif (text(i:i) == '&') then
         esc = scan_entity_like(text, i)
      endif
      if (esc) then
         n = n + 1_I4P
         buf(n:n) = BACKSLASH
      endif
      n = n + 1_I4P
      buf(n:n) = text(i:i)
   enddo
   escaped = buf(1:n)
   endfunction escape_string

   ! lines and prefixes
   subroutine emit_text(w, text, guard)
   !< Write a text as lines behind the prefixes (a final line feed ends the last line, it adds no empty one); with
   !< `guard`, a continuation line that would open a block is indented by 4 spaces (a definition's title, its label).
   type(md_writer), intent(inout) :: w     !< Writer.
   character(*),    intent(in)    :: text  !< Text.
   logical,         intent(in)    :: guard !< Guard continuation lines.
   integer(I4P)                   :: i, j  !< Bounds of a line.

   i = 1_I4P
   if (len(text) == 0) then
      call emit_line(w, '')
      return
   endif
   do while (i <= len(text, kind=I4P))
      j = index(text(i:), LF) + i - 1_I4P
      if (j < i) j = len(text, kind=I4P) + 1_I4P
      if (guard .and. i > 1_I4P .and. j > i) then
         if (block_start_escape(text(i:j-1), .true.) > 0_I4P) then
            call emit_line(w, '    '//text(i:j-1))
            i = j + 1_I4P
            cycle
         endif
      endif
      call emit_line(w, text(i:j-1))
      i = j + 1_I4P
   enddo
   endsubroutine emit_text

   subroutine emit_line(w, text)
   !< Write one line behind the prefixes of the open containers (each frame's first-line prefix once, its other-line
   !< prefix after); a blank line drops the prefixes' trailing white space.
   type(md_writer), intent(inout) :: w      !< Writer.
   character(*),    intent(in)    :: text   !< The line, its ending excluded.
   character(:), allocatable      :: prefix !< Prefix.
   integer(I4P)                   :: f      !< Frame.
   integer(I4P)                   :: k      !< Length.

   if (w%depth == 0_I4P) then
      prefix = ''
   elseif (w%pendings == 0_I4P) then
      prefix = w%rest_buf(1:w%rest_hi(w%depth))
   else
      k = 0_I4P
      do f=1, w%depth
         if (w%pending(f)) then
            k = k + w%first_hi(f) - w%first_lo(f) + 1_I4P
         else
            k = k + w%rest_hi(f) - w%rest_lo(f) + 1_I4P
         endif
      enddo
      allocate(character(k) :: prefix)
      k = 0_I4P
      do f=1, w%depth
         if (w%pending(f)) then
            prefix(k+1:k+w%first_hi(f)-w%first_lo(f)+1) = w%first_buf(w%first_lo(f):w%first_hi(f))
            k = k + w%first_hi(f) - w%first_lo(f) + 1_I4P
            w%pending(f) = .false.
         else
            prefix(k+1:k+w%rest_hi(f)-w%rest_lo(f)+1) = w%rest_buf(w%rest_lo(f):w%rest_hi(f))
            k = k + w%rest_hi(f) - w%rest_lo(f) + 1_I4P
         endif
      enddo
      w%pendings = 0_I4P
   endif
   if (len(text) == 0) then
      k = len(prefix, kind=I4P)
      do while (k > 0_I4P)
         if (.not. is_space_or_tab(prefix(k:k))) exit
         k = k - 1_I4P
      enddo
      call w%out%put(prefix(1:k)//LF)
   else
      call w%out%put(prefix//text//LF)
   endif
   endsubroutine emit_line

   subroutine push(w, first, rest, bullet)
   !< Open a container: its first line starts with `first`, the others with `rest`.
   type(md_writer), intent(inout) :: w      !< Writer.
   character(*),    intent(in)    :: first  !< First-line prefix.
   character(*),    intent(in)    :: rest   !< Other-line prefix.
   integer(I4P),    intent(in)    :: bullet !< The bullet of an item (`iachar`), 0 otherwise.
   integer(I4P)                   :: f, r   !< Bytes in use.

   if (w%depth == size(w%pending, kind=I4P)) then
      call grow(w%first_lo)
      call grow(w%first_hi)
      call grow(w%rest_lo)
      call grow(w%rest_hi)
      call grow(w%bullet)
      call grow_logical(w%pending)
   endif
   f = 0_I4P
   r = 0_I4P
   if (w%depth > 0_I4P) then
      f = w%first_hi(w%depth)
      r = w%rest_hi(w%depth)
   endif
   call ensure(w%first_buf, f + len(first, kind=I4P))
   call ensure(w%rest_buf, r + len(rest, kind=I4P))
   w%depth = w%depth + 1_I4P
   w%first_buf(f+1:f+len(first)) = first
   w%rest_buf(r+1:r+len(rest)) = rest
   w%first_lo(w%depth) = f + 1_I4P
   w%first_hi(w%depth) = f + len(first, kind=I4P)
   w%rest_lo(w%depth) = r + 1_I4P
   w%rest_hi(w%depth) = r + len(rest, kind=I4P)
   w%bullet(w%depth) = bullet
   w%pending(w%depth) = .true.
   w%pendings = w%pendings + 1_I4P
   endsubroutine push

   subroutine pop(w)
   !< Close the innermost container.
   type(md_writer), intent(inout) :: w !< Writer.

   if (w%depth == 0_I4P) return
   if (w%pending(w%depth)) w%pendings = w%pendings - 1_I4P
   w%depth = w%depth - 1_I4P
   endsubroutine pop

   ! extensions
   subroutine offer(doc, slots, node, entering, piece, handled, ierr, msg)
   !< Offer an event of a node to the enabled extensions' `write_markdown` hooks, in registration order.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering (else leaving).
   character(:), allocatable, intent(out)   :: piece    !< The Markdown of the extension that handled it.
   logical,                   intent(out)   :: handled  !< An extension handled it.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: s        !< Counter.

   handled = .false.
   piece = ''
   do s=1, size(slots, kind=I4P)
      if (.not. slots(s)%enabled) cycle
      if (.not. allocated(msg)) msg = ''
      call slots(s)%ext%write_markdown(doc, node, entering, piece, handled, ierr, msg)
      if (.not. allocated(piece)) piece = ''
      if (ierr /= BARUCH_OK) then
         msg = 'markdown_options%write_markdown: extension "'//slots(s)%ext%name()//'": '//msg
         handled = .false.
         return
      endif
      if (handled) return
   enddo
   piece = ''
   endsubroutine offer

   ! helpers
   function ends_unclosed_html(doc, node) result(unclosed)
   !< Return true if a block ends (itself, or down the last blocks of its lists and items, where blank lines do not
   !< end a block) with an HTML block of type 1 to 5 whose end condition its literal does not meet: it runs to the end
   !< of its container, every line after it, blank or not, is its own.
   type(markdown_document), intent(in) :: doc      !< Document.
   integer(I4P),            intent(in) :: node     !< Block.
   logical                             :: unclosed !< Result.
   character(:), allocatable           :: literal  !< Its literal.
   character(:), allocatable           :: lower    !< In lower case.
   integer(I4P)                        :: n        !< Current block.
   integer(I4P)                        :: k        !< Position.

   unclosed = .false.
   n = node
   do while (doc%node_kind(n) == NODE_LIST .or. doc%node_kind(n) == NODE_ITEM)
      n = doc%last_child(n)
      if (n == 0_I4P) return
   enddo
   if (doc%node_kind(n) /= NODE_HTML_BLOCK) return
   literal = doc%literal(n)
   select case(html_type(literal))
   case(1_I4P)
      lower = literal
      do k=1, len(lower, kind=I4P)
         if (lower(k:k) >= 'A' .and. lower(k:k) <= 'Z') lower(k:k) = char1(iachar(lower(k:k), kind=I4P) + 32_I4P)
      enddo
      unclosed = index(lower, '</script>') == 0 .and. index(lower, '</pre>') == 0 .and. &
                 index(lower, '</style>') == 0 .and. index(lower, '</textarea>') == 0
   case(2_I4P)
      unclosed = index(literal, '-->') == 0
   case(3_I4P)
      unclosed = index(literal, '?>') == 0
   case(4_I4P)
      unclosed = index(literal, '>') == 0
   case(5_I4P)
      unclosed = index(literal, ']]>') == 0
   endselect
   endfunction ends_unclosed_html

   function html_type(literal) result(htype)
   !< Return the type (1 to 7) of the HTML block a literal starts, 0 if none.
   character(*), intent(in) :: literal !< Literal.
   integer(I4P)             :: htype   !< Type.
   integer(I4P)             :: i, j    !< Bounds of its first line.

   j = index(literal, LF)
   if (j == 0) j = len(literal) + 1
   i = 1_I4P
   do while (i < j)
      if (literal(i:i) /= ' ') exit
      i = i + 1_I4P
   enddo
   htype = scan_html_block_start(literal(1:j-1), i, .true.)
   endfunction html_type

   function next_in(doc, n, top) result(next)
   !< Return the node after `n` in document order inside the subtree of `top` (0 after its last).
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: n    !< Node.
   integer(I4P),            intent(in) :: top  !< Subtree root.
   integer(I4P)                        :: next !< Next node.

   next = doc%first_child(n)
   if (next /= 0_I4P) return
   next = n
   do while (next /= top .and. next /= 0_I4P)
      if (doc%next_sibling(next) /= 0_I4P) then
         next = doc%next_sibling(next)
         return
      endif
      next = doc%parent(next)
   enddo
   next = 0_I4P
   endfunction next_in

   function next_in_range(doc, n, first, last) result(next)
   !< Return the node after `n` in document order among the siblings `first` to `last` and their subtrees.
   type(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),            intent(in) :: n     !< Node.
   integer(I4P),            intent(in) :: first !< First sibling.
   integer(I4P),            intent(in) :: last  !< Last sibling.
   integer(I4P)                        :: next  !< Next node, 0 after the last.
   integer(I4P)                        :: top   !< Parent of the siblings.

   top = doc%parent(first)
   next = doc%first_child(n)
   if (next /= 0_I4P) return
   next = n
   do while (next /= top .and. next /= 0_I4P)
      if (next == last) then
         next = 0_I4P
         return
      endif
      if (doc%next_sibling(next) /= 0_I4P) then
         next = doc%next_sibling(next)
         return
      endif
      next = doc%parent(next)
   enddo
   next = 0_I4P
   endfunction next_in_range

   pure function count_char(text, c) result(k)
   !< Return how many times a character occurs in a text.
   character(*), intent(in) :: text !< Text.
   character,    intent(in) :: c    !< Character.
   integer(I4P)             :: k    !< Count.
   integer(I4P)             :: i    !< Position.

   k = 0_I4P
   do i=1, len(text, kind=I4P)
      if (text(i:i) == c) k = k + 1_I4P
   enddo
   endfunction count_char

   pure function utf8_length(lead) result(l)
   !< Return the bytes of the UTF-8 character a lead byte starts (1 for a continuation byte).
   character, intent(in) :: lead !< Lead byte.
   integer(I4P)          :: l    !< Bytes.
   integer               :: b    !< Its value.

   b = iachar(lead)
   if (b >= 240) then
      l = 4_I4P
   elseif (b >= 224) then
      l = 3_I4P
   elseif (b >= 192) then
      l = 2_I4P
   else
      l = 1_I4P
   endif
   endfunction utf8_length

   pure function utf8_at(text, i) result(c)
   !< Return the UTF-8 character starting at `text(i)`.
   character(*), intent(in)  :: text !< Text.
   integer(I4P), intent(in)  :: i    !< Position.
   character(:), allocatable :: c    !< Character.

   c = text(i:min(len(text, kind=I4P), i + utf8_length(text(i:i)) - 1_I4P))
   endfunction utf8_at

   pure function utf8_code(c) result(code)
   !< Return the code point of one UTF-8 character.
   character(*), intent(in) :: c    !< Character.
   integer(I4P)             :: code !< Code point.
   integer(I4P)             :: k    !< Byte.

   select case(len(c))
   case(2)
      code = iand(iachar(c(1:1), kind=I4P), 31_I4P)
   case(3)
      code = iand(iachar(c(1:1), kind=I4P), 15_I4P)
   case(4)
      code = iand(iachar(c(1:1), kind=I4P), 7_I4P)
   case default
      code = iachar(c(1:1), kind=I4P)
      return
   endselect
   do k=2, len(c, kind=I4P)
      code = code * 64_I4P + iand(iachar(c(k:k), kind=I4P), 63_I4P)
   enddo
   endfunction utf8_code

   elemental function char1(code) result(c)
   !< Return the ASCII character of a code as a one-byte result: nvfortran 26.9 gives a deferred-length variable
   !< assigned `achar(i)` (i not a constant) the length `i` (szaghi/BARUCH#2, reproducer
   !< notes/nvfortran-achar-deferred-length.f90); a fixed-length result has its length.
   integer(I4P), intent(in) :: code !< Code.
   character                :: c    !< Character.

   c = achar(code)
   endfunction char1

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr

   pure subroutine grow(a)
   !< Double an integer array.
   integer(I4P), allocatable, intent(inout) :: a(:) !< Array.
   integer(I4P), allocatable                :: b(:) !< Grown copy.

   allocate(b(2 * size(a)))
   b = 0_I4P
   b(1:size(a)) = a
   call move_alloc(b, a)
   endsubroutine grow

   pure subroutine grow_matrix(a)
   !< Double the columns of an integer matrix.
   integer(I4P), allocatable, intent(inout) :: a(:,:) !< Matrix.
   integer(I4P), allocatable                :: b(:,:) !< Grown copy.

   allocate(b(size(a, 1), 2 * size(a, 2)))
   b = 0_I4P
   b(:, 1:size(a, 2)) = a
   call move_alloc(b, a)
   endsubroutine grow_matrix

   pure subroutine grow_logical(a)
   !< Double a logical array.
   logical, allocatable, intent(inout) :: a(:) !< Array.
   logical, allocatable                :: b(:) !< Grown copy.

   allocate(b(2 * size(a)))
   b = .false.
   b(1:size(a)) = a
   call move_alloc(b, a)
   endsubroutine grow_logical

   pure subroutine ensure(buf, n)
   !< Make a buffer hold at least `n` bytes, keeping its content.
   character(:), allocatable, intent(inout) :: buf !< Buffer.
   integer(I4P),              intent(in)    :: n   !< Bytes needed.
   character(:), allocatable                :: big !< Grown copy.

   if (len(buf, kind=I4P) >= n) return
   allocate(character(max(2_I4P * len(buf, kind=I4P), n)) :: big)
   big(1:len(buf)) = buf
   buf = big ! not move_alloc: nvfortran 26.9 loses the length (notes/nvfortran-move-alloc-deferred-length-char.f90)
   endsubroutine ensure

   ! md_buffer
   pure subroutine put(self, bytes)
   !< Append bytes, growing the buffer by doubling.
   class(md_buffer), intent(inout) :: self  !< Buffer.
   character(*),     intent(in)    :: bytes !< Bytes.
   character(:), allocatable       :: big   !< Grown buffer.
   integer(I4P)                    :: n     !< Bytes.

   n = len(bytes, kind=I4P)
   if (n == 0_I4P) return
   if (.not. allocated(self%chars)) allocate(character(max(256_I4P, 2_I4P * n)) :: self%chars)
   if (self%used + n > len(self%chars, kind=I4P)) then
      allocate(character(max(2_I4P * len(self%chars, kind=I4P), self%used + n)) :: big)
      big(1:self%used) = self%chars(1:self%used)
      self%chars = big ! not move_alloc: nvfortran 26.9 loses the length (notes/nvfortran-move-alloc-deferred-length-char.f90)
   endif
   self%chars(self%used+1:self%used+n) = bytes
   self%used = self%used + n
   endsubroutine put

   pure function last_char(self) result(c)
   !< Return the last UTF-8 character written, empty if nothing is written or the last byte is a line feed.
   class(md_buffer), intent(in) :: self !< Buffer.
   character(:), allocatable    :: c    !< Character.
   integer(I4P)                 :: k    !< Its first byte.

   c = ''
   if (self%used == 0_I4P) return
   if (self%chars(self%used:self%used) == LF) return
   k = self%used
   do while (k > 1_I4P)
      if (iand(iachar(self%chars(k:k)), 192) /= 128) exit
      k = k - 1_I4P
   enddo
   c = self%chars(k:self%used)
   endfunction last_char
endmodule baruch_writer
