!< BARUCH, the block parser: a document's lines into its tree of blocks (CommonMark 0.31.2), the extension hooks called.
module baruch_blocks
!< BARUCH, the block parser: a document's lines into its tree of blocks (CommonMark 0.31.2), the extension hooks called.
!<
!< The block phase of CommonMark, as the spec's appendix "A parsing strategy" describes it (and cmark's `blocks.c`
!< implements it, whose structure this follows): one pass over the lines of the source, no recursion, no per-container
!< copy of the text. The state is a `block_parser` (`baruch_blocks_state`): the source copied once, the position in the
!< current line (offset and column, a tab possibly consumed in part), the stack of open blocks, the tip's lines as byte
!< ranges of the source. What a line starts with is asked of the scanners (`baruch_blocks_scan`); link reference
!< definitions are read by `baruch_blocks_definitions`.
!<
!< Each line (`incorporate_line`):
!<
!< 1. **continuation**: every open block below the root, outermost first, says whether the line continues it and
!<    consumes its marker (`block_continue` for an extension's block; `continue_core` for a core one: `>` for a block
!<    quote, the item's indentation, an indented code block's four columns, a fence's indentation, a paragraph not
!<    blank...); the first that does not ends the walk. A closing fence (or an extension's `BLOCK_FINISHED`) closes its
!<    block and ends the line;
!< 2. **block starts**: unless the deepest matched block takes lines, the position is offered to the extensions'
!<    `block_start` (in registration order: an extension can take syntax away from the core), then to the core's starts
!<    in the spec's order: block quote, ATX heading, fence, HTML block (type 7 never interrupts a paragraph), setext
!<    underline (under a paragraph), thematic break, list item, indented code (never interrupting a paragraph). A start
!<    closes the unmatched blocks, then the open blocks that cannot hold the new one; a container tries again on the
!<    rest of the line, a leaf ends the loop;
!< 3. **the rest of the line**: a lazy continuation line (no block opened, not every block matched, the tip a
!<    paragraph) goes to the paragraph; otherwise the unmatched blocks close and the rest goes to the block it belongs
!<    to: a code or HTML block takes it as it is (an HTML block of type 1 to 5 closes on its end condition), a
!<    paragraph or an ATX heading from its first non-blank byte, and anything else opens a paragraph if it is not blank.
!<    The blank-line bookkeeping of tight and loose lists (CommonMark 5.3, cmark's rules) is done here.
!<
!< Closing a block (`close_blocks`) sets its span (first byte of its marker to the last byte of its last non-blank line)
!< and finishes it: a paragraph gives its leading link reference definitions (nodes in place, before it; a paragraph
!< left empty goes) and keeps the rest as one text node (its lines without their leading white space, joined by LF,
!< the final trailing white space removed: the inline phase reads it); an ATX heading drops its closing sequence (the
!< count recorded); a code block gets its literal (lines joined by LF, each ended by one; trailing blank lines of an
!< indented block dropped) and a fenced one its info string; an HTML block its literal; a list its tightness. An
!< extension's block gets `block_close`.
!<
!< The source style is recorded in the slots of `baruch_kinds_nodes`: bullet character or ordered delimiter, ATX or
!< setext and the closing `#`s, fence character and length, thematic break character. A setext underline turns the
!< paragraph into a heading (a new node; the paragraph becomes a tombstone), and a paragraph made of definitions only
!< goes: the tree is compacted once at the end of a parse that left tombstones, so a parsed document has none.
!<
!< **The hook contract**, checked here (`BARUCH_ERR_CONTRACT`, the parse stops, never loops): a claimed start returns a
!< new detached block of an extension kind; it consumes `0 <= consumed <= ` what is left of the line, and at least one
!< byte unless the block takes lines; `0 <= paragraph_taken <=` the paragraph's lines; a continuation consumes within
!< the line and answers one of the three `BLOCK_*` values. The block-start loop is therefore bounded by the bytes of the
!< line. The line ranges an extension's block receives cannot hold part of a tab: a tab partly consumed by a container
!< marker is given whole.
use penf,                      only : I4P
use baruch_document,           only : markdown_document
use baruch_extension,          only : extension_slot, BLOCK_UNMATCHED, BLOCK_MATCHED, BLOCK_FINISHED, &
                                      CONTENT_NONE, CONTENT_BLOCKS, CONTENT_LINES
use baruch_kinds_nodes,        only : NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_LIST, NODE_ITEM, NODE_CODE_BLOCK,         &
                                      NODE_HTML_BLOCK, NODE_PARAGRAPH, NODE_HEADING, NODE_THEMATIC_BREAK,             &
                                      NODE_LINK_REFERENCE_DEFINITION, NODE_TEXT, NODE_EXTENSION_FIRST,                &
                                      SLOT_HEADING_FORM, SLOT_HEADING_CLOSING, SLOT_LIST_TYPE, SLOT_LIST_START,       &
                                      SLOT_LIST_MARKER, SLOT_CODE_FORM, SLOT_CODE_FENCE_CHAR, SLOT_CODE_FENCE_LENGTH, &
                                      SLOT_BREAK_CHAR, HEADING_ATX, HEADING_SETEXT, LIST_BULLET, LIST_ORDERED,        &
                                      CODE_INDENTED, CODE_FENCED
use baruch_blocks_scan,        only : is_space_or_tab, backslash_unescape, scan_thematic_break, scan_atx_start,    &
                                      scan_open_fence, scan_close_fence, scan_setext_underline, scan_list_marker, &
                                      scan_html_block_start, html_block_end
use baruch_blocks_state,       only : block_parser, CONTENT_PARAGRAPH, FLAG_LAST_LINE_BLANK, FLAG_LAST_LINE_CHECKED, &
                                      FLAG_LIST_LAST_LINE_BLANK, TAB_STOP, CODE_INDENT, LF
use baruch_blocks_definitions, only : parse_definition
use baruch_status
implicit none
private
public :: parse_blocks

integer(I4P), parameter :: STACK_DEFAULT = 32_I4P !< Open blocks, tip lines and node flags allocated up front.
integer(I4P), parameter :: LINE_DONE = -1_I4P     !< A continuation ended the line (cmark's blank-line shortcut).

contains
   subroutine parse_blocks(doc, slots, stat, errmsg)
   !< Parse the lines of a document's source into blocks under its root, with the enabled extensions of `slots`. The
   !< document holds its source and a tree reset to the root (as `markdown_options%parse` leaves it); on a contract
   !< violation the tree keeps what was parsed before it.
   type(markdown_document),   intent(inout)         :: doc      !< Document.
   type(extension_slot),      intent(in)            :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(out), optional :: stat     !< Status.
   character(:), allocatable, intent(out), optional :: errmsg   !< Message.
   type(block_parser)                               :: p        !< Parser state.
   integer(I4P)                                     :: ln       !< Line number.
   integer(I4P)                                     :: ierr     !< Status.
   character(:), allocatable                        :: msg      !< Message.

   call status_ok(stat, errmsg)
   ierr = BARUCH_OK
   msg = ''
   call start(p, doc, slots)
   do ln=1, doc%lines_number()
      call incorporate_line(p, doc, slots, ln, ierr, msg)
      if (ierr /= BARUCH_OK) exit
   enddo
   if (ierr == BARUCH_OK) call close_blocks(p, doc, slots, 2_I4P, ierr, msg)
   if (ierr == BARUCH_OK .and. p%tombstones) call doc%compact(stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_span(doc%root(), 1_I4P, len(p%src, kind=I4P))
   if (ierr /= BARUCH_OK) call status_error(ierr, msg, stat, errmsg)
   endsubroutine parse_blocks

   ! private
   subroutine start(p, doc, slots)
   !< Initialise the parser: the source copied, the root open, the enabled extensions listed.
   type(block_parser),      intent(inout) :: p        !< Parser.
   type(markdown_document), intent(in)    :: doc      !< Document.
   type(extension_slot),    intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P)                           :: s        !< Counter.

   p%src = doc%source()
   allocate(p%open_node(STACK_DEFAULT), p%open_owner(STACK_DEFAULT), p%open_content(STACK_DEFAULT), &
            p%open_first(STACK_DEFAULT), p%open_last(STACK_DEFAULT), p%open_a(STACK_DEFAULT), p%open_b(STACK_DEFAULT))
   allocate(p%tip_first(STACK_DEFAULT), p%tip_last(STACK_DEFAULT), p%tip_pad(STACK_DEFAULT))
   allocate(p%flags(STACK_DEFAULT))
   p%flags = 0_I4P
   p%depth = 1_I4P
   p%open_node(1) = doc%root()
   p%open_owner(1) = 0_I4P
   p%open_content(1) = CONTENT_BLOCKS
   p%open_first(1) = 1_I4P
   p%open_last(1) = 0_I4P
   p%open_a(1) = 0_I4P
   p%open_b(1) = 0_I4P
   p%tip_lines = 0_I4P
   allocate(p%active(0))
   do s=1, size(slots, kind=I4P)
      if (slots(s)%enabled .and. allocated(slots(s)%ext)) p%active = [p%active, s]
   enddo
   endsubroutine start

   subroutine incorporate_line(p, doc, slots, ln, ierr, msg)
   !< Incorporate a line: continue the open blocks, try the block starts, give the rest of the line to its block.
   type(block_parser),        intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   type(extension_slot),      intent(in)    :: slots(:)    !< Extensions of the options.
   integer(I4P),              intent(in)    :: ln          !< Line number.
   integer(I4P),              intent(inout) :: ierr        !< Status.
   character(:), allocatable, intent(inout) :: msg         !< Message.
   integer(I4P)                             :: matched     !< Depth of the deepest block the line continues.
   integer(I4P)                             :: cont        !< Depth of the block new blocks open in.
   integer(I4P)                             :: d           !< Depth.
   integer(I4P)                             :: o           !< Owner slot.
   integer(I4P)                             :: match       !< What a continuation says.
   integer(I4P)                             :: consumed    !< Bytes a hook consumed.
   logical                                  :: all_matched !< The line continues every open block.
   logical                                  :: opened      !< A block start claimed the line.
   logical                                  :: maybe_lazy  !< The tip is a paragraph.
   logical                                  :: claimed     !< A block start claimed the position.
   logical                                  :: leaf        !< The new block is a leaf.
   logical                                  :: done        !< The line is consumed.

   call p%start_line(ln, doc%line_first(ln), doc%line_last(ln))
   ! 1. continuation of the open blocks
   matched = 1_I4P
   all_matched = .true.
   do d=2, p%depth
      call p%find_nonspace()
      o = p%open_owner(d)
      if (o > 0_I4P) then
         call slots(o)%ext%block_continue(doc, p%open_node(d), p%src(p%lf:p%ll), p%ln, p%offset, p%column, p%indent, &
                                          match, consumed, ierr, msg)
         if (ierr /= BARUCH_OK) then
            msg = hook_message(slots, o, 'block_continue', ln, msg)
            return
         endif
         if (consumed < 0_I4P .or. consumed > p%remaining()) then
            call contract(slots, o, 'block_continue', ln, 'consumed '//istr(consumed)//' bytes of the '// &
                          istr(p%remaining())//' left in the line', ierr, msg)
            return
         endif
         select case(match)
         case(BLOCK_UNMATCHED)
         case(BLOCK_MATCHED)
            call p%advance_bytes(consumed)
         case(BLOCK_FINISHED) ! the block takes the whole line and closes; the line is done
            p%open_last(d) = p%ll
            call close_blocks(p, doc, slots, d, ierr, msg)
            return
         case default
            call contract(slots, o, 'block_continue', ln, 'answered '//istr(match)// &
                          ', not BLOCK_UNMATCHED, BLOCK_MATCHED or BLOCK_FINISHED', ierr, msg)
            return
         endselect
      else
         call continue_core(p, doc, slots, d, match, ierr, msg)
         if (ierr /= BARUCH_OK .or. match == BLOCK_FINISHED .or. match == LINE_DONE) return
      endif
      if (match == BLOCK_UNMATCHED) then
         all_matched = .false.
         exit
      endif
      matched = d
      if (.not. p%line_blank) p%open_last(d) = p%ll
   enddo
   ! 2. block starts, at the deepest matched block
   cont = matched
   opened = .false.
   maybe_lazy = p%open_content(p%depth) == CONTENT_PARAGRAPH
   do
      if (p%open_content(cont) == CONTENT_LINES) exit
      call p%find_nonspace()
      call try_extension_starts(p, doc, slots, cont, all_matched, claimed, leaf, ierr, msg)
      if (ierr /= BARUCH_OK) return
      if (.not. claimed) then
         call try_core_starts(p, doc, slots, cont, all_matched, maybe_lazy, claimed, leaf, done, ierr, msg)
         if (ierr /= BARUCH_OK .or. done) return
      endif
      if (.not. claimed) exit
      opened = .true.
      maybe_lazy = .false.
      cont = p%depth
      if (leaf) exit
   enddo
   ! 3. the rest of the line
   call add_text(p, doc, slots, cont, opened, all_matched, ierr, msg)
   endsubroutine incorporate_line

   subroutine continue_core(p, doc, slots, d, match, ierr, msg)
   !< Say whether the line continues the open core block at depth `d` (CommonMark appendix, step 1), consuming its
   !< marker: `BLOCK_MATCHED`, `BLOCK_UNMATCHED`, `BLOCK_FINISHED` (a closing fence: the block closed, the line done)
   !< or `LINE_DONE` (a second blank line in a list: nothing else to do with the line, as cmark).
   type(block_parser),        intent(inout) :: p        !< Parser.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(in)    :: d        !< Depth of the block.
   integer(I4P),              intent(out)   :: match    !< What the line does to it.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: n        !< Node.
   integer(I4P)                             :: length   !< Closing fence length.
   integer(I4P)                             :: k        !< Counter.
   integer(I4P)                             :: tip      !< Kind of the tip.
   character                                :: fence    !< Fence character.

   match = BLOCK_UNMATCHED
   n = p%open_node(d)
   select case(doc%node_kind(n))
   case(NODE_BLOCK_QUOTE)
      if (p%indent <= 3_I4P .and. p%char_at(p%nonspace) == '>') then
         call p%advance_columns(p%indent + 1_I4P)
         if (is_space_or_tab(p%char_at(p%offset))) call p%advance_columns(1_I4P)
         match = BLOCK_MATCHED
      endif
   case(NODE_LIST)
      match = BLOCK_MATCHED
      if (p%blank) then
         if (p%flag(n, FLAG_LIST_LAST_LINE_BLANK) .and. p%indent == 0_I4P) then
            ! a second blank line: every open block below already closed at the first (no quadratic walk of deep lists)
            tip = doc%node_kind(p%open_node(p%depth))
            if (p%open_owner(p%depth) == 0_I4P .and. (tip == NODE_CODE_BLOCK .or. tip == NODE_HTML_BLOCK)) &
               call p%add_line()
            match = LINE_DONE
            return
         endif
         call p%set_flag(n, FLAG_LIST_LAST_LINE_BLANK, .true.)
      else
         call p%set_flag(n, FLAG_LIST_LAST_LINE_BLANK, .false.)
      endif
   case(NODE_ITEM)
      if (p%indent >= p%open_a(d)) then
         call p%advance_columns(p%open_a(d))
         match = BLOCK_MATCHED
      elseif (p%blank .and. first_real_child(doc, n) /= 0_I4P) then
         call p%advance_bytes(p%nonspace - p%offset)
         match = BLOCK_MATCHED
      endif
   case(NODE_CODE_BLOCK)
      if (doc%int_slot(n, SLOT_CODE_FORM) == CODE_INDENTED) then
         if (p%indent >= CODE_INDENT) then
            call p%advance_columns(CODE_INDENT)
            match = BLOCK_MATCHED
         elseif (p%blank) then
            call p%advance_bytes(p%nonspace - p%offset)
            match = BLOCK_MATCHED
         endif
      else
         fence = achar(doc%int_slot(n, SLOT_CODE_FENCE_CHAR))
         length = 0_I4P
         if (p%indent <= 3_I4P .and. p%char_at(p%nonspace) == fence) &
            length = scan_close_fence(p%src(p%lf:p%ll), p%nonspace, fence)
         if (length >= doc%int_slot(n, SLOT_CODE_FENCE_LENGTH) .and. length > 0_I4P) then
            p%open_last(d) = p%ll
            call close_blocks(p, doc, slots, d, ierr, msg)
            match = BLOCK_FINISHED
         else
            k = p%open_a(d)
            do while (k > 0_I4P .and. is_space_or_tab(p%char_at(p%offset)))
               call p%advance_columns(1_I4P)
               k = k - 1_I4P
            enddo
            match = BLOCK_MATCHED
         endif
      endif
   case(NODE_HTML_BLOCK)
      if (p%open_a(d) <= 5_I4P .or. .not. p%blank) match = BLOCK_MATCHED
   case(NODE_PARAGRAPH)
      if (.not. p%blank) match = BLOCK_MATCHED
   endselect
   endsubroutine continue_core

   subroutine try_extension_starts(p, doc, slots, cont, all_matched, claimed, leaf, ierr, msg)
   !< Offer the position to the enabled extensions' block starts. On a claim: close the unmatched blocks (`all_matched`
   !< becomes true), take the paragraph lines asked for, link the new block; `leaf` true if the new block is a leaf.
   type(block_parser),        intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   type(extension_slot),      intent(in)    :: slots(:)    !< Extensions of the options.
   integer(I4P),              intent(in)    :: cont        !< Depth of the deepest matched block.
   logical,                   intent(inout) :: all_matched !< Every open block matched (or the unmatched are closed).
   logical,                   intent(out)   :: claimed     !< A block start claimed the position.
   logical,                   intent(out)   :: leaf        !< The new block is a leaf.
   integer(I4P),              intent(inout) :: ierr        !< Status.
   character(:), allocatable, intent(inout) :: msg         !< Message.
   integer(I4P)                             :: host        !< Depth of the block a new block would open in.
   integer(I4P)                             :: paragraph   !< Open paragraph at the position, 0 if none.
   integer(I4P)                             :: lines       !< Its lines.
   integer(I4P)                             :: a           !< Counter.
   integer(I4P)                             :: o           !< Slot of the claiming extension.
   integer(I4P)                             :: node        !< Claimed block.
   integer(I4P)                             :: consumed    !< Bytes consumed by the claim.
   integer(I4P)                             :: taken       !< Paragraph lines taken.
   integer(I4P)                             :: content     !< Content of the claimed block.
   integer(I4P)                             :: first       !< First byte of its span.
   character(:), allocatable                :: why         !< Contract violation.

   claimed = .false.
   leaf = .false.
   if (size(p%active) == 0) return
   host = cont
   paragraph = 0_I4P
   lines = 0_I4P
   if (p%open_content(cont) == CONTENT_PARAGRAPH) then
      host = cont - 1_I4P
      paragraph = p%open_node(cont)
      lines = p%tip_lines
   endif
   node = 0_I4P
   o = 0_I4P
   consumed = 0_I4P
   taken = 0_I4P
   do a=1, size(p%active, kind=I4P)
      o = p%active(a)
      call slots(o)%ext%block_start(doc, p%src(p%lf:p%ll), p%ln, p%offset, p%column, p%indent, p%open_node(host),   &
                                    paragraph, p%tip_first(1:lines), p%tip_last(1:lines), node, consumed, taken, &
                                    ierr, msg)
      if (ierr /= BARUCH_OK) then
         msg = hook_message(slots, o, 'block_start', p%ln, msg)
         return
      endif
      if (node /= 0_I4P) exit
   enddo
   if (node == 0_I4P) return
   ! the contract
   why = ''
   content = CONTENT_NONE
   if (.not. doc%is_node(node)) then
      why = 'returned node '//istr(node)//', not a node of the document'
   elseif (doc%node_kind(node) < NODE_EXTENSION_FIRST .or. doc%kind_is_inline(doc%node_kind(node))) then
      why = 'returned node '//istr(node)//' of kind '//istr(doc%node_kind(node))//', not a block kind of an extension'
   elseif (doc%parent(node) /= 0_I4P .or. doc%first_child(node) /= 0_I4P) then
      why = 'returned node '//istr(node)//', not a new detached block (it has a parent or children)'
   else
      content = slots(o)%ext%block_content(doc%node_kind(node))
      if (content /= CONTENT_NONE .and. content /= CONTENT_BLOCKS .and. content /= CONTENT_LINES) then
         why = 'block_content of kind '//istr(doc%node_kind(node))//' is '//istr(content)// &
               ', not CONTENT_NONE, CONTENT_BLOCKS or CONTENT_LINES'
      elseif (consumed < 0_I4P .or. consumed > p%remaining()) then
         why = 'consumed '//istr(consumed)//' bytes of the '//istr(p%remaining())//' left in the line'
      elseif (consumed == 0_I4P .and. content /= CONTENT_LINES) then
         why = 'claimed a block that takes no lines without consuming input (it would be offered the same position '// &
               'again, forever)'
      elseif (taken < 0_I4P .or. taken > lines) then
         why = 'took '//istr(taken)//' lines of a paragraph of '//istr(lines)
      endif
   endif
   if (len(why) > 0) then
      if (doc%is_node(node) .and. doc%parent(node) == 0_I4P .and. node /= doc%root()) call doc%unlink(node)
      call contract(slots, o, 'block_start', p%ln, why, ierr, msg)
      return
   endif
   ! the claim: the block's span starts at its marker
   first = p%lf + p%nonspace - 1_I4P
   call p%advance_bytes(consumed)
   if (.not. all_matched) call close_blocks(p, doc, slots, cont + 1_I4P, ierr, msg)
   if (ierr /= BARUCH_OK) return
   all_matched = .true.
   if (paragraph /= 0_I4P) then ! the paragraph keeps the lines not taken, and closes (it holds no block)
      p%tip_lines = p%tip_lines - taken
      if (p%tip_lines == 0_I4P) then
         call doc%unlink(paragraph)
         p%tombstones = .true.
         p%open_last(cont - 1_I4P) = max(p%open_last(cont - 1_I4P), p%open_last(cont))
         p%depth = p%depth - 1_I4P
      else
         p%open_last(cont) = p%tip_last(p%tip_lines)
         call close_blocks(p, doc, slots, cont, ierr, msg)
         if (ierr /= BARUCH_OK) return
      endif
   endif
   call add_block(p, doc, slots, node, o, content, first, 0_I4P, 0_I4P, ierr, msg)
   if (ierr /= BARUCH_OK) return
   claimed = .true.
   leaf = content /= CONTENT_BLOCKS
   endsubroutine try_extension_starts

   subroutine try_core_starts(p, doc, slots, cont, all_matched, maybe_lazy, claimed, leaf, done, ierr, msg)
   !< Offer the position to the core block starts, in the spec's order (CommonMark appendix, step 2). On a claim the
   !< new block is linked and pushed (`leaf` true if it takes no blocks); a setext underline turning the paragraph into
   !< a heading consumes the line (`done`).
   type(block_parser),        intent(inout) :: p             !< Parser.
   type(markdown_document),   intent(inout) :: doc           !< Document.
   type(extension_slot),      intent(in)    :: slots(:)      !< Extensions of the options.
   integer(I4P),              intent(in)    :: cont          !< Depth of the deepest matched (or new) block.
   logical,                   intent(inout) :: all_matched   !< Every open block matched (or the unmatched are closed).
   logical,                   intent(in)    :: maybe_lazy    !< The tip is a paragraph (no indented code, no type 7).
   logical,                   intent(out)   :: claimed       !< A block start claimed the position.
   logical,                   intent(out)   :: leaf          !< The new block is a leaf.
   logical,                   intent(out)   :: done          !< The line is consumed.
   integer(I4P),              intent(inout) :: ierr          !< Status.
   character(:), allocatable, intent(inout) :: msg           !< Message.
   integer(I4P)                             :: ns            !< First non-blank byte of the line from the offset.
   integer(I4P)                             :: first         !< Its byte in the source.
   integer(I4P)                             :: node          !< New block.
   integer(I4P)                             :: level         !< Heading level.
   integer(I4P)                             :: matched       !< Bytes of a marker.
   integer(I4P)                             :: break_char    !< Thematic break character.
   integer(I4P)                             :: fail_at       !< Where a thematic break scan failed.
   integer(I4P)                             :: htype         !< HTML block type.
   integer(I4P)                             :: marker        !< List bullet or delimiter.
   integer(I4P)                             :: number        !< Ordered list start.
   integer(I4P)                             :: marker_offset !< Indentation of a list marker.
   integer(I4P)                             :: padding       !< Columns from the marker to the item's content.
   integer(I4P)                             :: spaces        !< Columns of white space after the marker.
   integer(I4P)                             :: save_offset   !< Offset after the marker.
   integer(I4P)                             :: save_column   !< Column after the marker.
   logical                                  :: save_partial  !< Partly consumed tab after the marker.
   logical                                  :: indented      !< Indented 4 columns or more.
   logical                                  :: is_paragraph  !< The block at `cont` is a paragraph.
   logical                                  :: ordered       !< An ordered list marker.

   claimed = .false.
   leaf = .false.
   done = .false.
   ns = p%nonspace
   first = p%lf + ns - 1_I4P
   indented = p%indent >= CODE_INDENT
   is_paragraph = p%open_content(cont) == CONTENT_PARAGRAPH
   if (.not. indented) then
      ! block quote
      if (p%char_at(ns) == '>') then
         call p%advance_bytes(ns + 1_I4P - p%offset)
         if (is_space_or_tab(p%char_at(p%offset))) call p%advance_columns(1_I4P)
         call open_core(p, doc, slots, NODE_BLOCK_QUOTE, CONTENT_BLOCKS, first, 0_I4P, 0_I4P, cont, all_matched, node, &
                        ierr, msg)
         claimed = ierr == BARUCH_OK
         return
      endif
      ! ATX heading
      call scan_atx_start(p%src(p%lf:p%ll), ns, level, matched)
      if (level > 0_I4P) then
         call p%advance_bytes(ns + matched - p%offset)
         call open_core(p, doc, slots, NODE_HEADING, CONTENT_LINES, first, 0_I4P, 0_I4P, cont, all_matched, node, &
                        ierr, msg)
         if (ierr == BARUCH_OK) call doc%set_heading_level(node, level, stat=ierr, errmsg=msg)
         if (ierr == BARUCH_OK) call doc%set_int_slot(node, SLOT_HEADING_FORM, HEADING_ATX, stat=ierr, errmsg=msg)
         claimed = ierr == BARUCH_OK
         leaf = .true.
         return
      endif
      ! fenced code
      matched = scan_open_fence(p%src(p%lf:p%ll), ns)
      if (matched > 0_I4P) then
         call open_core(p, doc, slots, NODE_CODE_BLOCK, CONTENT_LINES, first, ns - p%offset, 0_I4P, cont, &
                        all_matched, node, ierr, msg)
         if (ierr == BARUCH_OK) call doc%set_int_slot(node, SLOT_CODE_FORM, CODE_FENCED, stat=ierr, errmsg=msg)
         if (ierr == BARUCH_OK) call doc%set_int_slot(node, SLOT_CODE_FENCE_CHAR,                    &
                                                     iachar(p%src(first:first), kind=I4P), stat=ierr, errmsg=msg)
         if (ierr == BARUCH_OK) call doc%set_int_slot(node, SLOT_CODE_FENCE_LENGTH, matched, stat=ierr, errmsg=msg)
         call p%advance_bytes(ns + matched - p%offset)
         claimed = ierr == BARUCH_OK
         leaf = .true.
         return
      endif
      ! HTML block (the tag is content: nothing consumed)
      htype = scan_html_block_start(p%src(p%lf:p%ll), ns, allow7=.not. is_paragraph .and. .not. maybe_lazy)
      if (htype > 0_I4P) then
         call open_core(p, doc, slots, NODE_HTML_BLOCK, CONTENT_LINES, first, htype, 0_I4P, cont, all_matched, node, &
                        ierr, msg)
         claimed = ierr == BARUCH_OK
         leaf = .true.
         return
      endif
      ! setext heading underline, under a paragraph
      if (is_paragraph) then
         level = scan_setext_underline(p%src(p%lf:p%ll), ns)
         if (level > 0_I4P) then
            call setext_heading(p, doc, cont, level, done, ierr, msg)
            return ! not a heading (the paragraph was definitions only): the line is paragraph text
         endif
      endif
      ! thematic break
      if (p%break_fail <= ns) then
         call scan_thematic_break(p%src(p%lf:p%ll), ns, break_char, fail_at)
         if (fail_at > 0_I4P) p%break_fail = fail_at
         if (break_char > 0_I4P) then
            call open_core(p, doc, slots, NODE_THEMATIC_BREAK, CONTENT_NONE, first, 0_I4P, 0_I4P, cont, all_matched, &
                           node, ierr, msg)
            if (ierr == BARUCH_OK) call doc%set_int_slot(node, SLOT_BREAK_CHAR, break_char, stat=ierr, errmsg=msg)
            call p%advance_bytes(p%remaining())
            claimed = ierr == BARUCH_OK
            leaf = .true.
            return
         endif
      endif
      ! list item
      call scan_list_marker(p%src(p%lf:p%ll), ns, is_paragraph, ordered, marker, number, matched)
      if (matched > 0_I4P) then
         marker_offset = p%indent
         call p%advance_bytes(ns + matched - p%offset)
         save_partial = p%partial_tab
         save_offset = p%offset
         save_column = p%column
         do while (p%column - save_column <= 5_I4P .and. is_space_or_tab(p%char_at(p%offset)))
            call p%advance_columns(1_I4P)
         enddo
         spaces = p%column - save_column
         if (spaces >= 5_I4P .or. spaces < 1_I4P .or. p%char_at(p%offset) == LF) then
            ! content indented 5 or more (indented code in the item), or none: one space belongs to the marker
            padding = matched + 1_I4P
            p%offset = save_offset
            p%column = save_column
            p%partial_tab = save_partial
            if (spaces > 0_I4P) call p%advance_columns(1_I4P)
         else
            padding = matched + spaces
         endif
         if (.not. lists_match(p, doc, cont, ordered, marker)) then
            call open_core(p, doc, slots, NODE_LIST, CONTENT_BLOCKS, first, 0_I4P, 0_I4P, cont, all_matched, node, &
                           ierr, msg)
            if (ierr == BARUCH_OK) call doc%set_list_type(node, merge(LIST_ORDERED, LIST_BULLET, ordered), stat=ierr, &
                                                          errmsg=msg)
            if (ierr == BARUCH_OK .and. ordered) call doc%set_list_start(node, number, stat=ierr, errmsg=msg)
            if (ierr == BARUCH_OK) call doc%set_int_slot(node, SLOT_LIST_MARKER, marker, stat=ierr, errmsg=msg)
            if (ierr /= BARUCH_OK) return
         endif
         call open_core(p, doc, slots, NODE_ITEM, CONTENT_BLOCKS, first, marker_offset + padding, p%ln, cont, &
                        all_matched, node, ierr, msg)
         claimed = ierr == BARUCH_OK
         return
      endif
   elseif (.not. maybe_lazy .and. .not. p%blank) then
      ! indented code
      call p%advance_columns(CODE_INDENT)
      call open_core(p, doc, slots, NODE_CODE_BLOCK, CONTENT_LINES, first, 0_I4P, 0_I4P, cont, all_matched, node, &
                     ierr, msg)
      if (ierr == BARUCH_OK) call doc%set_int_slot(node, SLOT_CODE_FORM, CODE_INDENTED, stat=ierr, errmsg=msg)
      claimed = ierr == BARUCH_OK
      leaf = .true.
   endif
   endsubroutine try_core_starts

   subroutine open_core(p, doc, slots, kind, content, first, a, b, cont, all_matched, node, ierr, msg)
   !< Open a core block: the unmatched blocks below `cont` close, the new node is linked in the deepest open block that
   !< can hold it and pushed.
   type(block_parser),        intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   type(extension_slot),      intent(in)    :: slots(:)    !< Extensions of the options.
   integer(I4P),              intent(in)    :: kind        !< Kind.
   integer(I4P),              intent(in)    :: content     !< Content.
   integer(I4P), value,       intent(in)    :: first       !< First byte of its span.
   integer(I4P), value,       intent(in)    :: a           !< Core integer `a`.
   integer(I4P), value,       intent(in)    :: b           !< Core integer `b` (callers pass `p%ln`).
   integer(I4P),              intent(in)    :: cont        !< Depth of the deepest matched block.
   logical,                   intent(inout) :: all_matched !< Every open block matched (or the unmatched are closed).
   integer(I4P),              intent(out)   :: node        !< New block.
   integer(I4P),              intent(inout) :: ierr        !< Status.
   character(:), allocatable, intent(inout) :: msg         !< Message.

   node = 0_I4P
   if (.not. all_matched) then
      call close_blocks(p, doc, slots, cont + 1_I4P, ierr, msg)
      if (ierr /= BARUCH_OK) return
      all_matched = .true.
   endif
   node = doc%new_node(kind, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   call add_block(p, doc, slots, node, 0_I4P, content, first, a, b, ierr, msg)
   endsubroutine open_core

   function lists_match(p, doc, cont, ordered, marker) result(match)
   !< True if the block at `cont` is a list a new item of this marker belongs to: same type, same bullet or delimiter.
   type(block_parser),      intent(in) :: p       !< Parser.
   type(markdown_document), intent(in) :: doc     !< Document.
   integer(I4P),            intent(in) :: cont    !< Depth.
   logical,                 intent(in) :: ordered !< The item is ordered.
   integer(I4P),            intent(in) :: marker  !< Its bullet or delimiter.
   logical                             :: match   !< Result.
   integer(I4P)                        :: n       !< Node.

   match = .false.
   n = p%open_node(cont)
   if (p%open_owner(cont) /= 0_I4P) return
   if (doc%node_kind(n) /= NODE_LIST) return
   match = doc%int_slot(n, SLOT_LIST_TYPE) == merge(LIST_ORDERED, LIST_BULLET, ordered) .and. &
           doc%int_slot(n, SLOT_LIST_MARKER) == marker
   endfunction lists_match

   subroutine setext_heading(p, doc, cont, level, done, ierr, msg)
   !< A setext underline under the paragraph at `cont` (the tip): its leading link reference definitions go first; if
   !< text is left, a heading of it replaces the paragraph (`done`: the line is consumed).
   type(block_parser),        intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(in)    :: cont  !< Depth of the paragraph.
   integer(I4P),              intent(in)    :: level !< Heading level.
   logical,                   intent(out)   :: done  !< The paragraph became a heading.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   integer(I4P)                             :: para  !< Paragraph.
   integer(I4P)                             :: h     !< Heading.
   integer(I4P)                             :: first !< First byte of the heading.
   integer(I4P)                             :: d     !< Depth.

   done = .false.
   para = p%open_node(cont)
   call extract_definitions(p, doc, para, ierr, msg)
   if (ierr /= BARUCH_OK .or. p%tip_lines == 0_I4P) return
   first = first_nonspace(p, 1_I4P)
   h = doc%new_node(NODE_HEADING, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%insert_before(para, h, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_heading_level(h, level, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_int_slot(h, SLOT_HEADING_FORM, HEADING_SETEXT, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_span(h, first, p%ll, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call add_text_node(doc, h, paragraph_text(p), first, p%tip_last(p%tip_lines), ierr, msg)
   if (ierr == BARUCH_OK) call doc%unlink(para, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   p%tombstones = .true.
   p%tip_lines = 0_I4P
   p%depth = cont - 1_I4P
   p%open_last(p%depth) = max(p%open_last(p%depth), p%ll)
   do d=p%depth, 1_I4P, -1_I4P ! the line is not blank for the heading's ancestors (CommonMark 5.3 bookkeeping)
      call p%set_flag(p%open_node(d), FLAG_LAST_LINE_BLANK, .false.)
   enddo
   done = .true.
   endsubroutine setext_heading

   subroutine add_text(p, doc, slots, c, opened, all_matched, ierr, msg)
   !< The rest of the line (CommonMark appendix, step 3): the blank-line bookkeeping, then a lazy continuation line to
   !< the paragraph, or the unmatched blocks closed and the rest given to the block at depth `c`, or a new paragraph.
   type(block_parser),        intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   type(extension_slot),      intent(in)    :: slots(:)    !< Extensions of the options.
   integer(I4P),              intent(in)    :: c           !< Depth of the block the line ended in.
   logical,                   intent(in)    :: opened      !< A block was opened on the line.
   logical,                   intent(in)    :: all_matched !< Every open block matched.
   integer(I4P),              intent(inout) :: ierr        !< Status.
   character(:), allocatable, intent(inout) :: msg         !< Message.
   integer(I4P)                             :: n           !< Node at `c`.
   integer(I4P)                             :: k           !< Its kind.
   integer(I4P)                             :: last        !< Its last child.
   integer(I4P)                             :: d           !< Depth.
   logical                                  :: blank       !< The line counts as blank for `n`.

   call p%find_nonspace()
   n = p%open_node(c)
   k = doc%node_kind(n)
   ! bookkeeping for tight and loose lists (cmark's rules)
   if (p%blank) then
      last = last_real_child(doc, n)
      if (last /= 0_I4P) call p%set_flag(last, FLAG_LAST_LINE_BLANK, .true.)
   endif
   blank = p%blank
   if (blank .and. p%open_owner(c) == 0_I4P) then
      select case(k)
      case(NODE_BLOCK_QUOTE, NODE_HEADING, NODE_THEMATIC_BREAK)
         blank = .false. ! a block quote line starts with `>`
      case(NODE_CODE_BLOCK)
         blank = doc%int_slot(n, SLOT_CODE_FORM) /= CODE_FENCED ! blanks in fenced code do not count
      case(NODE_ITEM)
         blank = first_real_child(doc, n) /= 0_I4P .or. p%open_b(c) /= p%ln ! an empty item opened on this line
      endselect
   endif
   call p%set_flag(n, FLAG_LAST_LINE_BLANK, blank)
   do d=c-1_I4P, 1_I4P, -1_I4P
      call p%set_flag(p%open_node(d), FLAG_LAST_LINE_BLANK, .false.)
   enddo
   ! a lazy continuation line (CommonMark 5.1)
   if (.not. opened .and. .not. all_matched .and. .not. p%blank .and. p%open_content(p%depth) == CONTENT_PARAGRAPH) then
      call p%add_line()
      p%open_last(p%depth) = p%ll
      return
   endif
   if (.not. opened .and. .not. all_matched) then
      call close_blocks(p, doc, slots, c + 1_I4P, ierr, msg)
      if (ierr /= BARUCH_OK) return
   endif
   if (p%open_owner(c) > 0_I4P) then ! an extension's block: lines if it takes them, else a paragraph
      if (p%open_content(c) == CONTENT_LINES) then
         call p%add_line()
         if (.not. p%line_blank) p%open_last(c) = p%ll
      elseif (.not. p%blank) then
         call open_paragraph(p, doc, slots, ierr, msg)
      endif
      return
   endif
   select case(k)
   case(NODE_CODE_BLOCK)
      call p%add_line()
      if (.not. p%line_blank) p%open_last(c) = p%ll
   case(NODE_HTML_BLOCK)
      call p%add_line()
      if (.not. p%line_blank) p%open_last(c) = p%ll
      if (html_block_end(p%src(p%lf:p%ll), p%nonspace, p%open_a(c))) call close_blocks(p, doc, slots, c, ierr, msg)
   case default
      if (p%blank) then
         ! nothing to add
      elseif (k == NODE_PARAGRAPH .or. k == NODE_HEADING) then
         call p%advance_bytes(p%nonspace - p%offset)
         call p%add_line()
         p%open_last(c) = p%ll
      else
         call open_paragraph(p, doc, slots, ierr, msg)
      endif
   endselect
   endsubroutine add_text

   subroutine open_paragraph(p, doc, slots, ierr, msg)
   !< Open a paragraph at the first non-blank byte, the rest of the line its first line.
   type(block_parser),        intent(inout) :: p        !< Parser.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: node     !< Paragraph.

   node = doc%new_node(NODE_PARAGRAPH, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   call p%advance_bytes(p%nonspace - p%offset)
   call add_block(p, doc, slots, node, 0_I4P, CONTENT_PARAGRAPH, p%lf + p%nonspace - 1_I4P, 0_I4P, 0_I4P, ierr, msg)
   if (ierr /= BARUCH_OK) return
   call p%add_line()
   endsubroutine open_paragraph

   subroutine add_block(p, doc, slots, node, owner, content, first, a, b, ierr, msg)
   !< Link a new block as the last child of the deepest open block that can hold it (the ones that cannot are closed)
   !< and push it; a block taking lines starts with none.
   type(block_parser),        intent(inout) :: p        !< Parser.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(in)    :: node     !< New block.
   integer(I4P),              intent(in)    :: owner    !< Its owner.
   integer(I4P),              intent(in)    :: content  !< Its content.
   integer(I4P), value,       intent(in)    :: first    !< First byte of its span.
   integer(I4P), value,       intent(in)    :: a        !< Core integer `a`.
   integer(I4P), value,       intent(in)    :: b        !< Core integer `b` (callers pass `p%ln`).
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.

   do while (.not. can_hold(p, doc, slots, p%depth, doc%node_kind(node)))
      call close_blocks(p, doc, slots, p%depth, ierr, msg)
      if (ierr /= BARUCH_OK) return
   enddo
   call doc%append_child(p%open_node(p%depth), node, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   call p%push(node, owner, content, first, a, b)
   if (content == CONTENT_LINES .or. content == CONTENT_PARAGRAPH) p%tip_lines = 0_I4P
   endsubroutine add_block

   function can_hold(p, doc, slots, d, kind) result(can)
   !< Return true if the open block at depth `d` can hold a block of `kind`: the document, a block quote and an item
   !< hold any block, a list items only, an extension's container what its `can_contain` says, a leaf nothing.
   type(block_parser),      intent(in) :: p        !< Parser.
   type(markdown_document), intent(in) :: doc      !< Document.
   type(extension_slot),    intent(in) :: slots(:) !< Extensions of the options.
   integer(I4P),            intent(in) :: d        !< Depth.
   integer(I4P),            intent(in) :: kind     !< Kind of the new block.
   logical                             :: can      !< Result.
   integer(I4P)                        :: o        !< Owner.

   can = .false.
   if (p%open_content(d) /= CONTENT_BLOCKS) return
   o = p%open_owner(d)
   if (o > 0_I4P) then
      can = slots(o)%ext%can_contain(doc%node_kind(p%open_node(d)), kind)
   else
      select case(doc%node_kind(p%open_node(d)))
      case(NODE_DOCUMENT, NODE_BLOCK_QUOTE, NODE_ITEM)
         can = .true.
      case(NODE_LIST)
         can = kind == NODE_ITEM
      endselect
   endif
   endfunction can_hold

   subroutine close_blocks(p, doc, slots, d_from, ierr, msg)
   !< Close the open blocks from the tip up to depth `d_from` (deepest first): spans set, the core's finishing (or
   !< `block_close`) done. A closed block's last byte extends its parent's.
   type(block_parser),        intent(inout) :: p        !< Parser.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P), value,       intent(in)    :: d_from   !< Shallowest depth to close (2 or more): a copy, callers
                                                         !< pass `p%depth`, which the loop changes.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: d        !< Depth.
   integer(I4P)                             :: n        !< Node.
   integer(I4P)                             :: o        !< Owner.
   integer(I4P)                             :: lines    !< Content lines handed to the block.
   integer(I4P)                             :: k        !< Counter.

   do while (p%depth >= max(2_I4P, d_from))
      d = p%depth
      n = p%open_node(d)
      o = p%open_owner(d)
      call doc%set_span(n, p%open_first(d), max(p%open_last(d), p%open_first(d) - 1_I4P))
      lines = 0_I4P
      if (p%open_content(d) == CONTENT_LINES .or. p%open_content(d) == CONTENT_PARAGRAPH) lines = p%tip_lines
      if (o > 0_I4P) then
         do k=1, lines ! a partly consumed tab is given whole: a byte range cannot hold part of it
            if (p%tip_pad(k) > 0_I4P) p%tip_first(k) = p%tip_first(k) - 1_I4P
            p%tip_pad(k) = 0_I4P
         enddo
         call slots(o)%ext%block_close(doc, n, p%tip_first(1:lines), p%tip_last(1:lines), ierr, msg)
         if (ierr /= BARUCH_OK) then
            msg = hook_message(slots, o, 'block_close', p%ln, msg)
            return
         endif
      else
         select case(doc%node_kind(n))
         case(NODE_PARAGRAPH)
            call finish_paragraph(p, doc, n, ierr, msg)
         case(NODE_HEADING)
            call finish_atx_heading(p, doc, n, ierr, msg)
         case(NODE_CODE_BLOCK)
            call finish_code_block(p, doc, n, ierr, msg)
         case(NODE_HTML_BLOCK)
            call doc%set_literal(n, lines_text(p, 1_I4P, p%tip_lines), stat=ierr, errmsg=msg)
         case(NODE_LIST)
            call doc%set_list_tight(n, list_is_tight(p, doc, n), stat=ierr, errmsg=msg)
         endselect
         if (ierr /= BARUCH_OK) return
      endif
      if (lines > 0_I4P) p%tip_lines = 0_I4P
      p%open_last(d - 1_I4P) = max(p%open_last(d - 1_I4P), p%open_last(d))
      p%depth = d - 1_I4P
   enddo
   endsubroutine close_blocks

   subroutine finish_paragraph(p, doc, n, ierr, msg)
   !< Finish a paragraph: its leading link reference definitions become nodes before it; what is left is one text node
   !< (no line left: the paragraph goes).
   type(block_parser),        intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(in)    :: n     !< Paragraph.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   integer(I4P)                             :: first !< First byte of its text.

   call extract_definitions(p, doc, n, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (p%tip_lines == 0_I4P) then
      call doc%unlink(n, stat=ierr, errmsg=msg)
      p%tombstones = .true.
      return
   endif
   first = first_nonspace(p, 1_I4P)
   call doc%set_span(n, first, p%tip_last(p%tip_lines), stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call add_text_node(doc, n, paragraph_text(p), first, p%tip_last(p%tip_lines), ierr, msg)
   endsubroutine finish_paragraph

   subroutine finish_atx_heading(p, doc, n, ierr, msg)
   !< Finish an ATX heading: its line without the trailing white space and the closing sequence (a run of `#` after a
   !< space or a tab, or the whole content), whose length is recorded; the rest is one text node.
   type(block_parser),        intent(inout) :: p       !< Parser.
   type(markdown_document),   intent(inout) :: doc     !< Document.
   integer(I4P),              intent(in)    :: n       !< Heading.
   integer(I4P),              intent(inout) :: ierr    !< Status.
   character(:), allocatable, intent(inout) :: msg     !< Message.
   integer(I4P)                             :: a, b    !< Bytes of the content.
   integer(I4P)                             :: k       !< Byte.
   integer(I4P)                             :: closing !< Closing `#`s.

   closing = 0_I4P
   if (p%tip_lines > 0_I4P) then
      a = p%tip_first(1)
      b = p%tip_last(1)
      do while (b >= a)
         if (.not. is_space_or_tab(p%src(b:b))) exit
         b = b - 1_I4P
      enddo
      k = b
      do while (k >= a)
         if (p%src(k:k) /= '#') exit
         k = k - 1_I4P
      enddo
      if (k < a) then ! only #s: all closing sequence
         closing = b - k
         b = a - 1_I4P
      elseif (k < b .and. is_space_or_tab(p%src(k:k))) then
         closing = b - k
         b = k
         do while (b >= a)
            if (.not. is_space_or_tab(p%src(b:b))) exit
            b = b - 1_I4P
         enddo
      endif
      if (b >= a) call add_text_node(doc, n, p%src(a:b), a, b, ierr, msg)
   endif
   if (ierr == BARUCH_OK) call doc%set_int_slot(n, SLOT_HEADING_CLOSING, closing, stat=ierr, errmsg=msg)
   endsubroutine finish_atx_heading

   subroutine finish_code_block(p, doc, n, ierr, msg)
   !< Finish a code block: a fenced one's first line is its info string (trimmed, backslash escapes processed), the
   !< other lines its literal; an indented one drops its trailing blank lines. Each line of the literal ends with LF.
   type(block_parser),        intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(in)    :: n     !< Code block.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   integer(I4P)                             :: a, b  !< Bytes of the info string.
   integer(I4P)                             :: last  !< Last line of the literal.
   integer(I4P)                             :: k     !< Byte.

   if (doc%int_slot(n, SLOT_CODE_FORM) == CODE_FENCED) then
      if (p%tip_lines > 0_I4P) then
         a = p%tip_first(1)
         b = p%tip_last(1)
         do while (a <= b)
            if (.not. is_space_or_tab(p%src(a:a))) exit
            a = a + 1_I4P
         enddo
         do while (b >= a)
            if (.not. is_space_or_tab(p%src(b:b))) exit
            b = b - 1_I4P
         enddo
         if (b >= a) call doc%set_info(n, backslash_unescape(p%src(a:b)), stat=ierr, errmsg=msg)
      endif
      if (ierr == BARUCH_OK) call doc%set_literal(n, lines_text(p, 2_I4P, p%tip_lines), stat=ierr, errmsg=msg)
   else
      last = p%tip_lines
      do while (last > 0_I4P)
         k = p%tip_first(last)
         do while (k <= p%tip_last(last))
            if (.not. is_space_or_tab(p%src(k:k))) exit
            k = k + 1_I4P
         enddo
         if (k <= p%tip_last(last)) exit
         last = last - 1_I4P
      enddo
      call doc%set_literal(n, lines_text(p, 1_I4P, last), stat=ierr, errmsg=msg)
   endif
   endsubroutine finish_code_block

   function list_is_tight(p, doc, list) result(tight)
   !< Return true if a list is tight (CommonMark 5.3, cmark's rule): no item but the last ends with a blank line, and
   !< no two children of an item (or the last child of an item not the last) are separated by one. Link reference
   !< definitions do not count as children.
   type(block_parser),      intent(inout) :: p     !< Parser.
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: list  !< List.
   logical                                :: tight !< Result.
   integer(I4P)                           :: item  !< Item.
   integer(I4P)                           :: sub   !< Child of an item.
   integer(I4P)                           :: next  !< Next real sibling of `sub`.

   tight = .true.
   item = doc%first_child(list)
   do while (item /= 0_I4P)
      if (p%flag(item, FLAG_LAST_LINE_BLANK) .and. doc%next_sibling(item) /= 0_I4P) then
         tight = .false.
         return
      endif
      sub = first_real_child(doc, item)
      do while (sub /= 0_I4P)
         next = next_real_sibling(doc, sub)
         if (doc%next_sibling(item) /= 0_I4P .or. next /= 0_I4P) then
            if (ends_with_blank_line(p, doc, sub)) then
               tight = .false.
               return
            endif
         endif
         sub = next
      enddo
      item = doc%next_sibling(item)
   enddo
   endfunction list_is_tight

   function ends_with_blank_line(p, doc, node) result(blank)
   !< Return true if a block ends with a blank line, descending into the last child of lists and items (cmark's
   !< `S_ends_with_blank_line`, iterative, the answer cached per node).
   type(block_parser),      intent(inout) :: p     !< Parser.
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: node  !< Block.
   logical                                :: blank !< Result.
   integer(I4P)                           :: n     !< Current block.
   integer(I4P)                           :: last  !< Its last child.

   n = node
   do
      if (p%flag(n, FLAG_LAST_LINE_CHECKED)) exit
      call p%set_flag(n, FLAG_LAST_LINE_CHECKED, .true.)
      if (doc%node_kind(n) /= NODE_LIST .and. doc%node_kind(n) /= NODE_ITEM) exit
      last = last_real_child(doc, n)
      if (last == 0_I4P) exit
      n = last
   enddo
   blank = p%flag(n, FLAG_LAST_LINE_BLANK)
   endfunction ends_with_blank_line

   subroutine extract_definitions(p, doc, para, ierr, msg)
   !< Take the link reference definitions at the start of the paragraph's lines out of them: each one a node in place,
   !< before the paragraph (CommonMark 4.7). A definition takes whole lines.
   type(block_parser),        intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   integer(I4P),              intent(in)    :: para        !< Paragraph.
   integer(I4P),              intent(inout) :: ierr        !< Status.
   character(:), allocatable, intent(inout) :: msg         !< Message.
   character(:), allocatable                :: text        !< The lines, white space stripped, each ended by LF.
   character(:), allocatable                :: destination !< Destination.
   character(:), allocatable                :: title       !< Title.
   integer(I4P)                             :: pos         !< Position in `text`.
   integer(I4P)                             :: next        !< Position after a definition.
   integer(I4P)                             :: label_first !< First byte of the label.
   integer(I4P)                             :: label_last  !< Last byte of the label.
   integer(I4P)                             :: k           !< First line of a definition.
   integer(I4P)                             :: lines       !< Lines of a definition.
   integer(I4P)                             :: j           !< Counter.
   integer(I4P)                             :: def         !< Definition node.

   if (p%tip_lines == 0_I4P) return
   if (p%src(first_nonspace(p, 1_I4P):first_nonspace(p, 1_I4P)) /= '[') return
   text = lines_text(p, 1_I4P, p%tip_lines, strip=.true.)
   pos = 1_I4P
   k = 1_I4P
   do while (pos <= len(text, kind=I4P))
      if (text(pos:pos) /= '[') exit
      call parse_definition(text, pos, next, label_first, label_last, destination, title)
      if (next == 0_I4P) exit
      lines = 0_I4P
      do j=pos, next - 1_I4P
         if (text(j:j) == LF) lines = lines + 1_I4P
      enddo
      def = doc%new_node(NODE_LINK_REFERENCE_DEFINITION, stat=ierr, errmsg=msg)
      if (ierr == BARUCH_OK) call doc%insert_before(para, def, stat=ierr, errmsg=msg)
      if (ierr == BARUCH_OK) call doc%set_label(def, text(label_first:label_last), stat=ierr, errmsg=msg)
      if (ierr == BARUCH_OK) call doc%set_destination(def, destination, stat=ierr, errmsg=msg)
      if (ierr == BARUCH_OK) call doc%set_title(def, title, stat=ierr, errmsg=msg)
      if (ierr == BARUCH_OK) call doc%set_span(def, first_nonspace(p, k), p%tip_last(k + lines - 1_I4P), stat=ierr, &
                                               errmsg=msg)
      if (ierr /= BARUCH_OK) return
      k = k + lines
      pos = next
   enddo
   call p%drop_lines(k - 1_I4P)
   endsubroutine extract_definitions

   subroutine add_text_node(doc, parent, text, first, last, ierr, msg)
   !< Append a text node of a literal and a span to a block.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(in)    :: parent !< Block.
   character(*),              intent(in)    :: text   !< Literal.
   integer(I4P),              intent(in)    :: first  !< First byte of its span.
   integer(I4P),              intent(in)    :: last   !< Last byte of its span.
   integer(I4P),              intent(inout) :: ierr   !< Status.
   character(:), allocatable, intent(inout) :: msg    !< Message.
   integer(I4P)                             :: t      !< Text node.

   t = doc%new_node(NODE_TEXT, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_literal(t, text, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_span(t, first, last, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%append_child(parent, t, stat=ierr, errmsg=msg)
   endsubroutine add_text_node

   function paragraph_text(p) result(text)
   !< Return the text of the tip's lines: each without its leading white space, joined by LF, the final trailing white
   !< space removed.
   type(block_parser), intent(in) :: p    !< Parser.
   character(:), allocatable      :: text !< Text.
   integer(I4P)                   :: n    !< Length.

   text = lines_text(p, 1_I4P, p%tip_lines, strip=.true.)
   n = len(text, kind=I4P)
   do while (n > 0_I4P)
      if (.not. (is_space_or_tab(text(n:n)) .or. text(n:n) == LF)) exit
      n = n - 1_I4P
   enddo
   text = text(1:n)
   endfunction paragraph_text

   function lines_text(p, from, to, strip) result(text)
   !< Return the tip's lines `from` to `to`, each followed by LF: its pad as spaces then its bytes, or (`strip`) its
   !< bytes from the first that is not a space or a tab. Sized first, filled once.
   type(block_parser), intent(in)           :: p     !< Parser.
   integer(I4P),       intent(in)           :: from  !< First line.
   integer(I4P),       intent(in)           :: to    !< Last line.
   logical,            intent(in), optional :: strip !< Strip the leading white space.
   character(:), allocatable                :: text  !< Text.
   integer(I4P)                             :: k     !< Line.
   integer(I4P)                             :: n     !< Length.
   integer(I4P)                             :: a     !< First byte of a line.
   integer(I4P)                             :: pad   !< Spaces before it.
   logical                                  :: strip_ !< Strip, local.

   strip_ = .false.
   if (present(strip)) strip_ = strip
   n = 0_I4P
   do k=from, to
      call line_bounds(k, a, pad)
      n = n + pad + max(0_I4P, p%tip_last(k) - a + 1_I4P) + 1_I4P
   enddo
   allocate(character(n) :: text)
   n = 0_I4P
   do k=from, to
      call line_bounds(k, a, pad)
      text(n+1:n+pad) = repeat(' ', pad)
      n = n + pad
      if (p%tip_last(k) >= a) then
         text(n+1:n+p%tip_last(k)-a+1) = p%src(a:p%tip_last(k))
         n = n + p%tip_last(k) - a + 1_I4P
      endif
      text(n+1:n+1) = LF
      n = n + 1_I4P
   enddo
   contains
      subroutine line_bounds(k, a, pad)
      !< First byte and pad of line `k`.
      integer(I4P), intent(in)  :: k   !< Line.
      integer(I4P), intent(out) :: a   !< First byte.
      integer(I4P), intent(out) :: pad !< Pad.

      if (strip_) then
         a = first_nonspace(p, k)
         pad = 0_I4P
      else
         a = p%tip_first(k)
         pad = p%tip_pad(k)
      endif
      endsubroutine line_bounds
   endfunction lines_text

   pure function first_nonspace(p, k) result(b)
   !< Return the first byte of the tip's line `k` that is not a space or a tab (past its last byte if none).
   type(block_parser), intent(in) :: p !< Parser.
   integer(I4P),       intent(in) :: k !< Line.
   integer(I4P)                   :: b !< Byte.

   b = p%tip_first(k)
   do while (b <= p%tip_last(k))
      if (.not. is_space_or_tab(p%src(b:b))) exit
      b = b + 1_I4P
   enddo
   endfunction first_nonspace

   function first_real_child(doc, n) result(c)
   !< Return the first child of a node that is not a link reference definition, 0 if none.
   type(markdown_document), intent(in) :: doc !< Document.
   integer(I4P),            intent(in) :: n   !< Node.
   integer(I4P)                        :: c   !< Child.

   c = doc%first_child(n)
   do while (c /= 0_I4P)
      if (doc%node_kind(c) /= NODE_LINK_REFERENCE_DEFINITION) exit
      c = doc%next_sibling(c)
   enddo
   endfunction first_real_child

   function last_real_child(doc, n) result(c)
   !< Return the last child of a node that is not a link reference definition, 0 if none.
   type(markdown_document), intent(in) :: doc !< Document.
   integer(I4P),            intent(in) :: n   !< Node.
   integer(I4P)                        :: c   !< Child.

   c = doc%last_child(n)
   do while (c /= 0_I4P)
      if (doc%node_kind(c) /= NODE_LINK_REFERENCE_DEFINITION) exit
      c = doc%prev_sibling(c)
   enddo
   endfunction last_real_child

   function next_real_sibling(doc, n) result(s)
   !< Return the next sibling of a node that is not a link reference definition, 0 if none.
   type(markdown_document), intent(in) :: doc !< Document.
   integer(I4P),            intent(in) :: n   !< Node.
   integer(I4P)                        :: s   !< Sibling.

   s = doc%next_sibling(n)
   do while (s /= 0_I4P)
      if (doc%node_kind(s) /= NODE_LINK_REFERENCE_DEFINITION) exit
      s = doc%next_sibling(s)
   enddo
   endfunction next_real_sibling

   subroutine contract(slots, o, hook, ln, why, ierr, msg)
   !< Report a hook contract violation.
   type(extension_slot),      intent(in)  :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(in)  :: o        !< Slot of the extension.
   character(*),              intent(in)  :: hook     !< Hook.
   integer(I4P),              intent(in)  :: ln       !< Line.
   character(*),              intent(in)  :: why      !< What broke the contract.
   integer(I4P),              intent(out) :: ierr     !< Status.
   character(:), allocatable, intent(out) :: msg      !< Message.

   ierr = BARUCH_ERR_CONTRACT
   msg = 'markdown_options%parse: extension "'//slots(o)%ext%name()//'", '//hook//' at line '//istr(ln)// &
         ', broke the hook contract: '//why
   endsubroutine contract

   function hook_message(slots, o, hook, ln, why) result(msg)
   !< Return the message of a hook that failed: the hook's own, prefixed by where.
   type(extension_slot),      intent(in) :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(in) :: o        !< Slot of the extension.
   character(*),              intent(in) :: hook     !< Hook.
   integer(I4P),              intent(in) :: ln       !< Line.
   character(:), allocatable, intent(in) :: why      !< The hook's message (may be unallocated).
   character(:), allocatable             :: msg      !< Message.

   msg = 'markdown_options%parse: extension "'//slots(o)%ext%name()//'", '//hook//' at line '//istr(ln)//' failed'
   if (allocated(why)) then
      if (len(why) > 0) msg = msg//': '//why
   endif
   endfunction hook_message

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i !< Integer.
   character(:), allocatable :: s !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr
endmodule baruch_blocks
