!< BARUCH, the block parser: a document's lines into its tree of blocks, the extensions' block hooks called.
module baruch_blocks
!< BARUCH, the block parser: a document's lines into its tree of blocks, the extensions' block hooks called.
!<
!< **Status (plan step 1.3): the frame.** This module drives the extensions' block hooks over the lines of the source
!< and builds the tree from what they claim; the CommonMark block starts are not written yet: a line no extension
!< claims is paragraph text (a blank line closes the paragraph). Step 1.5 fills the frame with the CommonMark block
!< algorithm (spec, appendix "A parsing strategy"), in the places marked `1.5` below.
!<
!< **The frame.** One pass over the lines (`markdown_document%line_first`/`line_last`, the line index of plan step 1.2),
!< no recursion, no per-container copy of the text:
!<
!< - the source is copied once, at the start (`src`); a line is the slice `src(lf:ll)` and the parser's position in it
!<   an **offset** (first byte not consumed) and a **column** (tab stops of 4, CommonMark 2.2): a container's marker
!<   is consumed by moving them, never by cutting the line;
!< - the **open blocks** are a stack of arrays (node, owner, content, first and last byte), the root at depth 1 and the
!<   tip (the deepest open block) on top. The owner is 0 for a core block, else the index of the extension (in the
!<   options' slots) that opened it: its hooks are the ones called for that block;
!< - only the tip can take lines (a paragraph or a `CONTENT_LINES` block: a leaf is always the tip), so one pair of
!<   arrays, `tip_first`/`tip_last`, holds the byte ranges of the tip's content lines; they are what `block_start` sees
!<   as the open paragraph's lines and `block_close` receives.
!<
!< Each line (`incorporate_line`, commonmark.js's shape):
!<
!< 1. **continuation**: every open block below the root, outermost first, says whether the line continues it
!<    (`block_continue` for an extension's block; `continue_core` for a core one) and consumes its marker; the first
!<    that does not ends the walk: the blocks below it are unmatched;
!< 2. **block starts**: unless the deepest matched block is a leaf taking lines, the extensions' `block_start` are
!<    tried at the position (in the order they were registered, before the core's starts: an extension can take
!<    syntax away from the core, as front matter takes `---`), then the core's (1.5). A claim closes the unmatched
!<    blocks, takes the paragraph lines it asked for, links the new block (closing the open blocks that cannot hold it:
!<    `can_contain`) and, for a container, tries again on the rest of the line; a leaf ends the loop;
!< 3. **the rest of the line**: a lazy continuation line goes to the open paragraph (1.5: only when no block start
!<    matched, CommonMark 5.1); otherwise the unmatched blocks close, and the rest goes to the tip if it takes lines,
!<    or opens a paragraph if it is not blank.
!<
!< Closing a block (`close_blocks`) sets its span (first byte of its first line's marker to the last byte of its last
!< line) and calls `block_close` (or the core's finishing: 1.5, link reference definitions out of paragraphs).
!<
!< **The hook contract**, checked here (`BARUCH_ERR_CONTRACT`, the parse stops, never loops): a claimed start returns a
!< new detached block of an extension kind; it consumes `0 <= consumed <= ` what is left of the line, and at least one
!< byte unless the block takes lines (a container claiming without consuming would be offered the same position
!< again, forever); `0 <= paragraph_taken <=` the paragraph's lines; a continuation consumes within the line and
!< answers one of the three `BLOCK_*` values. The block-start loop is therefore bounded by the bytes of the line.
!<
!< For 1.5 (not in the frame yet): partly consumed tabs (a container marker ending inside a tab leaves columns that
!< belong to the content: a `pad` per tip line), the core block starts and continuations, the blank-line bookkeeping
!< of lists (tight or loose), setext headings (taking the paragraph like `paragraph_taken`), link reference
!< definitions, the paragraph's raw content kept for the inline parser instead of the text node written now.
use penf,               only : I4P
use baruch_document,    only : markdown_document
use baruch_extension,   only : extension_slot, BLOCK_UNMATCHED, BLOCK_MATCHED, BLOCK_FINISHED, &
                               CONTENT_NONE, CONTENT_BLOCKS, CONTENT_LINES
use baruch_kinds_nodes, only : NODE_DOCUMENT, NODE_PARAGRAPH, NODE_TEXT, NODE_BLOCK_QUOTE, NODE_ITEM, &
                               NODE_EXTENSION_FIRST
use baruch_status
implicit none
private
public :: parse_blocks

integer(I4P), parameter :: STACK_DEFAULT = 32_I4P !< Open blocks and tip lines allocated up front (both grow).
integer(I4P), parameter :: CONTENT_PARAGRAPH = -1_I4P !< Content of a paragraph: lines, but blocks may start in it.
character,    parameter :: TAB = achar(9)  !< Tab.
character,    parameter :: LF  = achar(10) !< Line feed.

type :: block_parser
   !< The state of one parse: the source, the current line, the open blocks, the tip's lines.
   character(:), allocatable :: src                  !< The document's source, copied once.
   integer(I4P)              :: ln = 0_I4P           !< Current line number.
   integer(I4P)              :: lf = 0_I4P           !< Its first byte in `src`.
   integer(I4P)              :: ll = 0_I4P           !< Its last byte (ending excluded).
   integer(I4P)              :: offset = 1_I4P       !< First byte of the line not consumed (1-based in the line).
   integer(I4P)              :: column = 0_I4P       !< Column of `offset`.
   integer(I4P)              :: nonspace = 1_I4P     !< First byte at or after `offset` that is not a space or tab.
   integer(I4P)              :: indent = 0_I4P       !< Columns from `offset` to `nonspace`.
   logical                   :: blank = .true.       !< Nothing but spaces and tabs from `offset`.
   integer(I4P)              :: depth = 0_I4P        !< Open blocks, the root included.
   integer(I4P), allocatable :: open_node(:)         !< Node of each open block, root first.
   integer(I4P), allocatable :: open_owner(:)        !< Its owner: 0 the core, else the slot of its extension.
   integer(I4P), allocatable :: open_content(:)      !< Its content: `CONTENT_*` or `CONTENT_PARAGRAPH`.
   integer(I4P), allocatable :: open_first(:)        !< First byte of its span.
   integer(I4P), allocatable :: open_last(:)         !< Last byte of its span so far.
   integer(I4P)              :: tip_lines = 0_I4P    !< Content lines of the tip.
   integer(I4P), allocatable :: tip_first(:)         !< First byte of each.
   integer(I4P), allocatable :: tip_last(:)          !< Last byte of each.
   integer(I4P), allocatable :: active(:)            !< Slots of the enabled extensions, in order.
endtype block_parser

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
            p%open_first(STACK_DEFAULT), p%open_last(STACK_DEFAULT))
   allocate(p%tip_first(STACK_DEFAULT), p%tip_last(STACK_DEFAULT))
   p%depth = 1_I4P
   p%open_node(1) = doc%root()
   p%open_owner(1) = 0_I4P
   p%open_content(1) = CONTENT_BLOCKS
   p%open_first(1) = 1_I4P
   p%open_last(1) = 0_I4P
   p%tip_lines = 0_I4P
   allocate(p%active(0))
   do s=1, size(slots, kind=I4P)
      if (slots(s)%enabled .and. allocated(slots(s)%ext)) p%active = [p%active, s]
   enddo
   endsubroutine start

   subroutine incorporate_line(p, doc, slots, ln, ierr, msg)
   !< Incorporate a line: continue the open blocks, try the block starts, give the rest of the line to the tip.
   type(block_parser),        intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   type(extension_slot),      intent(in)    :: slots(:)    !< Extensions of the options.
   integer(I4P),              intent(in)    :: ln          !< Line number.
   integer(I4P),              intent(inout) :: ierr        !< Status.
   character(:), allocatable, intent(inout) :: msg         !< Message.
   integer(I4P)                             :: matched     !< Depth of the deepest block the line continues.
   integer(I4P)                             :: d           !< Depth.
   integer(I4P)                             :: o           !< Owner slot.
   integer(I4P)                             :: match       !< What a continuation says.
   integer(I4P)                             :: consumed    !< Bytes a hook consumed.
   logical                                  :: all_matched !< The line continues every open block.
   logical                                  :: leaf        !< A leaf is open at the position: no more starts.
   logical                                  :: claimed     !< A block start claimed the position.
   logical                                  :: lazy        !< The line is a paragraph continuation text.

   p%ln = ln
   p%lf = doc%line_first(ln)
   p%ll = doc%line_last(ln)
   p%offset = 1_I4P
   p%column = 0_I4P
   ! 1. continuation of the open blocks
   matched = 1_I4P
   do d=2, p%depth
      call find_nonspace(p)
      o = p%open_owner(d)
      if (o > 0_I4P) then
         call slots(o)%ext%block_continue(doc, p%open_node(d), p%src(p%lf:p%ll), p%ln, p%offset, p%column, p%indent, &
                                          match, consumed, ierr, msg)
         if (ierr /= BARUCH_OK) then
            msg = hook_message(slots, o, 'block_continue', ln, msg)
            return
         endif
         if (consumed < 0_I4P .or. consumed > remaining(p)) then
            call contract(slots, o, 'block_continue', ln, 'consumed '//istr(consumed)//' bytes of the '// &
                          istr(remaining(p))//' left in the line', ierr, msg)
            return
         endif
         select case(match)
         case(BLOCK_UNMATCHED)
            exit
         case(BLOCK_MATCHED)
            call advance(p, consumed)
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
         call continue_core(p, d, match)
         if (match == BLOCK_UNMATCHED) exit
      endif
      matched = d
      p%open_last(d) = p%ll
   enddo
   all_matched = matched == p%depth
   ! 2. block starts, at the deepest matched block
   leaf = p%open_content(matched) == CONTENT_LINES
   do while (.not. leaf)
      call find_nonspace(p)
      call try_starts(p, doc, slots, matched, all_matched, claimed, leaf, ierr, msg)
      if (ierr /= BARUCH_OK) return
      if (.not. claimed) exit
      matched = p%depth
   enddo
   ! 3. the rest of the line
   call find_nonspace(p)
   lazy = (.not. all_matched) .and. (.not. p%blank) .and. p%open_content(p%depth) == CONTENT_PARAGRAPH
   if (lazy) then ! a paragraph continuation text (CommonMark 5.1): no block opened, some not matched
      call add_line(p)
      p%open_last(p%depth) = p%ll
      return
   endif
   if (.not. all_matched) call close_blocks(p, doc, slots, matched + 1_I4P, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (p%open_content(p%depth) == CONTENT_LINES .or. p%open_content(p%depth) == CONTENT_PARAGRAPH) then
      call add_line(p)
      p%open_last(p%depth) = p%ll
   elseif (.not. p%blank) then ! 1.5: the core leaf starts come before this
      call open_paragraph(p, doc, slots, ierr, msg)
      if (ierr /= BARUCH_OK) return
      call add_line(p)
   endif
   endsubroutine incorporate_line

   subroutine try_starts(p, doc, slots, matched, all_matched, claimed, leaf, ierr, msg)
   !< Offer the position to the block starts. On a claim: close the unmatched blocks (`all_matched` becomes true),
   !< take the paragraph lines asked for, link the new block; `leaf` true if the new block is a leaf.
   type(block_parser),        intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   type(extension_slot),      intent(in)    :: slots(:)    !< Extensions of the options.
   integer(I4P),              intent(in)    :: matched     !< Depth of the deepest matched block.
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
   host = matched
   paragraph = 0_I4P
   lines = 0_I4P
   if (p%open_content(matched) == CONTENT_PARAGRAPH) then
      host = matched - 1_I4P
      paragraph = p%open_node(matched)
      lines = p%tip_lines
   endif
   node = 0_I4P
   o = 0_I4P
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
   ! 1.5: the core block starts, when no extension claimed the position
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
      elseif (consumed < 0_I4P .or. consumed > remaining(p)) then
         why = 'consumed '//istr(consumed)//' bytes of the '//istr(remaining(p))//' left in the line'
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
   call advance(p, consumed)
   if (.not. all_matched) call close_blocks(p, doc, slots, matched + 1_I4P, ierr, msg)
   if (ierr /= BARUCH_OK) return
   all_matched = .true.
   if (paragraph /= 0_I4P) then ! the paragraph keeps the lines not taken, and closes (it holds no block)
      p%tip_lines = p%tip_lines - taken
      if (p%tip_lines == 0_I4P) then
         call doc%unlink(paragraph)
         p%open_last(matched - 1_I4P) = max(p%open_last(matched - 1_I4P), p%open_last(matched))
         p%depth = p%depth - 1_I4P
      else
         p%open_last(matched) = p%tip_last(p%tip_lines)
         call close_blocks(p, doc, slots, matched, ierr, msg)
         if (ierr /= BARUCH_OK) return
      endif
   endif
   call add_block(p, doc, slots, node, o, content, first, ierr, msg)
   if (ierr /= BARUCH_OK) return
   claimed = .true.
   leaf = content /= CONTENT_BLOCKS
   endsubroutine try_starts

   subroutine continue_core(p, d, match)
   !< Say whether the line continues an open core block. The frame knows the paragraph only (a blank line ends it);
   !< 1.5: block quotes (`>`), list items (indentation), code blocks, HTML blocks.
   type(block_parser), intent(in)  :: p     !< Parser.
   integer(I4P),       intent(in)  :: d     !< Depth of the block.
   integer(I4P),       intent(out) :: match !< `BLOCK_UNMATCHED`, `BLOCK_MATCHED`.

   match = BLOCK_UNMATCHED
   if (p%open_content(d) == CONTENT_PARAGRAPH .and. .not. p%blank) match = BLOCK_MATCHED
   endsubroutine continue_core

   subroutine open_paragraph(p, doc, slots, ierr, msg)
   !< Open a paragraph at the position, its lines starting empty.
   type(block_parser),        intent(inout) :: p        !< Parser.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: node     !< Paragraph.

   node = doc%new_node(NODE_PARAGRAPH, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   call add_block(p, doc, slots, node, 0_I4P, CONTENT_PARAGRAPH, p%lf + p%nonspace - 1_I4P, ierr, msg)
   endsubroutine open_paragraph

   subroutine add_block(p, doc, slots, node, owner, content, first, ierr, msg)
   !< Link a new block as the last child of the deepest open block that can hold it (the ones that cannot are closed)
   !< and push it; a block taking lines starts with none.
   type(block_parser),        intent(inout) :: p        !< Parser.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(in)    :: node     !< New block.
   integer(I4P),              intent(in)    :: owner    !< Its owner.
   integer(I4P),              intent(in)    :: content  !< Its content.
   integer(I4P),              intent(in)    :: first    !< First byte of its span.
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.

   do while (.not. can_hold(p, doc, slots, p%depth, doc%node_kind(node)))
      call close_blocks(p, doc, slots, p%depth, ierr, msg)
      if (ierr /= BARUCH_OK) return
   enddo
   call doc%append_child(p%open_node(p%depth), node, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   if (p%depth == size(p%open_node, kind=I4P)) then
      call grow(p%open_node)
      call grow(p%open_owner)
      call grow(p%open_content)
      call grow(p%open_first)
      call grow(p%open_last)
   endif
   p%depth = p%depth + 1_I4P
   p%open_node(p%depth) = node
   p%open_owner(p%depth) = owner
   p%open_content(p%depth) = content
   p%open_first(p%depth) = first
   p%open_last(p%depth) = p%ll
   if (content == CONTENT_LINES .or. content == CONTENT_PARAGRAPH) p%tip_lines = 0_I4P
   endsubroutine add_block

   function can_hold(p, doc, slots, d, kind) result(can)
   !< Return true if the open block at depth `d` can hold a block of `kind`: a core container holds any block (1.5: a
   !< list holds items only), an extension's container what its `can_contain` says, a leaf nothing.
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
      endselect
   endif
   endfunction can_hold

   subroutine close_blocks(p, doc, slots, d_from, ierr, msg)
   !< Close the open blocks from the tip up to depth `d_from` (deepest first): spans set, `block_close` (or the core's
   !< finishing) called. A closed block's last byte extends its parent's.
   type(block_parser),        intent(inout) :: p        !< Parser.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(in)    :: d_from   !< Shallowest depth to close (2 or more).
   integer(I4P),              intent(inout) :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message.
   integer(I4P)                             :: d        !< Depth.
   integer(I4P)                             :: n        !< Node.
   integer(I4P)                             :: o        !< Owner.
   integer(I4P)                             :: lines    !< Content lines handed to the block.

   do while (p%depth >= max(2_I4P, d_from))
      d = p%depth
      n = p%open_node(d)
      o = p%open_owner(d)
      call doc%set_span(n, p%open_first(d), max(p%open_last(d), p%open_first(d) - 1_I4P))
      lines = 0_I4P
      if (p%open_content(d) == CONTENT_LINES .or. p%open_content(d) == CONTENT_PARAGRAPH) lines = p%tip_lines
      if (o > 0_I4P) then
         call slots(o)%ext%block_close(doc, n, p%tip_first(1:lines), p%tip_last(1:lines), ierr, msg)
         if (ierr /= BARUCH_OK) then
            msg = hook_message(slots, o, 'block_close', p%ln, msg)
            return
         endif
      elseif (p%open_content(d) == CONTENT_PARAGRAPH) then
         call finish_paragraph(p, doc, n, lines, ierr, msg)
         if (ierr /= BARUCH_OK) return
      endif
      if (lines > 0_I4P) p%tip_lines = 0_I4P
      p%open_last(d - 1_I4P) = max(p%open_last(d - 1_I4P), p%open_last(d))
      p%depth = d - 1_I4P
   enddo
   endsubroutine close_blocks

   subroutine finish_paragraph(p, doc, n, lines, ierr, msg)
   !< Finish a paragraph: its lines, their leading white space and the final trailing one stripped, joined by LF into
   !< one text child. 1.5/2.x: the raw content kept for the inline parser, link reference definitions taken out.
   type(block_parser),        intent(in)    :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(in)    :: n     !< Paragraph.
   integer(I4P),              intent(in)    :: lines !< Its lines.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   character(:), allocatable                :: text  !< Content.
   integer(I4P)                             :: i     !< Counter.
   integer(I4P)                             :: b     !< First byte of a line's text.
   integer(I4P)                             :: t     !< Text node.

   text = ''
   do i=1, lines
      b = p%tip_first(i)
      do while (b <= p%tip_last(i))
         if (p%src(b:b) /= ' ' .and. p%src(b:b) /= TAB) exit
         b = b + 1_I4P
      enddo
      if (i > 1_I4P) text = text//LF
      text = text//p%src(b:p%tip_last(i))
   enddo
   i = len(text, kind=I4P)
   do while (i > 0_I4P)
      if (text(i:i) /= ' ' .and. text(i:i) /= TAB) exit
      i = i - 1_I4P
   enddo
   t = doc%new_node(NODE_TEXT, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_literal(t, text(1:i), stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_span(t, doc%span_first(n), doc%span_last(n), stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%append_child(n, t, stat=ierr, errmsg=msg)
   endsubroutine finish_paragraph

   subroutine add_line(p)
   !< Give the rest of the line, from the offset, to the tip.
   type(block_parser), intent(inout) :: p !< Parser.

   if (p%tip_lines == size(p%tip_first, kind=I4P)) then
      call grow(p%tip_first)
      call grow(p%tip_last)
   endif
   p%tip_lines = p%tip_lines + 1_I4P
   p%tip_first(p%tip_lines) = p%lf + p%offset - 1_I4P
   p%tip_last(p%tip_lines) = p%ll
   endsubroutine add_line

   subroutine find_nonspace(p)
   !< Find the first byte at or after the offset that is not a space or a tab, its indentation in columns, and
   !< whether the rest of the line is blank.
   type(block_parser), intent(inout) :: p      !< Parser.
   integer(I4P)                      :: i      !< Byte of the line.
   integer(I4P)                      :: col    !< Its column.

   i = p%offset
   col = p%column
   do while (p%lf + i - 1_I4P <= p%ll)
      select case(p%src(p%lf + i - 1_I4P:p%lf + i - 1_I4P))
      case(' ')
         col = col + 1_I4P
      case(TAB)
         col = col + 4_I4P - mod(col, 4_I4P)
      case default
         exit
      endselect
      i = i + 1_I4P
   enddo
   p%nonspace = i
   p%indent = col - p%column
   p%blank = p%lf + i - 1_I4P > p%ll
   endsubroutine find_nonspace

   subroutine advance(p, bytes)
   !< Consume bytes of the line: the offset moves, the column follows (a tab to the next stop of 4, a UTF-8
   !< continuation byte none, any other byte one). 1.5: a tab consumed by columns, partly.
   type(block_parser), intent(inout) :: p     !< Parser.
   integer(I4P),       intent(in)    :: bytes !< Bytes to consume.
   integer(I4P)                      :: k     !< Counter.
   integer                           :: c     !< Byte value.

   do k=1, bytes
      c = iachar(p%src(p%lf + p%offset - 1_I4P:p%lf + p%offset - 1_I4P))
      if (c == 9) then
         p%column = p%column + 4_I4P - mod(p%column, 4_I4P)
      elseif (c < 128 .or. c >= 192) then
         p%column = p%column + 1_I4P
      endif
      p%offset = p%offset + 1_I4P
   enddo
   endsubroutine advance

   pure function remaining(p) result(bytes)
   !< Return the bytes of the line not consumed.
   type(block_parser), intent(in) :: p     !< Parser.
   integer(I4P)                   :: bytes !< Bytes.

   bytes = p%ll - (p%lf + p%offset - 1_I4P) + 1_I4P
   endfunction remaining

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

   pure subroutine grow(a)
   !< Double an integer array, keeping its values.
   integer(I4P), allocatable, intent(inout) :: a(:) !< Array.
   integer(I4P), allocatable                :: b(:) !< Grown copy.

   allocate(b(2_I4P * max(1_I4P, size(a, kind=I4P))))
   b(1:size(a)) = a
   call move_alloc(b, a)
   endsubroutine grow

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i !< Integer.
   character(:), allocatable :: s !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr
endmodule baruch_blocks
