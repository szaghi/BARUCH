!< BARUCH, the inline parser: the content of each paragraph and heading into inline nodes (CommonMark 0.31.2, 6).
module baruch_inlines
!< BARUCH, the inline parser: the content of each paragraph and heading into inline nodes (CommonMark 0.31.2, 6).
!<
!< The inline phase of CommonMark, after the block phase (`baruch_blocks`) has built every block and read every link
!< reference definition. It follows the spec's appendix "A parsing strategy" and cmark's `inlines.c` (BSD-2), whose
!< structure it keeps: no recursion, no module state, one `inline_parser` per parse holding every array.
!<
!< **The subject.** The block parser leaves, in each paragraph and heading, one text node: its content, the lines
!< without their leading white space joined by LF. The inline parser takes that string as its *subject* and a cursor
!< `pos` over it; it reads it once, left to right, with no copy per character: plain text is found with `scan` for the
!< next special byte, and every node is a slice or the few bytes a construct stands for. The text node goes, its block
!< receives the inline nodes in its place. Spans are kept: a **line map** gives, for each line of the subject, its first
!< offset in the subject and the byte of the document's source it came from, so every offset of the subject maps back
!< to the source (`source_byte`), the container markers between the lines skipped.
!<
!< **One step** (`parse_inline`) looks at the byte at the cursor:
!<
!< - LF: a soft line break, or a hard one when two or more spaces end the line (they are not text); the next line's
!<   leading spaces are skipped;
!< - `\`: before ASCII punctuation, that character as text; before LF, a hard line break; else a backslash;
!< - `` ` ``: a code span when a backtick string of the same length closes it (else the opening string is text); its
!<   content has line endings as spaces and one space stripped at both ends when it has one at both ends and is not
!<   only spaces. Where each closing length was last seen is remembered, so a subject of unmatched backticks is
!<   read in linear time (cmark's `backticks` array);
!< - `&`: a character reference (`baruch_inlines_scan`), its UTF-8 bytes as text; else `&`;
!< - `<`: a URI autolink, an email autolink (`NODE_LINK`, `LINK_AUTOLINK`, its text child), raw inline HTML (open and
!<   closing tags, comments, processing instructions, declarations, CDATA: `NODE_HTML_INLINE`, verbatim); else `<`;
!< - `*`, `_`: a delimiter run; when it can open or close emphasis (the flanking rules below), a text node of its own
!<   pushed on the **delimiter stack** (doubly linked through indices, so that a run can leave from its middle), else
!<   text;
!< - `[`, `![`: a text node of its own pushed on the **bracket stack** (image flag, `active`, `bracket_after`, the
!<   first delimiter pushed after it); `]` closes the top bracket: a link or an image when an inline destination or a
!<   reference follows (`handle_close_bracket`), else text;
!< - anything else: text up to the next special byte (trailing spaces and tabs before a LF dropped).
!<
!< Text is accumulated, not written byte by byte: consecutive pieces (plain text, an escaped character, a reference's
!< bytes) become **one** text node when something else comes. After the subject, `process_emphasis` runs over the
!< delimiter stack, the bracket stack is emptied, and the adjacent text nodes left by the delimiters and brackets that
!< matched nothing are **merged** (one pass over the block's subtree, each run concatenated once).
!<
!< **Emphasis** (spec 6.2 and appendix): a run is left-flanking when the character after it is not Unicode White_Space
!< and either not Unicode punctuation (General_Category P or S) or preceded by white space or punctuation;
!< right-flanking symmetrically; the start and the end of the subject count as white space. A `*` run opens when
!< left-flanking, closes when right-flanking; a `_` run opens when left-flanking and not right-flanking or preceded by
!< punctuation, closes symmetrically. `process_emphasis` (cmark's) moves forward over the closers; for each it looks
!< back for an opener of the same character, refusing an opener when one of them can both open and close and the sum
!< of their original lengths is a multiple of 3 while the closer's is not (the rule of 3). A match takes two
!< characters from both when both have two (strong), else one (emphasis): the nodes between become the children of a
!< new `NODE_EMPH` or `NODE_STRONG` (`SLOT_EMPHASIS_CHAR` the character), the delimiters between are dropped. A closer
!< with no opener sets the lower bound of later searches (`openers_bottom`, one per character, closer can-open and
!< original length modulo 3): the matching is linear. A run keeps its text node; its literal and span are set once,
!< when it leaves the stack, to the characters no match used.
!<
!< **Links and images** (spec 6.3, 6.4): at `]` the top bracket, if active, is tried as the text of an inline link
!< (`(`, white space, a destination `<...>` or raw with balanced parentheses, white space, an optional title, white
!< space, `)`), then of a full reference (`[label]` right after it), a collapsed one (`[]`) or a shortcut one (its
!< text as the label, when no bracket was pushed after it), the label looked up in the `reference_map` of the
!< document's definitions (`baruch_references`). On a match the nodes after the bracket become the children of a
!< `NODE_LINK` or `NODE_IMAGE` (destination, title, `SLOT_LINK_FORM`, the label as written), the delimiters inside are
!< processed, and a link deactivates every `[` before it (links do not nest; the walk stops at the first inactive one,
!< as cmark's). The look-ahead for a `[label]` is bounded by 999 characters.
use penf,                only : I4P
use baruch_document,     only : markdown_document
use stringifor,          only : string
use baruch_kinds_nodes,  only : NODE_PARAGRAPH, NODE_HEADING, NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, &
                                NODE_HTML_INLINE, NODE_EMPH, NODE_STRONG, NODE_LINK, NODE_IMAGE, SLOT_EMPHASIS_CHAR,  &
                                SLOT_LINK_FORM, LINK_INLINE, LINK_FULL, LINK_COLLAPSED, LINK_SHORTCUT, LINK_AUTOLINK
use baruch_blocks_scan,  only : BACKSLASH, is_ascii_punctuation, is_ascii_letter
use baruch_blocks_definitions, only : scan_destination, scan_title, LABEL_MAX
use baruch_inlines_scan, only : scan_reference, decode_references, unescape_string, scan_autolink_uri,            &
                                scan_autolink_email, scan_html_tag, scan_html_comment, scan_processing_instruction, &
                                scan_declaration, scan_cdata
use baruch_references,   only : reference_map
use baruch_status
implicit none
private
public :: parse_inlines

character,    parameter :: LF  = achar(10) !< Line feed.
character,    parameter :: TAB = achar(9)  !< Tab.
character(*), parameter :: SPECIALS = LF//'`'//BACKSLASH//'&<*_[]!' !< Bytes a text run stops at.
integer(I4P), parameter :: BACKTICKS_MAX = 80_I4P !< Longest backtick string remembered (cmark's `MAXBACKTICKS`).
integer(I4P), parameter :: STACK_DEFAULT = 16_I4P !< Delimiters, brackets and lines allocated up front (all grow).

type :: inline_parser
   !< The state of the inline phase of one parse: the subject of the current block and the stacks, reused block after
   !< block.
   character(:), allocatable :: src                    !< The document's source, copied once per parse.
   integer(I4P)              :: block = 0_I4P          !< The paragraph or heading being parsed.
   character(:), allocatable :: subj                   !< Its content, the subject.
   integer(I4P)              :: n = 0_I4P              !< Bytes of the subject.
   integer(I4P)              :: pos = 1_I4P            !< The cursor: first byte not read.
   ! the line map
   integer(I4P)              :: lines = 0_I4P          !< Lines of the subject.
   integer(I4P), allocatable :: line_subj(:)           !< First offset of each line in the subject.
   integer(I4P), allocatable :: line_src(:)            !< Its byte in the source.
   ! the text accumulated
   character(:), allocatable :: text                   !< Bytes of the text not yet written as a node.
   integer(I4P)              :: text_used = 0_I4P      !< Bytes of `text` in use.
   integer(I4P)              :: text_first = 0_I4P     !< First offset of the subject they come from.
   integer(I4P)              :: text_last = 0_I4P      !< Last offset.
   ! the delimiter stack: a list linked through indices, `delim_top` the last pushed
   integer(I4P)              :: delims_used = 0_I4P    !< Records used (removed ones included).
   integer(I4P)              :: delim_top = 0_I4P      !< Top of the stack, 0 when empty.
   integer(I4P), allocatable :: delim_node(:)          !< The text node of the run.
   integer(I4P), allocatable :: delim_char(:)          !< Its character, `iachar` of `*` or `_`.
   integer(I4P), allocatable :: delim_count(:)         !< Delimiters left in the run.
   integer(I4P), allocatable :: delim_original(:)      !< Delimiters of the run as read (rule of 3).
   integer(I4P), allocatable :: delim_first(:)         !< Offset of its first delimiter left (a closer uses its first).
   integer(I4P), allocatable :: delim_prev(:)          !< Delimiter below, 0 if none.
   integer(I4P), allocatable :: delim_next(:)          !< Delimiter above, 0 if none.
   logical,      allocatable :: delim_open(:)          !< The run can open emphasis.
   logical,      allocatable :: delim_close(:)         !< The run can close emphasis.
   ! the bracket stack
   integer(I4P)              :: brackets = 0_I4P       !< Brackets on the stack.
   integer(I4P), allocatable :: bracket_node(:)        !< The text node of `[` or `![`.
   integer(I4P), allocatable :: bracket_pos(:)         !< Offset of the subject after it.
   integer(I4P), allocatable :: bracket_delim(:)       !< The first delimiter pushed after it (record number).
   logical,      allocatable :: bracket_image(:)       !< `![`.
   logical,      allocatable :: bracket_active(:)      !< Can still open a link.
   logical,      allocatable :: bracket_after(:)       !< Another bracket was pushed after it.
   ! linear-time memory
   logical                   :: backticks_scanned = .false. !< The subject was scanned to its end for a closer.
   integer(I4P)              :: backticks(BACKTICKS_MAX) = 0_I4P !< Last offset of a backtick string of each length.
   logical                   :: no_comment_end = .false.     !< No `-->` after some `<!--`: none after any later.
   logical                   :: no_pi_end = .false.          !< No `?>` after some `<?`.
   logical                   :: no_declaration_end = .false. !< No `>` after some `<!X`.
   logical                   :: no_cdata_end = .false.       !< No `]]>` after some `<![CDATA[`.
   ! the link reference definitions
   type(reference_map)       :: refs                         !< The document's definitions, by normalised label.
endtype inline_parser

contains
   subroutine parse_inlines(doc, stat, errmsg)
   !< Parse the inline content of every paragraph and heading of a document whose blocks are parsed (as
   !< `markdown_options%parse` leaves them): each block's text node is replaced by its inline nodes. The tree is
   !< compacted at the end, so it holds no tombstone.
   type(markdown_document),   intent(inout)         :: doc    !< Document.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   type(inline_parser)                              :: p      !< Parser state.
   integer(I4P)                                     :: last   !< Last node of the blocks.
   integer(I4P)                                     :: n      !< Node.
   integer(I4P)                                     :: t      !< Its text child.
   integer(I4P)                                     :: ierr   !< Status.
   character(:), allocatable                        :: msg    !< Message.
   logical                                          :: parsed !< A block was parsed.

   call status_ok(stat, errmsg)
   ierr = BARUCH_OK
   msg = ''
   parsed = .false.
   call start(p, doc)
   call p%refs%build(doc)
   last = doc%last_node()
   do n=1, last ! the blocks were all created before: the inline nodes come after `last`
      if (.not. doc%is_node(n)) cycle
      if (doc%node_kind(n) /= NODE_PARAGRAPH .and. doc%node_kind(n) /= NODE_HEADING) cycle
      t = doc%first_child(n)
      if (t == 0_I4P) cycle
      if (doc%node_kind(t) /= NODE_TEXT .or. doc%next_sibling(t) /= 0_I4P) cycle
      call parse_block(p, doc, n, t, ierr, msg)
      if (ierr /= BARUCH_OK) exit
      parsed = .true.
   enddo
   if (ierr == BARUCH_OK .and. parsed) call doc%compact(stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) call status_error(ierr, msg, stat, errmsg)
   endsubroutine parse_inlines

   ! private
   subroutine start(p, doc)
   !< Initialise the parser: the source copied, the stacks allocated.
   type(inline_parser),     intent(inout) :: p   !< Parser.
   type(markdown_document), intent(in)    :: doc !< Document.

   p%src = doc%source()
   allocate(p%line_subj(STACK_DEFAULT), p%line_src(STACK_DEFAULT))
   allocate(p%delim_node(STACK_DEFAULT), p%delim_char(STACK_DEFAULT), p%delim_count(STACK_DEFAULT),         &
            p%delim_original(STACK_DEFAULT), p%delim_first(STACK_DEFAULT), p%delim_prev(STACK_DEFAULT),     &
            p%delim_next(STACK_DEFAULT), p%delim_open(STACK_DEFAULT), p%delim_close(STACK_DEFAULT))
   allocate(p%bracket_node(STACK_DEFAULT), p%bracket_pos(STACK_DEFAULT), p%bracket_delim(STACK_DEFAULT),   &
            p%bracket_image(STACK_DEFAULT), p%bracket_active(STACK_DEFAULT), p%bracket_after(STACK_DEFAULT))
   allocate(character(256) :: p%text)
   endsubroutine start

   subroutine parse_block(p, doc, block, t, ierr, msg)
   !< Parse the content of one block, held by its text node `t`: the text node goes, the inline nodes come.
   type(inline_parser),       intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(in)    :: block !< Paragraph or heading.
   integer(I4P),              intent(in)    :: t     !< Its text node.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.

   p%block = block
   p%subj = doc%literal(t)
   p%n = len(p%subj, kind=I4P)
   p%pos = 1_I4P
   call map_lines(p, doc, doc%span_first(t))
   call doc%unlink(t, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   p%text_used = 0_I4P
   p%delims_used = 0_I4P
   p%delim_top = 0_I4P
   p%brackets = 0_I4P
   p%backticks_scanned = .false.
   p%backticks = 0_I4P
   p%no_comment_end = .false.
   p%no_pi_end = .false.
   p%no_declaration_end = .false.
   p%no_cdata_end = .false.
   do while (p%pos <= p%n)
      call parse_inline(p, doc, ierr, msg)
      if (ierr /= BARUCH_OK) return
   enddo
   call flush_text(p, doc, ierr, msg)
   if (ierr /= BARUCH_OK) return
   call process_emphasis(p, doc, 1_I4P, ierr, msg)
   if (ierr /= BARUCH_OK) return
   p%brackets = 0_I4P
   call merge_texts(doc, block, ierr, msg)
   endsubroutine parse_block

   subroutine map_lines(p, doc, first)
   !< Build the line map of the subject: line 1 starts at the text node's first byte; every other line ends where its
   !< source line ends (the last one before its trailing white space, which the block parser removed), consecutive
   !< lines of the source.
   type(inline_parser),     intent(inout) :: p     !< Parser.
   type(markdown_document), intent(in)    :: doc   !< Document.
   integer(I4P),            intent(in)    :: first !< First byte of the text node.
   integer(I4P)                           :: ln    !< Source line of the first one.
   integer(I4P)                           :: lo    !< Binary search, low.
   integer(I4P)                           :: hi    !< Binary search, high.
   integer(I4P)                           :: mid   !< Binary search, middle.
   integer(I4P)                           :: a     !< First offset of a line of the subject.
   integer(I4P)                           :: e     !< Last byte of its source line.
   integer(I4P)                           :: k     !< Position.
   integer(I4P)                           :: nl    !< Lines of the source.

   ! the source line holding `first`: the last whose first byte is at most `first`
   nl = doc%lines_number()
   lo = 1_I4P
   hi = nl
   ln = 1_I4P
   do while (lo <= hi)
      mid = (lo + hi) / 2_I4P
      if (doc%line_first(mid) <= first) then
         ln = mid
         lo = mid + 1_I4P
      else
         hi = mid - 1_I4P
      endif
   enddo
   p%lines = 0_I4P
   a = 1_I4P
   do
      if (p%lines == size(p%line_subj, kind=I4P)) then
         call grow(p%line_subj)
         call grow(p%line_src)
      endif
      p%lines = p%lines + 1_I4P
      p%line_subj(p%lines) = a
      k = index(p%subj(a:), LF, kind=I4P) ! the end of this line of the subject
      if (p%lines == 1_I4P) then
         p%line_src(1) = first
      elseif (ln > nl) then ! not a parsed text node: keep the offsets where they are
         p%line_src(p%lines) = p%line_src(p%lines - 1_I4P) + a - p%line_subj(p%lines - 1_I4P)
      else
         e = doc%line_last(ln)
         if (k == 0_I4P) then ! the last line: its trailing white space is not in the subject
            do while (e >= doc%line_first(ln))
               if (p%src(e:e) /= ' ' .and. p%src(e:e) /= TAB) exit
               e = e - 1_I4P
            enddo
            p%line_src(p%lines) = e - (p%n - a)
         else
            p%line_src(p%lines) = e - (k - 2_I4P)
         endif
      endif
      if (k == 0_I4P) exit
      a = a + k
      ln = ln + 1_I4P
   enddo
   endsubroutine map_lines

   pure function source_byte(p, o) result(b)
   !< Return the byte of the source an offset of the subject comes from (a LF: the first byte of its line ending).
   type(inline_parser), intent(in) :: p  !< Parser.
   integer(I4P),        intent(in) :: o  !< Offset in the subject.
   integer(I4P)                    :: b  !< Byte in the source.
   integer(I4P)                    :: lo !< Binary search, low.
   integer(I4P)                    :: hi !< Binary search, high.
   integer(I4P)                    :: k  !< Line.
   integer(I4P)                    :: mid !< Binary search, middle.

   lo = 1_I4P
   hi = p%lines
   k = 1_I4P
   do while (lo <= hi)
      mid = (lo + hi) / 2_I4P
      if (p%line_subj(mid) <= o) then
         k = mid
         lo = mid + 1_I4P
      else
         hi = mid - 1_I4P
      endif
   enddo
   b = p%line_src(k) + o - p%line_subj(k)
   endfunction source_byte

   subroutine parse_inline(p, doc, ierr, msg)
   !< One step: the construct at the cursor, read and written (or accumulated as text); the cursor moves past it.
   type(inline_parser),       intent(inout) :: p    !< Parser.
   type(markdown_document),   intent(inout) :: doc  !< Document.
   integer(I4P),              intent(inout) :: ierr !< Status.
   character(:), allocatable, intent(inout) :: msg  !< Message.
   integer(I4P)                             :: e    !< First special byte after the cursor (offset), past the end.
   integer(I4P)                             :: t    !< Last byte of the text kept.

   select case(p%subj(p%pos:p%pos))
   case(LF)
      call handle_newline(p, doc, ierr, msg)
   case(BACKSLASH)
      call handle_backslash(p, doc, ierr, msg)
   case('`')
      call handle_backticks(p, doc, ierr, msg)
   case('&')
      call handle_entity(p)
   case('<')
      call handle_pointy_brace(p, doc, ierr, msg)
   case('*', '_')
      call handle_delim(p, doc, ierr, msg)
   case('[')
      call push_bracket(p, doc, .false., ierr, msg)
   case('!')
      if (p%pos < p%n) then
         if (p%subj(p%pos+1:p%pos+1) == '[') then
            call push_bracket(p, doc, .true., ierr, msg)
            return
         endif
      endif
      call add_text(p, '!', p%pos, p%pos)
      p%pos = p%pos + 1_I4P
   case(']')
      call handle_close_bracket(p, doc, ierr, msg)
   case default
      e = scan(p%subj(p%pos+1:), SPECIALS, kind=I4P)
      if (e == 0_I4P) then
         e = p%n + 1_I4P
      else
         e = p%pos + e
      endif
      t = e - 1_I4P
      if (e <= p%n) then
         if (p%subj(e:e) == LF) then ! trailing white space before a line ending is not text
            do while (t >= p%pos)
               if (p%subj(t:t) /= ' ' .and. p%subj(t:t) /= TAB) exit
               t = t - 1_I4P
            enddo
         endif
      endif
      if (t >= p%pos) call add_text(p, p%subj(p%pos:t), p%pos, t)
      p%pos = e
   endselect
   endsubroutine parse_inline

   subroutine handle_newline(p, doc, ierr, msg)
   !< A line ending: a hard line break after two or more spaces (from the first of them), else a soft one; the next
   !< line's leading spaces are skipped.
   type(inline_parser),       intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   integer(I4P)                             :: first !< First byte of the break.
   integer(I4P)                             :: node  !< The break.

   first = p%pos
   do while (first > 1_I4P)
      if (p%subj(first-1:first-1) /= ' ') exit
      first = first - 1_I4P
   enddo
   call flush_text(p, doc, ierr, msg)
   if (ierr /= BARUCH_OK) return
   if (p%pos - first >= 2_I4P) then
      node = add_node(p, doc, NODE_LINEBREAK, first, p%pos, ierr, msg)
   else
      node = add_node(p, doc, NODE_SOFTBREAK, p%pos, p%pos, ierr, msg)
   endif
   p%pos = p%pos + 1_I4P
   call skip_spaces(p)
   endsubroutine handle_newline

   subroutine handle_backslash(p, doc, ierr, msg)
   !< A backslash: before ASCII punctuation, that character as text; before a line ending, a hard line break (the
   !< next line's leading spaces skipped); else a backslash as text.
   type(inline_parser),       intent(inout) :: p    !< Parser.
   type(markdown_document),   intent(inout) :: doc  !< Document.
   integer(I4P),              intent(inout) :: ierr !< Status.
   character(:), allocatable, intent(inout) :: msg  !< Message.
   integer(I4P)                             :: node !< The break.

   if (p%pos < p%n) then
      if (is_ascii_punctuation(p%subj(p%pos+1:p%pos+1))) then
         call add_text(p, p%subj(p%pos+1:p%pos+1), p%pos, p%pos + 1_I4P)
         p%pos = p%pos + 2_I4P
         return
      elseif (p%subj(p%pos+1:p%pos+1) == LF) then
         call flush_text(p, doc, ierr, msg)
         if (ierr /= BARUCH_OK) return
         node = add_node(p, doc, NODE_LINEBREAK, p%pos, p%pos + 1_I4P, ierr, msg)
         p%pos = p%pos + 2_I4P
         call skip_spaces(p)
         return
      endif
   endif
   call add_text(p, BACKSLASH, p%pos, p%pos)
   p%pos = p%pos + 1_I4P
   endsubroutine handle_backslash

   subroutine handle_backticks(p, doc, ierr, msg)
   !< A backtick string: a code span up to the next backtick string of the same length, or text if there is none.
   type(inline_parser),       intent(inout) :: p       !< Parser.
   type(markdown_document),   intent(inout) :: doc     !< Document.
   integer(I4P),              intent(inout) :: ierr    !< Status.
   character(:), allocatable, intent(inout) :: msg     !< Message.
   integer(I4P)                             :: first   !< First byte of the opener.
   integer(I4P)                             :: length  !< Backticks of the opener.
   integer(I4P)                             :: closer  !< First byte of the closer, 0 if none.
   integer(I4P)                             :: node    !< Code span.

   first = p%pos
   do while (p%pos <= p%n)
      if (p%subj(p%pos:p%pos) /= '`') exit
      p%pos = p%pos + 1_I4P
   enddo
   length = p%pos - first
   closer = closing_backticks(p, length)
   if (closer == 0_I4P) then ! the opener is text; the cursor stays after it
      call add_text(p, p%subj(first:p%pos-1), first, p%pos - 1_I4P)
      return
   endif
   call flush_text(p, doc, ierr, msg)
   if (ierr /= BARUCH_OK) return
   node = add_node(p, doc, NODE_CODE, first, closer + length - 1_I4P, ierr, msg)
   if (ierr == BARUCH_OK) call doc%set_literal(node, code_content(p%subj(p%pos:closer-1)), stat=ierr, errmsg=msg)
   p%pos = closer + length
   endsubroutine handle_backticks

   function closing_backticks(p, length) result(closer)
   !< Return the first byte of the next backtick string of `length` backticks from the cursor, 0 if none. Every string
   !< met on the way is remembered by its length; once the subject was scanned to its end, a length whose last string
   !< is before the cursor has none after it (cmark's `scan_to_closing_backticks`).
   type(inline_parser), intent(inout) :: p      !< Parser.
   integer(I4P),        intent(in)    :: length !< Backticks of the opener.
   integer(I4P)                       :: closer !< First byte of the closer, 0 if none.
   integer(I4P)                       :: k      !< Position.
   integer(I4P)                       :: a      !< First byte of a backtick string.
   integer(I4P)                       :: e      !< Offset of the next backtick.

   closer = 0_I4P
   if (length > BACKTICKS_MAX) return
   if (p%backticks_scanned .and. p%backticks(length) < p%pos) return
   k = p%pos
   do while (k <= p%n)
      e = index(p%subj(k:), '`', kind=I4P)
      if (e == 0_I4P) exit
      a = k + e - 1_I4P
      k = a
      do while (k <= p%n)
         if (p%subj(k:k) /= '`') exit
         k = k + 1_I4P
      enddo
      if (k - a <= BACKTICKS_MAX) p%backticks(k - a) = a
      if (k - a == length) then
         closer = a
         return
      endif
   enddo
   p%backticks_scanned = .true.
   endfunction closing_backticks

   pure function code_content(raw) result(content)
   !< Return a code span's content: line endings as spaces, then one space removed at both ends when there is one at
   !< both ends and the content is not only spaces.
   character(*), intent(in)  :: raw     !< Bytes between the backtick strings.
   character(:), allocatable :: content !< Content.
   integer(I4P)              :: k       !< Position.
   integer(I4P)              :: n       !< Length.

   content = raw
   n = len(content, kind=I4P)
   do k=1, n
      if (content(k:k) == LF) content(k:k) = ' '
   enddo
   if (n >= 2_I4P) then
      if (content(1:1) == ' ' .and. content(n:n) == ' ' .and. verify(content, ' ') > 0) content = content(2:n-1)
   endif
   endfunction code_content

   subroutine handle_entity(p)
   !< An `&`: a character reference as its UTF-8 bytes, or `&`.
   type(inline_parser), intent(inout) :: p      !< Parser.
   character(:), allocatable          :: bytes  !< The reference's bytes.
   integer(I4P)                       :: length !< Bytes of the reference.

   call scan_reference(p%subj, p%pos, bytes, length)
   if (length > 0_I4P) then
      call add_text(p, bytes, p%pos, p%pos + length - 1_I4P)
      p%pos = p%pos + length
   else
      call add_text(p, '&', p%pos, p%pos)
      p%pos = p%pos + 1_I4P
   endif
   endsubroutine handle_entity

   subroutine handle_pointy_brace(p, doc, ierr, msg)
   !< A `<`: a URI autolink, an email autolink, raw inline HTML, or `<`.
   type(inline_parser),       intent(inout) :: p      !< Parser.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: ierr   !< Status.
   character(:), allocatable, intent(inout) :: msg    !< Message.
   integer(I4P)                             :: length !< Bytes of the construct.
   integer(I4P)                             :: node   !< Its node.

   length = scan_autolink_uri(p%subj(p%pos:))
   if (length > 0_I4P) then
      call add_autolink(p, doc, length, .false., ierr, msg)
      return
   endif
   length = scan_autolink_email(p%subj(p%pos:))
   if (length > 0_I4P) then
      call add_autolink(p, doc, length, .true., ierr, msg)
      return
   endif
   length = raw_html_length(p)
   if (length > 0_I4P) then
      call flush_text(p, doc, ierr, msg)
      if (ierr /= BARUCH_OK) return
      node = add_node(p, doc, NODE_HTML_INLINE, p%pos, p%pos + length - 1_I4P, ierr, msg)
      if (ierr == BARUCH_OK) call doc%set_literal(node, p%subj(p%pos:p%pos+length-1), stat=ierr, errmsg=msg)
      p%pos = p%pos + length
      return
   endif
   call add_text(p, '<', p%pos, p%pos)
   p%pos = p%pos + 1_I4P
   endsubroutine handle_pointy_brace

   subroutine add_autolink(p, doc, length, email, ierr, msg)
   !< An autolink of `length` bytes at the cursor: a link (`LINK_AUTOLINK`) whose destination is its content (with
   !< `mailto:` before an email address) and whose text child is its content, character references decoded in both
   !< (backslash escapes are not: spec 6.5).
   type(inline_parser),       intent(inout) :: p       !< Parser.
   type(markdown_document),   intent(inout) :: doc     !< Document.
   integer(I4P),              intent(in)    :: length  !< Bytes of the autolink, `<` and `>` included.
   logical,                   intent(in)    :: email   !< An email address.
   integer(I4P),              intent(inout) :: ierr    !< Status.
   character(:), allocatable, intent(inout) :: msg     !< Message.
   character(:), allocatable                :: content !< Its content, references decoded.
   integer(I4P)                             :: link    !< Link.
   integer(I4P)                             :: t       !< Its text.

   call flush_text(p, doc, ierr, msg)
   if (ierr /= BARUCH_OK) return
   content = decode_references(p%subj(p%pos+1:p%pos+length-2))
   link = add_node(p, doc, NODE_LINK, p%pos, p%pos + length - 1_I4P, ierr, msg)
   if (ierr == BARUCH_OK) call doc%set_int_slot(link, SLOT_LINK_FORM, LINK_AUTOLINK, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) then
      if (email) then
         call doc%set_destination(link, 'mailto:'//content, stat=ierr, errmsg=msg)
      else
         call doc%set_destination(link, content, stat=ierr, errmsg=msg)
      endif
   endif
   if (ierr == BARUCH_OK) t = doc%new_node(NODE_TEXT, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_literal(t, content, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_span(t, source_byte(p, p%pos + 1_I4P), source_byte(p, p%pos + length - 2_I4P), &
                                            stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%append_child(link, t, stat=ierr, errmsg=msg)
   p%pos = p%pos + length
   endsubroutine add_autolink

   function raw_html_length(p) result(length)
   !< Return the bytes of the raw inline HTML at the cursor (`<` there), 0 for none; a failed search for the end of a
   !< comment, a processing instruction, a declaration or a CDATA section is remembered for the rest of the subject.
   type(inline_parser), intent(inout) :: p      !< Parser.
   integer(I4P)                       :: length !< Bytes of the HTML.
   logical                            :: no_end !< The end string is nowhere after the cursor.
   integer(I4P)                       :: rest   !< Bytes from the cursor.

   length = 0_I4P
   rest = p%n - p%pos + 1_I4P
   if (rest < 3_I4P) return
   no_end = .false.
   if (p%subj(p%pos+1:p%pos+1) == '?') then
      if (.not. p%no_pi_end) call scan_processing_instruction(p%subj(p%pos:), length, no_end)
      if (no_end) p%no_pi_end = .true.
   elseif (p%subj(p%pos+1:p%pos+1) == '!') then
      if (rest >= 4_I4P .and. p%subj(p%pos:min(p%n, p%pos+3_I4P)) == '<!--') then
         if (.not. p%no_comment_end) call scan_html_comment(p%subj(p%pos:), length, no_end)
         if (no_end) p%no_comment_end = .true.
      elseif (rest >= 9_I4P .and. p%subj(p%pos:min(p%n, p%pos+8_I4P)) == '<![CDATA[') then
         if (.not. p%no_cdata_end) call scan_cdata(p%subj(p%pos:), length, no_end)
         if (no_end) p%no_cdata_end = .true.
      elseif (is_ascii_letter(p%subj(p%pos+2:p%pos+2))) then
         if (.not. p%no_declaration_end) call scan_declaration(p%subj(p%pos:), length, no_end)
         if (no_end) p%no_declaration_end = .true.
      endif
   else
      length = scan_html_tag(p%subj(p%pos:))
   endif
   endfunction raw_html_length

   subroutine handle_delim(p, doc, ierr, msg)
   !< A run of `*` or `_`: a text node of its own pushed on the delimiter stack when it can open or close emphasis
   !< (the flanking rules, with Unicode White_Space and punctuation around it), else text.
   type(inline_parser),       intent(inout) :: p            !< Parser.
   type(markdown_document),   intent(inout) :: doc          !< Document.
   integer(I4P),              intent(inout) :: ierr         !< Status.
   character(:), allocatable, intent(inout) :: msg          !< Message.
   character                                :: c            !< The delimiter character.
   integer(I4P)                             :: first        !< First byte of the run.
   integer(I4P)                             :: node         !< Its text node.
   logical                                  :: before_space !< White space (or the start) before the run.
   logical                                  :: before_punct !< Punctuation before the run.
   logical                                  :: after_space  !< White space (or the end) after the run.
   logical                                  :: after_punct  !< Punctuation after the run.
   logical                                  :: left         !< Left-flanking.
   logical                                  :: right        !< Right-flanking.
   logical                                  :: can_open     !< Can open emphasis.
   logical                                  :: can_close    !< Can close emphasis.

   c = p%subj(p%pos:p%pos)
   first = p%pos
   do while (p%pos <= p%n)
      if (p%subj(p%pos:p%pos) /= c) exit
      p%pos = p%pos + 1_I4P
   enddo
   call classify(p%subj, char_before(p%subj, first), before_space, before_punct)
   call classify(p%subj, char_at(p%subj, p%pos), after_space, after_punct)
   left = .not. after_space .and. (.not. after_punct .or. before_space .or. before_punct)
   right = .not. before_space .and. (.not. before_punct .or. after_space .or. after_punct)
   if (c == '_') then
      can_open = left .and. (.not. right .or. before_punct)
      can_close = right .and. (.not. left .or. after_punct)
   else
      can_open = left
      can_close = right
   endif
   if (.not. (can_open .or. can_close)) then
      call add_text(p, p%subj(first:p%pos-1), first, p%pos - 1_I4P)
      return
   endif
   node = add_text_node(p, doc, first, p%pos - 1_I4P, ierr, msg)
   if (ierr /= BARUCH_OK) return
   call push_delimiter(p, node, c, first, p%pos - first, can_open, can_close)
   endsubroutine handle_delim

   pure function char_before(text, i) result(first)
   !< Return the first byte of the UTF-8 character before position `i` (it ends at `i - 1`), 0 at the start.
   character(*), intent(in) :: text  !< Text, well-formed UTF-8.
   integer(I4P), intent(in) :: i     !< Position.
   integer(I4P)             :: first !< First byte of the character before, 0 if none.

   first = i - 1_I4P
   do while (first > 1_I4P)
      if (iand(iachar(text(first:first)), 192) /= 128) exit ! not a continuation byte
      first = first - 1_I4P
   enddo
   endfunction char_before

   pure function char_at(text, i) result(first)
   !< Return `i`, or 0 past the end of the text (no character there).
   character(*), intent(in) :: text  !< Text.
   integer(I4P), intent(in) :: i     !< Position.
   integer(I4P)             :: first !< Position of the character, 0 if none.

   first = i
   if (i > len(text, kind=I4P)) first = 0_I4P
   endfunction char_at

   subroutine classify(text, i, space, punct)
   !< Whether the UTF-8 character at `i` is Unicode White_Space and whether it is Unicode punctuation (General_Category
   !< P or S), by StringiFor's `is_space(unicode=.true.)` and `is_punct(unicode=.true.)`; no character (`i` 0, the
   !< start or the end of the subject) counts as white space.
   character(*), intent(in)  :: text  !< Text, well-formed UTF-8.
   integer(I4P), intent(in)  :: i     !< First byte of the character, 0 for none.
   logical,      intent(out) :: space !< White space.
   logical,      intent(out) :: punct !< Punctuation.
   type(string)              :: u     !< The character.
   integer(I4P)              :: b     !< Its lead byte.
   integer(I4P)              :: n     !< Its bytes.

   space = .true.
   punct = .false.
   if (i < 1_I4P) return
   b = iachar(text(i:i))
   if (b < 128_I4P) then
      space = text(i:i) == ' ' .or. (b >= 9_I4P .and. b <= 13_I4P)
      punct = is_ascii_punctuation(text(i:i))
      return
   elseif (b >= 240_I4P) then
      n = 4_I4P
   elseif (b >= 224_I4P) then
      n = 3_I4P
   else
      n = 2_I4P
   endif
   u = text(i:min(len(text, kind=I4P), i + n - 1_I4P))
   space = u%is_space(unicode=.true.)
   punct = u%is_punct(unicode=.true.)
   endsubroutine classify

   subroutine push_delimiter(p, node, c, first, count, can_open, can_close)
   !< Push a delimiter run on the delimiter stack.
   type(inline_parser), intent(inout) :: p         !< Parser.
   integer(I4P),        intent(in)    :: node      !< Its text node.
   character,           intent(in)    :: c         !< Its character.
   integer(I4P),        intent(in)    :: first     !< Its first offset in the subject.
   integer(I4P),        intent(in)    :: count     !< Its length.
   logical,             intent(in)    :: can_open  !< It can open emphasis.
   logical,             intent(in)    :: can_close !< It can close emphasis.
   integer(I4P)                       :: d         !< New record.

   if (p%delims_used == size(p%delim_node, kind=I4P)) then
      call grow(p%delim_node)
      call grow(p%delim_char)
      call grow(p%delim_count)
      call grow(p%delim_original)
      call grow(p%delim_first)
      call grow(p%delim_prev)
      call grow(p%delim_next)
      call grow_logical(p%delim_open)
      call grow_logical(p%delim_close)
   endif
   p%delims_used = p%delims_used + 1_I4P
   d = p%delims_used
   p%delim_node(d) = node
   p%delim_char(d) = iachar(c)
   p%delim_count(d) = count
   p%delim_original(d) = count
   p%delim_first(d) = first
   p%delim_open(d) = can_open
   p%delim_close(d) = can_close
   p%delim_prev(d) = p%delim_top
   p%delim_next(d) = 0_I4P
   if (p%delim_top /= 0_I4P) p%delim_next(p%delim_top) = d
   p%delim_top = d
   endsubroutine push_delimiter

   subroutine remove_delimiter(p, d)
   !< Remove a delimiter from the stack, wherever it is (its text node stays).
   type(inline_parser), intent(inout) :: p !< Parser.
   integer(I4P),        intent(in)    :: d !< Delimiter.

   if (p%delim_prev(d) /= 0_I4P) p%delim_next(p%delim_prev(d)) = p%delim_next(d)
   if (p%delim_next(d) /= 0_I4P) then
      p%delim_prev(p%delim_next(d)) = p%delim_prev(d)
   else
      p%delim_top = p%delim_prev(d)
   endif
   endsubroutine remove_delimiter

   subroutine retire_delimiter(p, doc, d, ierr, msg)
   !< Remove a delimiter from the stack for good: its text node keeps the characters no match used (literal and span
   !< set once, here), or goes when none is left.
   type(inline_parser),       intent(inout) :: p    !< Parser.
   type(markdown_document),   intent(inout) :: doc  !< Document.
   integer(I4P),              intent(in)    :: d    !< Delimiter.
   integer(I4P),              intent(inout) :: ierr !< Status.
   character(:), allocatable, intent(inout) :: msg  !< Message.
   integer(I4P)                             :: node !< Its text node.
   integer(I4P)                             :: a    !< First offset left.
   integer(I4P)                             :: k    !< Characters left.
   character                                :: c    !< The delimiter character (a fixed-length variable, not
                                                    !< `achar` in an expression: szaghi/BARUCH#2 B3).

   node = p%delim_node(d)
   k = p%delim_count(d)
   a = p%delim_first(d)
   if (k == 0_I4P) then
      call doc%unlink(node, stat=ierr, errmsg=msg)
   elseif (k < p%delim_original(d)) then
      c = achar(p%delim_char(d))
      call doc%set_literal(node, repeat(c, k), stat=ierr, errmsg=msg)
      if (ierr == BARUCH_OK) call doc%set_span(node, source_byte(p, a), source_byte(p, a + k - 1_I4P), &
                                               stat=ierr, errmsg=msg)
   endif
   call remove_delimiter(p, d)
   endsubroutine retire_delimiter

   subroutine process_emphasis(p, doc, stack_bottom, ierr, msg)
   !< Match the delimiters from `stack_bottom` (a record number: the first pushed after a bracket, 1 for the whole
   !< stack) up into emphasis and strong emphasis, then remove them all (cmark's `process_emphasis`, spec appendix).
   type(inline_parser),       intent(inout) :: p                  !< Parser.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   integer(I4P),              intent(in)    :: stack_bottom       !< First delimiter processed.
   integer(I4P),              intent(inout) :: ierr               !< Status.
   character(:), allocatable, intent(inout) :: msg                !< Message.
   integer(I4P)                             :: openers_bottom(0:11) !< Lower bound of the opener search, per key.
   integer(I4P)                             :: closer             !< Current closer candidate.
   integer(I4P)                             :: opener             !< Opener candidate.
   integer(I4P)                             :: old_closer         !< The closer before a match moved it.
   integer(I4P)                             :: key                !< Character, closer can open, length mod 3.
   integer(I4P)                             :: d                  !< Delimiter.
   logical                                  :: found              !< An opener matches.

   openers_bottom = stack_bottom
   ! the first delimiter at or above the bottom
   closer = 0_I4P
   d = p%delim_top
   do while (d /= 0_I4P)
      if (d < stack_bottom) exit
      closer = d
      d = p%delim_prev(d)
   enddo
   ! forward over the closers
   do while (closer /= 0_I4P)
      if (.not. p%delim_close(closer)) then
         closer = p%delim_next(closer)
         cycle
      endif
      key = mod(p%delim_original(closer), 3_I4P)
      if (p%delim_open(closer)) key = key + 3_I4P
      if (p%delim_char(closer) == iachar('_')) key = key + 6_I4P
      found = .false.
      opener = p%delim_prev(closer)
      do while (opener /= 0_I4P)
         if (opener < openers_bottom(key)) exit
         if (p%delim_open(opener) .and. p%delim_char(opener) == p%delim_char(closer)) then
            if (.not. (p%delim_open(closer) .or. p%delim_close(opener)) .or.          &
                mod(p%delim_original(closer), 3_I4P) == 0_I4P .or.                     &
                mod(p%delim_original(opener) + p%delim_original(closer), 3_I4P) /= 0_I4P) then
               found = .true.
               exit
            endif
         endif
         opener = p%delim_prev(opener)
      enddo
      old_closer = closer
      if (found) then
         call insert_emph(p, doc, opener, closer, ierr, msg)
         if (ierr /= BARUCH_OK) return
      else
         closer = p%delim_next(closer)
         openers_bottom(key) = old_closer ! no opener below it for this key, now or later
         if (.not. p%delim_open(old_closer)) then
            call retire_delimiter(p, doc, old_closer, ierr, msg)
            if (ierr /= BARUCH_OK) return
         endif
      endif
   enddo
   ! every delimiter from the bottom up leaves the stack
   do while (p%delim_top /= 0_I4P)
      if (p%delim_top < stack_bottom) exit
      call retire_delimiter(p, doc, p%delim_top, ierr, msg)
      if (ierr /= BARUCH_OK) return
   enddo
   endsubroutine process_emphasis

   subroutine insert_emph(p, doc, opener, closer, ierr, msg)
   !< Match an opener and a closer: two characters of each (strong emphasis) when both have two, else one; the nodes
   !< between them become the children of the new node, the delimiters between them leave the stack; an opener or a
   !< closer with no character left goes. `closer` becomes the closer to try next (itself if characters are left).
   type(inline_parser),       intent(inout) :: p      !< Parser.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(in)    :: opener !< Opener.
   integer(I4P),              intent(inout) :: closer !< Closer; on exit the next one to try.
   integer(I4P),              intent(inout) :: ierr   !< Status.
   character(:), allocatable, intent(inout) :: msg    !< Message.
   integer(I4P)                             :: used   !< Characters used from each.
   integer(I4P)                             :: first  !< First offset of the new node (the opener's used ones).
   integer(I4P)                             :: last   !< Last offset (the closer's used ones).
   integer(I4P)                             :: d      !< Delimiter between.
   integer(I4P)                             :: prev   !< The one below it.
   integer(I4P)                             :: emph   !< New node.
   integer(I4P)                             :: t      !< Node moved.
   integer(I4P)                             :: next   !< The one after it.
   integer(I4P)                             :: onode  !< Opener's text node.
   integer(I4P)                             :: cnode  !< Closer's text node.

   used = 1_I4P
   if (p%delim_count(opener) >= 2_I4P .and. p%delim_count(closer) >= 2_I4P) used = 2_I4P
   p%delim_count(opener) = p%delim_count(opener) - used ! an opener gives its last characters
   first = p%delim_first(opener) + p%delim_count(opener)
   last = p%delim_first(closer) + used - 1_I4P          ! a closer its first
   p%delim_first(closer) = p%delim_first(closer) + used
   p%delim_count(closer) = p%delim_count(closer) - used
   d = p%delim_prev(closer)
   do while (d /= 0_I4P .and. d /= opener)
      prev = p%delim_prev(d)
      call retire_delimiter(p, doc, d, ierr, msg)
      if (ierr /= BARUCH_OK) return
      d = prev
   enddo
   onode = p%delim_node(opener)
   cnode = p%delim_node(closer)
   if (used == 2_I4P) then
      emph = doc%new_node(NODE_STRONG, stat=ierr, errmsg=msg)
   else
      emph = doc%new_node(NODE_EMPH, stat=ierr, errmsg=msg)
   endif
   if (ierr == BARUCH_OK) call doc%set_int_slot(emph, SLOT_EMPHASIS_CHAR, p%delim_char(opener), stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_span(emph, source_byte(p, first), source_byte(p, last), stat=ierr, errmsg=msg)
   t = doc%next_sibling(onode) ! the new node is linked after its children: no ancestor walk per child
   do while (ierr == BARUCH_OK .and. t /= cnode .and. t /= 0_I4P)
      next = doc%next_sibling(t)
      call doc%append_child(emph, t, stat=ierr, errmsg=msg)
      t = next
   enddo
   if (ierr == BARUCH_OK) call doc%insert_after(onode, emph, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   if (p%delim_count(opener) == 0_I4P) then
      call retire_delimiter(p, doc, opener, ierr, msg)
      if (ierr /= BARUCH_OK) return
   endif
   if (p%delim_count(closer) == 0_I4P) then
      d = p%delim_next(closer)
      call retire_delimiter(p, doc, closer, ierr, msg)
      closer = d
   endif
   endsubroutine insert_emph

   subroutine push_bracket(p, doc, image, ierr, msg)
   !< A `[` (or `![`, an image): a text node of its own, pushed on the bracket stack, active.
   type(inline_parser),       intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   logical,                   intent(in)    :: image !< `![`.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   integer(I4P)                             :: last  !< Last byte of the opener.
   integer(I4P)                             :: node  !< Its text node.
   integer(I4P)                             :: b     !< New bracket.

   last = p%pos
   if (image) last = p%pos + 1_I4P
   node = add_text_node(p, doc, p%pos, last, ierr, msg)
   if (ierr /= BARUCH_OK) return
   p%pos = last + 1_I4P
   if (p%brackets == size(p%bracket_node, kind=I4P)) then
      call grow(p%bracket_node)
      call grow(p%bracket_pos)
      call grow(p%bracket_delim)
      call grow_logical(p%bracket_image)
      call grow_logical(p%bracket_active)
      call grow_logical(p%bracket_after)
   endif
   if (p%brackets > 0_I4P) p%bracket_after(p%brackets) = .true.
   p%brackets = p%brackets + 1_I4P
   b = p%brackets
   p%bracket_node(b) = node
   p%bracket_pos(b) = p%pos
   p%bracket_delim(b) = p%delims_used + 1_I4P
   p%bracket_image(b) = image
   p%bracket_active(b) = .true.
   p%bracket_after(b) = .false.
   endsubroutine push_bracket

   subroutine handle_close_bracket(p, doc, ierr, msg)
   !< A `]`. With an active bracket on top of the stack, try an inline link, then a full, collapsed or shortcut
   !< reference (cmark's `handle_close_bracket`); on a match, the link or the image; else (or with no bracket, or an
   !< inactive one) `]` is text and the top bracket is popped.
   type(inline_parser),       intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   integer(I4P),              intent(inout) :: ierr        !< Status.
   character(:), allocatable, intent(inout) :: msg         !< Message.
   character(:), allocatable                :: destination !< Destination, processed.
   character(:), allocatable                :: title       !< Title, processed.
   character(:), allocatable                :: label       !< Label as written (reference forms).
   integer(I4P)                             :: rb          !< Offset of `]`.
   integer(I4P)                             :: initial     !< Offset after it.
   integer(I4P)                             :: b           !< Top bracket.
   integer(I4P)                             :: form        !< Link form.
   integer(I4P)                             :: k           !< Position.
   integer(I4P)                             :: d_first     !< First byte of the destination.
   integer(I4P)                             :: d_last      !< Last byte.
   integer(I4P)                             :: end_url     !< Offset after the destination.
   integer(I4P)                             :: t_first     !< Offset of the title.
   integer(I4P)                             :: t_length    !< Bytes of the title, delimiters included.
   integer(I4P)                             :: l_first     !< First byte of a `[label]` after `]`.
   integer(I4P)                             :: l_last      !< Last byte.
   integer(I4P)                             :: after       !< Offset after the reference.
   integer(I4P)                             :: def         !< Definition referred to.
   logical                                  :: ok          !< A step matched.
   logical                                  :: has_label   !< A `[label]` follows.

   rb = p%pos
   p%pos = p%pos + 1_I4P
   initial = p%pos
   if (p%brackets == 0_I4P) then
      call add_text(p, ']', rb, rb)
      return
   endif
   b = p%brackets
   if (.not. p%bracket_active(b)) then
      p%brackets = b - 1_I4P
      call add_text(p, ']', rb, rb)
      return
   endif
   ! an inline link: `(`, white space, destination, white space, title, white space, `)`
   ok = .false.
   if (initial <= p%n) then
      if (p%subj(initial:initial) == '(') then
         k = initial + 1_I4P
         call skip_white(p, k)
         call scan_destination(p%subj, k, .true., d_first, d_last, ok)
         if (ok) then
            end_url = k
            call skip_white(p, k)
            t_first = k
            t_length = 0_I4P
            if (t_first /= end_url) t_length = scan_title(p%subj, t_first)
            k = t_first + t_length
            if (t_length > 0_I4P) call skip_white(p, k)
            ok = .false.
            if (k <= p%n) ok = p%subj(k:k) == ')'
         endif
         if (ok) then
            destination = unescape_string(p%subj(d_first:d_last))
            title = ''
            if (t_length > 0_I4P) title = unescape_string(p%subj(t_first+1:t_first+t_length-2))
            label = ''
            form = LINK_INLINE
            after = k + 1_I4P
         endif
      endif
   endif
   ! a reference: full `[label]`, collapsed `[]`, shortcut (the text itself, when no bracket is inside it)
   if (.not. ok) then
      call scan_link_label(p, initial, has_label, l_first, l_last)
      ! a blank `[ ]` is no link label (spec 6.3): not a full reference, nor `[]`; the text may be a shortcut
      if (has_label .and. l_last >= l_first) has_label = verify(p%subj(l_first:l_last), ' '//TAB//LF) > 0
      def = 0_I4P
      if (has_label .and. l_last >= l_first) then
         label = p%subj(l_first:l_last)
         form = LINK_FULL
         after = l_last + 2_I4P
         def = p%refs%lookup(label)
      elseif (.not. p%bracket_after(b)) then
         label = p%subj(p%bracket_pos(b):rb-1_I4P)
         if (has_label) then
            form = LINK_COLLAPSED
            after = l_last + 2_I4P
         else
            form = LINK_SHORTCUT
            after = initial
         endif
         def = p%refs%lookup(label)
      endif
      if (def /= 0_I4P) then
         ok = .true.
         destination = doc%destination(def)
         title = doc%title(def)
      endif
   endif
   if (.not. ok) then
      p%brackets = b - 1_I4P
      call add_text(p, ']', rb, rb)
      return
   endif
   p%pos = after
   call add_link(p, doc, b, form, destination, title, label, ierr, msg)
   endsubroutine handle_close_bracket

   subroutine add_link(p, doc, b, form, destination, title, label, ierr, msg)
   !< A link or an image whose text starts after bracket `b` (the top one) and ends before the cursor's construct:
   !< the nodes after the bracket's text node become its children, the bracket's node goes, the delimiters inside are
   !< processed, the bracket is popped, and a link deactivates the `[` before it.
   type(inline_parser),       intent(inout) :: p           !< Parser.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   integer(I4P),              intent(in)    :: b           !< The bracket.
   integer(I4P),              intent(in)    :: form        !< Link form.
   character(*),              intent(in)    :: destination !< Destination.
   character(*),              intent(in)    :: title       !< Title.
   character(*),              intent(in)    :: label       !< Label as written.
   integer(I4P),              intent(inout) :: ierr        !< Status.
   character(:), allocatable, intent(inout) :: msg         !< Message.
   integer(I4P)                             :: link        !< Link or image.
   integer(I4P)                             :: opener      !< The bracket's text node.
   integer(I4P)                             :: first       !< First offset of the link (its `[` or `!`).
   integer(I4P)                             :: t           !< Node moved.
   integer(I4P)                             :: next        !< The one after it.
   integer(I4P)                             :: k           !< Bracket.

   call flush_text(p, doc, ierr, msg)
   if (ierr /= BARUCH_OK) return
   opener = p%bracket_node(b)
   first = p%bracket_pos(b) - 1_I4P
   if (p%bracket_image(b)) then
      first = first - 1_I4P
      link = doc%new_node(NODE_IMAGE, stat=ierr, errmsg=msg)
   else
      link = doc%new_node(NODE_LINK, stat=ierr, errmsg=msg)
   endif
   if (ierr == BARUCH_OK) call doc%set_span(link, source_byte(p, first), source_byte(p, p%pos - 1_I4P), &
                                            stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_int_slot(link, SLOT_LINK_FORM, form, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_destination(link, destination, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK .and. len(title) > 0) call doc%set_title(link, title, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK .and. len(label) > 0) call doc%set_label(link, label, stat=ierr, errmsg=msg)
   t = doc%next_sibling(opener) ! the new node is linked after its children: no ancestor walk per child
   do while (ierr == BARUCH_OK .and. t /= 0_I4P)
      next = doc%next_sibling(t)
      call doc%append_child(link, t, stat=ierr, errmsg=msg)
      t = next
   enddo
   if (ierr == BARUCH_OK) call doc%insert_before(opener, link, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%unlink(opener, stat=ierr, errmsg=msg)
   if (ierr /= BARUCH_OK) return
   call process_emphasis(p, doc, p%bracket_delim(b), ierr, msg)
   if (ierr /= BARUCH_OK) return
   p%brackets = b - 1_I4P
   if (.not. p%bracket_image(b)) then ! links do not nest: the `[` before this one open nothing any more
      do k=p%brackets, 1_I4P, -1_I4P
         if (p%bracket_image(k)) cycle
         if (.not. p%bracket_active(k)) exit ! the ones below were deactivated by an earlier link
         p%bracket_active(k) = .false.
      enddo
   endif
   endsubroutine add_link

   subroutine scan_link_label(p, at, found, first, last)
   !< A link label right at offset `at`: `[`, at most 999 characters with no unescaped bracket, `]` (cmark's
   !< `link_label`: the look-ahead is bounded). `first:last` is its content.
   type(inline_parser), intent(in)  :: p      !< Parser.
   integer(I4P),        intent(in)  :: at     !< Offset.
   logical,             intent(out) :: found  !< A label.
   integer(I4P),        intent(out) :: first  !< First byte of its content.
   integer(I4P),        intent(out) :: last   !< Last byte.
   integer(I4P)                     :: k      !< Position.
   integer(I4P)                     :: length !< Characters read.

   found = .false.
   first = at + 1_I4P
   last = at
   if (at > p%n) return
   if (p%subj(at:at) /= '[') return
   k = at + 1_I4P
   length = 0_I4P
   do while (k <= p%n)
      if (p%subj(k:k) == '[' .or. p%subj(k:k) == ']') exit
      if (p%subj(k:k) == BACKSLASH .and. k < p%n) then
         if (is_ascii_punctuation(p%subj(k+1:k+1))) then
            k = k + 1_I4P
            length = length + 1_I4P
         endif
      endif
      k = k + 1_I4P
      length = length + 1_I4P
      if (length > LABEL_MAX) return
   enddo
   if (k > p%n) return
   if (p%subj(k:k) /= ']') return
   found = .true.
   last = k - 1_I4P
   endsubroutine scan_link_label

   pure subroutine skip_white(p, k)
   !< Move offset `k` past spaces, tabs and line endings.
   type(inline_parser), intent(in)    :: p !< Parser.
   integer(I4P),        intent(inout) :: k !< Offset.

   do while (k <= p%n)
      if (p%subj(k:k) /= ' ' .and. p%subj(k:k) /= TAB .and. p%subj(k:k) /= LF) exit
      k = k + 1_I4P
   enddo
   endsubroutine skip_white

   subroutine skip_spaces(p)
   !< Move the cursor past spaces and tabs.
   type(inline_parser), intent(inout) :: p !< Parser.

   do while (p%pos <= p%n)
      if (p%subj(p%pos:p%pos) /= ' ' .and. p%subj(p%pos:p%pos) /= TAB) exit
      p%pos = p%pos + 1_I4P
   enddo
   endsubroutine skip_spaces

   ! nodes
   subroutine add_text(p, bytes, first, last)
   !< Accumulate text: `bytes`, standing for the subject's offsets `first` to `last`.
   type(inline_parser), intent(inout) :: p     !< Parser.
   character(*),        intent(in)    :: bytes !< Text.
   integer(I4P),        intent(in)    :: first !< First offset it stands for.
   integer(I4P),        intent(in)    :: last  !< Last offset.
   character(:), allocatable          :: tmp   !< Grown buffer.
   integer(I4P)                       :: need  !< Bytes needed.

   need = p%text_used + len(bytes, kind=I4P)
   if (need > len(p%text, kind=I4P)) then ! grown by assignment, not move_alloc (nvfortran 26.9, szaghi/BARUCH#2 B1)
      allocate(character(max(need, 2_I4P * len(p%text, kind=I4P))) :: tmp)
      tmp(1:p%text_used) = p%text(1:p%text_used)
      p%text = tmp
   endif
   if (p%text_used == 0_I4P) p%text_first = first
   p%text(p%text_used+1:need) = bytes
   p%text_used = need
   p%text_last = last
   endsubroutine add_text

   subroutine flush_text(p, doc, ierr, msg)
   !< Write the text accumulated, if any, as one text node.
   type(inline_parser),       intent(inout) :: p    !< Parser.
   type(markdown_document),   intent(inout) :: doc  !< Document.
   integer(I4P),              intent(inout) :: ierr !< Status.
   character(:), allocatable, intent(inout) :: msg  !< Message.
   integer(I4P)                             :: node !< Text node.

   if (p%text_used == 0_I4P) return
   node = add_node(p, doc, NODE_TEXT, p%text_first, p%text_last, ierr, msg)
   if (ierr == BARUCH_OK) call doc%set_literal(node, p%text(1:p%text_used), stat=ierr, errmsg=msg)
   p%text_used = 0_I4P
   endsubroutine flush_text

   function add_text_node(p, doc, first, last, ierr, msg) result(node)
   !< Write the text accumulated, then the subject's offsets `first` to `last` as a text node of their own (a
   !< delimiter run, a bracket); return it.
   type(inline_parser),       intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(in)    :: first !< First offset.
   integer(I4P),              intent(in)    :: last  !< Last offset.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   integer(I4P)                             :: node  !< Text node.

   node = 0_I4P
   call flush_text(p, doc, ierr, msg)
   if (ierr == BARUCH_OK) node = add_node(p, doc, NODE_TEXT, first, last, ierr, msg)
   if (ierr == BARUCH_OK) call doc%set_literal(node, p%subj(first:last), stat=ierr, errmsg=msg)
   endfunction add_text_node

   function add_node(p, doc, kind, first, last, ierr, msg) result(node)
   !< Append a new node of a kind to the block, spanning the source bytes of the subject's offsets `first` to `last`.
   type(inline_parser),       intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(in)    :: kind  !< Kind.
   integer(I4P),              intent(in)    :: first !< First offset.
   integer(I4P),              intent(in)    :: last  !< Last offset.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   integer(I4P)                             :: node  !< New node.

   node = doc%new_node(kind, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%set_span(node, source_byte(p, first), source_byte(p, last), stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call doc%append_child(p%block, node, stat=ierr, errmsg=msg)
   endfunction add_node

   subroutine merge_texts(doc, top, ierr, msg)
   !< Merge every run of adjacent text nodes under `top` into its first: the literals concatenated once, the spans
   !< joined, the others unlinked. The subtree is visited by its links, with no recursion.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(in)    :: top   !< Root of the subtree.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   character(:), allocatable                :: text  !< The run's literal.
   character(:), allocatable                :: piece !< A literal of the run.
   integer(I4P)                             :: n     !< Current node.
   integer(I4P)                             :: s     !< Node of the run.
   integer(I4P)                             :: after !< The node after the run.
   integer(I4P)                             :: bytes !< Bytes of the run.
   integer(I4P)                             :: next  !< Next node in the visit.

   n = doc%first_child(top)
   do while (n /= 0_I4P)
      if (doc%node_kind(n) == NODE_TEXT) then
         after = doc%next_sibling(n)
         if (after /= 0_I4P) then
            if (doc%node_kind(after) == NODE_TEXT) then
               bytes = 0_I4P
               s = n
               do while (s /= 0_I4P)
                  if (doc%node_kind(s) /= NODE_TEXT) exit
                  bytes = bytes + len(doc%literal(s), kind=I4P)
                  s = doc%next_sibling(s)
               enddo
               allocate(character(bytes) :: text)
               bytes = 0_I4P
               s = n
               do while (s /= 0_I4P)
                  if (doc%node_kind(s) /= NODE_TEXT) exit
                  piece = doc%literal(s)
                  text(bytes+1:bytes+len(piece)) = piece
                  bytes = bytes + len(piece, kind=I4P)
                  after = s
                  s = doc%next_sibling(s)
               enddo
               call doc%set_literal(n, text, stat=ierr, errmsg=msg)
               if (ierr == BARUCH_OK) call doc%set_span(n, doc%span_first(n), doc%span_last(after), stat=ierr, errmsg=msg)
               deallocate(text)
               do while (ierr == BARUCH_OK) ! unlink the others
                  s = doc%next_sibling(n)
                  if (s == 0_I4P) exit
                  if (doc%node_kind(s) /= NODE_TEXT) exit
                  call doc%unlink(s, stat=ierr, errmsg=msg)
               enddo
               if (ierr /= BARUCH_OK) return
            endif
         endif
      endif
      ! the next node: the first child, else the next sibling of the node or of its nearest ancestor below `top`
      next = doc%first_child(n)
      do while (next == 0_I4P .and. n /= top)
         next = doc%next_sibling(n)
         if (next == 0_I4P) n = doc%parent(n)
      enddo
      n = next
   enddo
   endsubroutine merge_texts

   pure subroutine grow(a)
   !< Double an integer array, its values kept.
   integer(I4P), allocatable, intent(inout) :: a(:) !< Array.
   integer(I4P), allocatable                :: b(:) !< Grown copy.

   allocate(b(2 * size(a)))
   b(1:size(a)) = a
   b(size(a)+1:) = 0_I4P
   call move_alloc(b, a)
   endsubroutine grow

   pure subroutine grow_logical(a)
   !< Double a logical array, its values kept.
   logical, allocatable, intent(inout) :: a(:) !< Array.
   logical, allocatable                :: b(:) !< Grown copy.

   allocate(b(2 * size(a)))
   b(1:size(a)) = a
   b(size(a)+1:) = .false.
   call move_alloc(b, a)
   endsubroutine grow_logical
endmodule baruch_inlines
