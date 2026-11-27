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
!< - `*`, `_`: a delimiter run, a text node of its own pushed on the **delimiter stack**;
!< - `[`, `![`: a text node of its own pushed on the **bracket stack**; `]` closes the top bracket;
!< - anything else: text up to the next special byte (trailing spaces and tabs before a LF dropped).
!<
!< Text is accumulated, not written byte by byte: consecutive pieces (plain text, an escaped character, a reference's
!< bytes) become **one** text node when something else comes. After the subject, `process_emphasis` runs over the
!< delimiter stack, the bracket stack is emptied, and the adjacent text nodes left by the delimiters and brackets that
!< matched nothing are **merged** (one pass over the block's subtree, each run concatenated once).
!<
!< **What plan step 2.4 completes.** The delimiter stack (doubly linked through indices, so that a delimiter can be
!< removed from its middle) and the bracket stack (with its image flag, `active`, `bracket_after` and the delimiter
!< below it) are built and maintained here; the flanking rules of a delimiter run (`can_open`, `can_close`), the
!< matching loop of `process_emphasis` and the link and image resolution of a `]` are step 2.4's. Until then a
!< delimiter run is pushed as neither opener nor closer, `process_emphasis` only clears the stack, and a `]` only pops
!< its bracket: emphasis, links and images stay text.
use penf,                only : I4P
use baruch_document,     only : markdown_document
use baruch_kinds_nodes,  only : NODE_PARAGRAPH, NODE_HEADING, NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, &
                                NODE_HTML_INLINE, NODE_LINK, SLOT_LINK_FORM, LINK_AUTOLINK
use baruch_blocks_scan,  only : BACKSLASH, is_ascii_punctuation, is_ascii_letter
use baruch_inlines_scan, only : scan_reference, decode_references, scan_autolink_uri, scan_autolink_email,       &
                                scan_html_tag, scan_html_comment, scan_processing_instruction, scan_declaration, &
                                scan_cdata
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
   integer(I4P), allocatable :: delim_prev(:)          !< Delimiter below, 0 if none.
   integer(I4P), allocatable :: delim_next(:)          !< Delimiter above, 0 if none.
   logical,      allocatable :: delim_open(:)          !< The run can open emphasis.
   logical,      allocatable :: delim_close(:)         !< The run can close emphasis.
   ! the bracket stack
   integer(I4P)              :: brackets = 0_I4P       !< Brackets on the stack.
   integer(I4P), allocatable :: bracket_node(:)        !< The text node of `[` or `![`.
   integer(I4P), allocatable :: bracket_pos(:)         !< Offset of the subject after it.
   integer(I4P), allocatable :: bracket_delim(:)       !< The top of the delimiter stack when it was pushed.
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
            p%delim_original(STACK_DEFAULT), p%delim_prev(STACK_DEFAULT), p%delim_next(STACK_DEFAULT),     &
            p%delim_open(STACK_DEFAULT), p%delim_close(STACK_DEFAULT))
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
   call process_emphasis(p, 0_I4P)
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
      call handle_close_bracket(p)
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
   !< A run of `*` or `_`: a text node of its own, pushed on the delimiter stack. Whether it can open or close
   !< emphasis (the flanking rules) is plan step 2.4's: until then it is pushed as neither.
   type(inline_parser),       intent(inout) :: p     !< Parser.
   type(markdown_document),   intent(inout) :: doc   !< Document.
   integer(I4P),              intent(inout) :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   character                                :: c     !< The delimiter character.
   integer(I4P)                             :: first !< First byte of the run.
   integer(I4P)                             :: node  !< Its text node.

   c = p%subj(p%pos:p%pos)
   first = p%pos
   do while (p%pos <= p%n)
      if (p%subj(p%pos:p%pos) /= c) exit
      p%pos = p%pos + 1_I4P
   enddo
   node = add_text_node(p, doc, first, p%pos - 1_I4P, ierr, msg)
   if (ierr /= BARUCH_OK) return
   call push_delimiter(p, node, c, p%pos - first, .false., .false.)
   endsubroutine handle_delim

   subroutine push_delimiter(p, node, c, count, can_open, can_close)
   !< Push a delimiter run on the delimiter stack.
   type(inline_parser), intent(inout) :: p         !< Parser.
   integer(I4P),        intent(in)    :: node      !< Its text node.
   character,           intent(in)    :: c         !< Its character.
   integer(I4P),        intent(in)    :: count     !< Its length.
   logical,             intent(in)    :: can_open  !< It can open emphasis.
   logical,             intent(in)    :: can_close !< It can close emphasis.
   integer(I4P)                       :: d         !< New record.

   if (p%delims_used == size(p%delim_node, kind=I4P)) then
      call grow(p%delim_node)
      call grow(p%delim_char)
      call grow(p%delim_count)
      call grow(p%delim_original)
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

   subroutine process_emphasis(p, stack_bottom)
   !< Process the delimiters above `stack_bottom` (0: the whole stack), then remove them. The matching of openers and
   !< closers into emphasis and strong emphasis is plan step 2.4's; until then no delimiter can open or close, and the
   !< delimiters are only removed (their text nodes stay text).
   type(inline_parser), intent(inout) :: p            !< Parser.
   integer(I4P),        intent(in)    :: stack_bottom !< Delimiter below the ones processed, 0 for none.

   do while (p%delim_top /= stack_bottom .and. p%delim_top /= 0_I4P)
      call remove_delimiter(p, p%delim_top)
   enddo
   endsubroutine process_emphasis

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
   p%bracket_delim(b) = p%delim_top
   p%bracket_image(b) = image
   p%bracket_active(b) = .true.
   p%bracket_after(b) = .false.
   endsubroutine push_bracket

   subroutine handle_close_bracket(p)
   !< A `]`: the top bracket is popped and `]` is text. Matching it with a link or an image (an inline destination, a
   !< reference) is plan step 2.4's.
   type(inline_parser), intent(inout) :: p !< Parser.

   if (p%brackets > 0_I4P) p%brackets = p%brackets - 1_I4P
   call add_text(p, ']', p%pos, p%pos)
   p%pos = p%pos + 1_I4P
   endsubroutine handle_close_bracket

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
