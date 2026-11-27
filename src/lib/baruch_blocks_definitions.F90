!< BARUCH, link reference definitions: read from the start of a paragraph's content (CommonMark 0.31.2, 4.7).
module baruch_blocks_definitions
!< BARUCH, link reference definitions: read from the start of a paragraph's content (CommonMark 0.31.2, 4.7).
!<
!< When a paragraph closes (or a setext underline comes under it), the block parser hands its content, the lines with
!< their leading white space removed and each followed by LF, to `parse_definition` as long as it starts with `[`:
!< each definition read is a `link_reference_definition` node in place, before the paragraph, and the paragraph keeps
!< the rest (it goes if nothing is left). A definition always ends at a line ending, so it takes whole lines.
!<
!< What is stored (resolution of references against the definitions comes with the links, plan step 2.4):
!<
!< - the **label** as written between the brackets, untouched (matching normalises it: case fold, white space);
!< - the **destination** without its `<>`, if any, and the **title** without its delimiters, both with their
!<   backslash escapes processed. Entity and numeric character references stay as written until the HTML5 entity
!<   table of plan step 2.2 (decoded by the inline step 2.3).
use penf,               only : I4P
use baruch_blocks_scan, only : BACKSLASH, is_space_or_tab, is_ascii_punctuation, backslash_unescape
implicit none
private
public :: parse_definition

integer(I4P), parameter :: LABEL_MAX = 999_I4P !< Characters a link label may hold (CommonMark 6.3).
integer(I4P), parameter :: PARENS_MAX = 32_I4P !< Nesting of unescaped parentheses in a destination (as cmark).
character,    parameter :: TAB = achar(9)      !< Tab.
character,    parameter :: LF  = achar(10)     !< Line feed.

contains
   pure subroutine parse_definition(text, i, next, label_first, label_last, destination, title)
   !< Read a link reference definition at `text(i:)` (`text(i:i)` is `[`): label, `:`, destination, optional title,
   !< then only spaces and tabs to the end of the line. `next` is the position after its line ending, 0 if no
   !< definition starts at `i`; the label is `text(label_first:label_last)`.
   character(*),              intent(in)  :: text        !< Content: lines, each ended by LF.
   integer(I4P),              intent(in)  :: i           !< Position of `[`.
   integer(I4P),              intent(out) :: next        !< Position after the definition, 0 for none.
   integer(I4P),              intent(out) :: label_first !< First byte of the label.
   integer(I4P),              intent(out) :: label_last  !< Last byte of the label.
   character(:), allocatable, intent(out) :: destination !< Destination, processed.
   character(:), allocatable, intent(out) :: title       !< Title, processed (empty if none).
   integer(I4P)                           :: pos         !< Position.
   integer(I4P)                           :: n           !< Length of the text.
   integer(I4P)                           :: d_first     !< First byte of the destination.
   integer(I4P)                           :: d_last      !< Last byte of the destination.
   integer(I4P)                           :: before      !< Position before the title.
   integer(I4P)                           :: t_length    !< Bytes of the title, delimiters included.
   logical                                :: ok          !< A step matched.

   next = 0_I4P
   label_first = 0_I4P
   label_last = -1_I4P
   destination = ''
   title = ''
   n = len(text, kind=I4P)
   ! the label
   call scan_label(text, i, pos, ok)
   if (.not. ok) return
   label_first = i + 1_I4P
   label_last = pos - 2_I4P
   if (pos > n) return
   if (text(pos:pos) /= ':') return
   pos = pos + 1_I4P
   ! the destination
   call skip_spaces_one_line_ending(text, pos)
   call scan_destination(text, pos, d_first, d_last, ok)
   if (.not. ok) return
   ! the title, optional, separated by white space
   before = pos
   call skip_spaces_one_line_ending(text, pos)
   t_length = 0_I4P
   if (pos /= before) t_length = scan_title(text, pos)
   if (t_length > 0_I4P) then
      title = backslash_unescape(text(pos+1:pos+t_length-2))
      pos = pos + t_length
   else
      pos = before
   endif
   ! nothing else on the line; a title followed by more text: the definition ends before it
   call skip_spaces(text, pos)
   if (.not. at_line_end(text, pos)) then
      if (t_length == 0_I4P) return
      title = ''
      pos = before
      call skip_spaces(text, pos)
      if (.not. at_line_end(text, pos)) return
   endif
   if (pos <= n) pos = pos + 1_I4P ! the line feed
   next = pos
   destination = backslash_unescape(text(d_first:d_last))
   endsubroutine parse_definition

   ! private
   pure subroutine scan_label(text, i, pos, ok)
   !< A link label at `i` (`[` ... `]`): no unescaped bracket inside, at most 999 characters, at least one that is not
   !< white space. `pos` is the position after `]`.
   character(*), intent(in)  :: text   !< Text.
   integer(I4P), intent(in)  :: i      !< Position of `[`.
   integer(I4P), intent(out) :: pos    !< Position after `]`.
   logical,      intent(out) :: ok     !< A label.
   integer(I4P)              :: n      !< Length.
   integer(I4P)              :: length !< Characters inside.
   logical                   :: blank  !< Only white space inside so far.

   ok = .false.
   n = len(text, kind=I4P)
   pos = i
   if (text(i:i) /= '[') return
   pos = i + 1_I4P
   length = 0_I4P
   blank = .true.
   do while (pos <= n)
      if (text(pos:pos) == '[' .or. text(pos:pos) == ']') exit
      if (.not. (is_space_or_tab(text(pos:pos)) .or. text(pos:pos) == LF)) blank = .false.
      if (text(pos:pos) == BACKSLASH .and. pos < n) then
         if (is_ascii_punctuation(text(pos+1:pos+1))) then
            pos = pos + 1_I4P
            length = length + 1_I4P
         endif
      endif
      pos = pos + 1_I4P
      length = length + 1_I4P
      if (length > LABEL_MAX) return
   enddo
   if (pos > n) return
   if (text(pos:pos) /= ']' .or. blank) return
   pos = pos + 1_I4P
   ok = .true.
   endsubroutine scan_label

   pure subroutine scan_destination(text, pos, first, last, ok)
   !< A link destination at `pos`: `<...>` (no line ending, no unescaped `<` or `>` inside), or a nonempty run of
   !< characters that are not white space with balanced unescaped parentheses. `pos` moves after it; `first:last` is
   !< the destination without its `<>`.
   character(*), intent(in)    :: text   !< Text.
   integer(I4P), intent(inout) :: pos    !< Position.
   integer(I4P), intent(out)   :: first  !< First byte.
   integer(I4P), intent(out)   :: last   !< Last byte.
   logical,      intent(out)   :: ok     !< A destination.
   integer(I4P)                :: n      !< Length.
   integer(I4P)                :: k      !< Position.
   integer(I4P)                :: parens !< Open parentheses.
   character                   :: c      !< Character.

   ok = .false.
   n = len(text, kind=I4P)
   first = pos
   last = pos - 1_I4P
   if (pos > n) return
   if (text(pos:pos) == '<') then
      k = pos + 1_I4P
      do while (k <= n)
         c = text(k:k)
         if (c == '>') exit
         if (c == BACKSLASH) then
            k = k + 2_I4P
            cycle
         endif
         if (c == LF .or. c == '<') return
         k = k + 1_I4P
      enddo
      if (k > n) return
      first = pos + 1_I4P
      last = k - 1_I4P
      pos = k + 1_I4P
   else
      k = pos
      parens = 0_I4P
      do while (k <= n)
         c = text(k:k)
         if (c == BACKSLASH .and. k < n) then
            if (is_ascii_punctuation(text(k+1:k+1))) then
               k = k + 2_I4P
               cycle
            endif
         endif
         if (c == '(') then
            parens = parens + 1_I4P
            if (parens > PARENS_MAX) return
         elseif (c == ')') then
            if (parens == 0_I4P) exit
            parens = parens - 1_I4P
         elseif (c == ' ' .or. c == TAB .or. c == LF .or. iachar(c) < 32 .or. iachar(c) == 127) then
            exit
         endif
         k = k + 1_I4P
      enddo
      if (k == pos .or. k > n .or. parens /= 0_I4P) return
      first = pos
      last = k - 1_I4P
      pos = k
   endif
   ok = .true.
   endsubroutine scan_destination

   pure function scan_title(text, pos) result(length)
   !< A link title at `pos`: `"..."`, `'...'` or `(...)`, a backslash escaping the closing delimiter (and `(` inside
   !< parentheses, which may not hold an unescaped one). Return its bytes with the delimiters, 0 for none.
   character(*), intent(in) :: text   !< Text.
   integer(I4P), intent(in) :: pos    !< Position.
   integer(I4P)             :: length !< Bytes.
   character                :: closing !< Closing delimiter.
   integer(I4P)             :: k      !< Position.
   integer(I4P)             :: n      !< Length.

   length = 0_I4P
   n = len(text, kind=I4P)
   if (pos > n) return
   select case(text(pos:pos))
   case('"')
      closing = '"'
   case("'")
      closing = "'"
   case('(')
      closing = ')'
   case default
      return
   endselect
   k = pos + 1_I4P
   do while (k <= n)
      if (text(k:k) == BACKSLASH .and. k < n) then
         if (is_ascii_punctuation(text(k+1:k+1))) then
            k = k + 2_I4P
            cycle
         endif
      endif
      if (text(k:k) == closing) then
         length = k - pos + 1_I4P
         return
      endif
      if (closing == ')' .and. text(k:k) == '(') return
      k = k + 1_I4P
   enddo
   endfunction scan_title

   pure subroutine skip_spaces(text, pos)
   !< Move `pos` past spaces and tabs.
   character(*), intent(in)    :: text !< Text.
   integer(I4P), intent(inout) :: pos  !< Position.

   do while (pos <= len(text, kind=I4P))
      if (.not. is_space_or_tab(text(pos:pos))) exit
      pos = pos + 1_I4P
   enddo
   endsubroutine skip_spaces

   pure subroutine skip_spaces_one_line_ending(text, pos)
   !< Move `pos` past spaces and tabs, at most one line ending, and the spaces and tabs after it.
   character(*), intent(in)    :: text !< Text.
   integer(I4P), intent(inout) :: pos  !< Position.

   call skip_spaces(text, pos)
   if (pos <= len(text, kind=I4P)) then
      if (text(pos:pos) == LF) then
         pos = pos + 1_I4P
         call skip_spaces(text, pos)
      endif
   endif
   endsubroutine skip_spaces_one_line_ending

   pure function at_line_end(text, pos) result(is)
   !< True at a line feed or at the end of the text.
   character(*), intent(in) :: text !< Text.
   integer(I4P), intent(in) :: pos  !< Position.
   logical                  :: is   !< Result.

   is = pos > len(text, kind=I4P)
   if (.not. is) is = text(pos:pos) == LF
   endfunction at_line_end
endmodule baruch_blocks_definitions
