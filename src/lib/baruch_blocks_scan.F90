!< BARUCH, the block scanners: what a line starts with, read from a position (CommonMark 0.31.2, sections 4 and 5).
module baruch_blocks_scan
!< BARUCH, the block scanners: what a line starts with, read from a position (CommonMark 0.31.2, sections 4 and 5).
!<
!< Pure procedures on one line of the source (`line`, its ending excluded) and a 1-based position `i` in it: the block
!< parser (`baruch_blocks`) asks them whether a thematic break, an ATX heading, a code fence, an HTML block, a setext
!< underline or a list marker starts there. Beyond the end of the line is the end of the line: no scanner reads past
!< `len(line)`. Nothing here allocates but the unescaped strings, nothing keeps state.
!<
!< The HTML tag scanners (`scan_open_tag`, `scan_closing_tag`) are the spec's "open tag" and "closing tag" (section
!< 6.6); the white space they allow is spaces, tabs and line endings, so the inline parser can reuse them on text that
!< spans lines.
use penf, only : I4P
implicit none
private
public :: BACKSLASH
public :: is_space_or_tab, is_ascii_punctuation, is_ascii_letter, is_ascii_digit, lower_ascii
public :: backslash_unescape
public :: scan_thematic_break, scan_atx_start, scan_open_fence, scan_close_fence, scan_setext_underline
public :: scan_list_marker
public :: scan_html_block_start, html_block_end
public :: scan_open_tag, scan_closing_tag

character, parameter :: BACKSLASH = achar(92) !< Backslash (no literal: nvfortran reads one as an escape).
character, parameter :: TAB = achar(9)  !< Tab.
character, parameter :: LF  = achar(10) !< Line feed.
character, parameter :: CR  = achar(13) !< Carriage return.

contains
   ! characters
   elemental function is_space_or_tab(c) result(is)
   !< True for a space or a tab.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.

   is = c == ' ' .or. c == TAB
   endfunction is_space_or_tab

   elemental function is_ascii_punctuation(c) result(is)
   !< True for an ASCII punctuation character (CommonMark 2.1): ``!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~``.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.
   integer               :: k  !< Code.

   k = iachar(c)
   is = (k >= 33 .and. k <= 47) .or. (k >= 58 .and. k <= 64) .or. (k >= 91 .and. k <= 96) .or. (k >= 123 .and. k <= 126)
   endfunction is_ascii_punctuation

   elemental function is_ascii_letter(c) result(is)
   !< True for an ASCII letter.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.

   is = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')
   endfunction is_ascii_letter

   elemental function is_ascii_digit(c) result(is)
   !< True for an ASCII digit.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.

   is = c >= '0' .and. c <= '9'
   endfunction is_ascii_digit

   elemental function lower_ascii(c) result(l)
   !< An ASCII upper-case letter in lower case; any other byte unchanged.
   character, intent(in) :: c !< Character.
   character             :: l !< Result.

   l = c
   if (c >= 'A' .and. c <= 'Z') l = achar(iachar(c) + 32)
   endfunction lower_ascii

   pure function backslash_unescape(s) result(u)
   !< Return `s` with its backslash escapes replaced by the characters they escape: a backslash before an ASCII
   !< punctuation character is dropped (CommonMark 2.4); any other backslash stays.
   character(*), intent(in)  :: s !< Text.
   character(:), allocatable :: u !< Unescaped text.
   integer                   :: i !< Position in `s`.
   integer                   :: j !< Length of `u`.

   if (index(s, BACKSLASH) == 0) then
      u = s
      return
   endif
   allocate(character(len(s)) :: u)
   i = 1
   j = 0
   do while (i <= len(s))
      if (s(i:i) == BACKSLASH .and. i < len(s)) then
         if (is_ascii_punctuation(s(i+1:i+1))) i = i + 1
      endif
      j = j + 1
      u(j:j) = s(i:i)
      i = i + 1
   enddo
   u = u(1:j)
   endfunction backslash_unescape

   ! leaf block starts
   pure subroutine scan_thematic_break(line, i, char_code, fail_at)
   !< A thematic break at `i` (CommonMark 4.1): three or more matching `-`, `_` or `*`, with only spaces and tabs
   !< between them and after. `char_code` is the character's code, 0 if none; `fail_at` the position where the scan
   !< failed (the parser does not scan again a line already failed at or after a position).
   character(*), intent(in)  :: line      !< Line.
   integer(I4P), intent(in)  :: i         !< Position.
   integer(I4P), intent(out) :: char_code !< `iachar` of the break character, 0 for no break.
   integer(I4P), intent(out) :: fail_at   !< Position where the scan failed (0 on success).
   character                 :: c         !< Break character.
   integer(I4P)              :: k         !< Position.
   integer(I4P)              :: count     !< Break characters.

   char_code = 0_I4P
   fail_at = i
   if (i > len(line, kind=I4P)) return
   c = line(i:i)
   if (c /= '-' .and. c /= '_' .and. c /= '*') return
   count = 0_I4P
   do k=i, len(line, kind=I4P)
      if (line(k:k) == c) then
         count = count + 1_I4P
      elseif (.not. is_space_or_tab(line(k:k))) then
         fail_at = k
         return
      endif
   enddo
   if (count >= 3_I4P) then
      char_code = iachar(c, kind=I4P)
      fail_at = 0_I4P
   else
      fail_at = len(line, kind=I4P) + 1_I4P
   endif
   endsubroutine scan_thematic_break

   pure subroutine scan_atx_start(line, i, level, matched)
   !< An ATX heading opening sequence at `i` (CommonMark 4.2): 1 to 6 `#` followed by a space, a tab or the end of the
   !< line. `level` is the number of `#` (0: none); `matched` the bytes of the sequence and of the spaces and tabs after
   !< it.
   character(*), intent(in)  :: line    !< Line.
   integer(I4P), intent(in)  :: i       !< Position.
   integer(I4P), intent(out) :: level   !< Level, 0 for no heading.
   integer(I4P), intent(out) :: matched !< Bytes matched.
   integer(I4P)              :: k       !< Position.

   level = 0_I4P
   matched = 0_I4P
   k = i
   do while (k <= len(line, kind=I4P))
      if (line(k:k) /= '#') exit
      k = k + 1_I4P
   enddo
   if (k - i < 1_I4P .or. k - i > 6_I4P) return
   if (k <= len(line, kind=I4P)) then
      if (.not. is_space_or_tab(line(k:k))) return
   endif
   level = k - i
   do while (k <= len(line, kind=I4P))
      if (.not. is_space_or_tab(line(k:k))) exit
      k = k + 1_I4P
   enddo
   matched = k - i
   endsubroutine scan_atx_start

   pure function scan_open_fence(line, i) result(length)
   !< An opening code fence at `i` (CommonMark 4.5): three or more backticks or tildes; after backticks, the rest of the
   !< line holds no backtick. Return the fence length, 0 for none.
   character(*), intent(in) :: line   !< Line.
   integer(I4P), intent(in) :: i      !< Position.
   integer(I4P)             :: length !< Fence length.
   character                :: c      !< Fence character.
   integer(I4P)             :: k      !< Position.

   length = 0_I4P
   if (i > len(line, kind=I4P)) return
   c = line(i:i)
   if (c /= '`' .and. c /= '~') return
   k = i
   do while (k <= len(line, kind=I4P))
      if (line(k:k) /= c) exit
      k = k + 1_I4P
   enddo
   if (k - i < 3_I4P) return
   if (c == '`' .and. k <= len(line, kind=I4P)) then
      if (index(line(k:), '`') > 0) return
   endif
   length = k - i
   endfunction scan_open_fence

   pure function scan_close_fence(line, i, c) result(length)
   !< A closing code fence of character `c` at `i` (CommonMark 4.5): three or more `c`, then only spaces and tabs.
   !< Return its length, 0 for none (the caller compares it with the opening fence's).
   character(*), intent(in) :: line   !< Line.
   integer(I4P), intent(in) :: i      !< Position.
   character,    intent(in) :: c      !< Fence character.
   integer(I4P)             :: length !< Fence length.
   integer(I4P)             :: k      !< Position.

   length = 0_I4P
   k = i
   do while (k <= len(line, kind=I4P))
      if (line(k:k) /= c) exit
      k = k + 1_I4P
   enddo
   if (k - i < 3_I4P) return
   if (len_trim_space(line(k:)) > 0) return
   length = k - i
   endfunction scan_close_fence

   pure function scan_setext_underline(line, i) result(level)
   !< A setext heading underline at `i` (CommonMark 4.3): `=`s (level 1) or `-`s (level 2), no space between them,
   !< then only spaces and tabs. Return the level, 0 for none.
   character(*), intent(in) :: line  !< Line.
   integer(I4P), intent(in) :: i     !< Position.
   integer(I4P)             :: level !< Level.
   character                :: c     !< Underline character.
   integer(I4P)             :: k     !< Position.

   level = 0_I4P
   if (i > len(line, kind=I4P)) return
   c = line(i:i)
   if (c /= '=' .and. c /= '-') return
   k = i
   do while (k <= len(line, kind=I4P))
      if (line(k:k) /= c) exit
      k = k + 1_I4P
   enddo
   if (len_trim_space(line(k:)) > 0) return
   level = merge(1_I4P, 2_I4P, c == '=')
   endfunction scan_setext_underline

   pure subroutine scan_list_marker(line, i, interrupts, ordered, marker, start, length)
   !< A list marker at `i` (CommonMark 5.2): `-`, `+` or `*`, or 1 to 9 digits and `.` or `)`, followed by a space, a
   !< tab or the end of the line. A marker that would interrupt a paragraph (`interrupts`) must be followed by
   !< something not blank and, when ordered, number 1. `length` is the marker's bytes, 0 for none.
   character(*), intent(in)  :: line       !< Line.
   integer(I4P), intent(in)  :: i          !< Position.
   logical,      intent(in)  :: interrupts !< The marker would interrupt a paragraph.
   logical,      intent(out) :: ordered    !< An ordered marker.
   integer(I4P), intent(out) :: marker     !< `iachar` of the bullet or of the delimiter.
   integer(I4P), intent(out) :: start      !< Number of an ordered marker.
   integer(I4P), intent(out) :: length     !< Bytes of the marker, 0 for none.
   integer(I4P)              :: k          !< Position.
   integer(I4P)              :: n          !< Line length.

   ordered = .false.
   marker = 0_I4P
   start = 0_I4P
   length = 0_I4P
   n = len(line, kind=I4P)
   if (i > n) return
   k = i
   select case(line(i:i))
   case('-', '+', '*')
      k = i + 1_I4P
   case('0':'9')
      do while (k <= n .and. k - i < 9_I4P)
         if (.not. is_ascii_digit(line(k:k))) exit
         start = 10_I4P * start + iachar(line(k:k), kind=I4P) - 48_I4P
         k = k + 1_I4P
      enddo
      if (k > n) return
      if (line(k:k) /= '.' .and. line(k:k) /= ')') return
      if (interrupts .and. start /= 1_I4P) return
      ordered = .true.
      k = k + 1_I4P
   case default
      return
   endselect
   if (k <= n) then
      if (.not. is_space_or_tab(line(k:k))) return
   endif
   if (interrupts .and. len_trim_space(line(k:)) == 0) return
   marker = iachar(line(k-1:k-1), kind=I4P)
   length = k - i
   endsubroutine scan_list_marker

   ! HTML blocks
   pure function scan_html_block_start(line, i, allow7) result(htype)
   !< The start condition of an HTML block at `i` (CommonMark 4.6): its type, 1 to 7, 0 for none. Type 7 (a lone
   !< complete tag) is tried only if `allow7` (it cannot interrupt a paragraph).
   character(*), intent(in) :: line   !< Line.
   integer(I4P), intent(in) :: i      !< Position.
   logical,      intent(in) :: allow7 !< Try type 7.
   integer(I4P)             :: htype  !< Type, 0 for none.
   character(:), allocatable :: name  !< Tag name, lower case.
   integer(I4P)             :: k      !< Position.
   integer(I4P)             :: n      !< Line length.
   integer(I4P)             :: t      !< Tag length.
   logical                  :: closing !< A `</` tag.

   htype = 0_I4P
   n = len(line, kind=I4P)
   if (i + 1_I4P > n) return
   if (line(i:i) /= '<') return
   ! 2, 3, 4, 5: comments, processing instructions, declarations, CDATA
   if (starts_with(line, i, '<!--')) then
      htype = 2_I4P
      return
   endif
   if (line(i+1:i+1) == '?') then
      htype = 3_I4P
      return
   endif
   if (starts_with(line, i, '<![CDATA[')) then
      htype = 5_I4P
      return
   endif
   if (line(i+1:i+1) == '!') then
      if (i + 2_I4P <= n) then
         if (is_ascii_letter(line(i+2:i+2))) htype = 4_I4P
      endif
      return
   endif
   ! 1 and 6: a tag name of their lists
   closing = line(i+1:i+1) == '/'
   k = i + 1_I4P
   if (closing) k = k + 1_I4P
   call tag_name(line, k, name)
   if (len(name) > 0) then
      k = k + len(name, kind=I4P)
      if (.not. closing .and. is_type1_name(name)) then
         if (k > n) then
            htype = 1_I4P
         elseif (is_space_or_tab(line(k:k)) .or. line(k:k) == '>') then
            htype = 1_I4P
         endif
         if (htype /= 0_I4P) return
      endif
      if (is_type6_name(name)) then
         if (k > n) then
            htype = 6_I4P
         elseif (is_space_or_tab(line(k:k)) .or. line(k:k) == '>') then
            htype = 6_I4P
         elseif (starts_with(line, k, '/>')) then
            htype = 6_I4P
         endif
         if (htype /= 0_I4P) return
      endif
   endif
   ! 7: a complete open or closing tag alone on the line, of any name but those of type 1
   if (.not. allow7) return
   if (closing) then
      t = scan_closing_tag(line(i:))
   else
      t = scan_open_tag(line(i:))
   endif
   if (t == 0_I4P) return
   k = i + 1_I4P
   if (closing) k = k + 1_I4P
   call tag_name(line, k, name)
   if (is_type1_name(name)) return
   if (len_trim_space(line(i+t:)) > 0) return
   htype = 7_I4P
   endfunction scan_html_block_start

   pure function html_block_end(line, i, htype) result(ends)
   !< True if `line(i:)` meets the end condition of an HTML block of type `htype` 1 to 5 (CommonMark 4.6): it contains
   !< `</script>`, `</pre>`, `</style>` or `</textarea>` (any case; type 1), `-->` (2), `?>` (3), `>` (4) or `]]>`
   !< (5). Types 6 and 7 end at a blank line, not here: false.
   character(*), intent(in) :: line  !< Line.
   integer(I4P), intent(in) :: i     !< Position.
   integer(I4P), intent(in) :: htype !< Type.
   logical                  :: ends  !< Result.
   character(:), allocatable :: name !< Tag name.
   integer(I4P)             :: k     !< Position.
   integer(I4P)             :: e     !< End of a name.

   ends = .false.
   if (i > len(line, kind=I4P)) return
   select case(htype)
   case(1_I4P)
      k = i
      do
         e = index(line(k:), '</', kind=I4P)
         if (e == 0_I4P) exit
         k = k + e + 1_I4P
         call tag_name(line, k, name)
         if (is_type1_name(name)) then
            e = k + len(name, kind=I4P)
            if (e <= len(line, kind=I4P)) then
               if (line(e:e) == '>') then
                  ends = .true.
                  return
               endif
            endif
         endif
      enddo
   case(2_I4P)
      ends = index(line(i:), '-->') > 0
   case(3_I4P)
      ends = index(line(i:), '?>') > 0
   case(4_I4P)
      ends = index(line(i:), '>') > 0
   case(5_I4P)
      ends = index(line(i:), ']]>') > 0
   endselect
   endfunction html_block_end

   pure function scan_open_tag(text) result(length)
   !< An open tag at the start of `text` (CommonMark 6.6): `<`, a tag name, attributes, optional white space, an
   !< optional `/`, `>`. Return its bytes, 0 for none.
   character(*), intent(in) :: text   !< Text, starting at `<`.
   integer(I4P)             :: length !< Bytes of the tag.
   integer(I4P)             :: k      !< Position.
   integer(I4P)             :: w      !< Position after white space.
   integer(I4P)             :: a      !< Position after an attribute.
   integer(I4P)             :: n      !< Length.

   length = 0_I4P
   n = len(text, kind=I4P)
   if (n < 3_I4P) return
   if (text(1:1) /= '<' .or. .not. is_ascii_letter(text(2:2))) return
   k = 3_I4P
   do while (k <= n)
      if (.not. is_tag_name_char(text(k:k))) exit
      k = k + 1_I4P
   enddo
   do ! attributes, each after white space
      w = skip_white(text, k)
      if (w == k) exit
      a = scan_attribute(text, w)
      if (a == w) exit
      k = a
   enddo
   k = skip_white(text, k)
   if (k > n) return
   if (text(k:k) == '/') k = k + 1_I4P
   if (k > n) return
   if (text(k:k) /= '>') return
   length = k
   endfunction scan_open_tag

   pure function scan_closing_tag(text) result(length)
   !< A closing tag at the start of `text` (CommonMark 6.6): `</`, a tag name, optional white space, `>`. Return its
   !< bytes, 0 for none.
   character(*), intent(in) :: text   !< Text, starting at `<`.
   integer(I4P)             :: length !< Bytes of the tag.
   integer(I4P)             :: k      !< Position.
   integer(I4P)             :: n      !< Length.

   length = 0_I4P
   n = len(text, kind=I4P)
   if (n < 4_I4P) return
   if (text(1:2) /= '</' .or. .not. is_ascii_letter(text(3:3))) return
   k = 4_I4P
   do while (k <= n)
      if (.not. is_tag_name_char(text(k:k))) exit
      k = k + 1_I4P
   enddo
   k = skip_white(text, k)
   if (k > n) return
   if (text(k:k) /= '>') return
   length = k
   endfunction scan_closing_tag

   ! private
   pure function scan_attribute(text, i) result(k)
   !< An attribute at `i`: a name, optionally white space, `=`, white space and a value. Return the position after it,
   !< `i` for none.
   character(*), intent(in) :: text !< Text.
   integer(I4P), intent(in) :: i    !< Position.
   integer(I4P)             :: k    !< Position after the attribute.
   integer(I4P)             :: e    !< Position after `=` and white space.
   integer(I4P)             :: v    !< Position in the value.
   integer(I4P)             :: n    !< Length.
   character                :: c    !< Character.
   character                :: q    !< Quote.

   k = i
   n = len(text, kind=I4P)
   if (i > n) return
   c = text(i:i)
   if (.not. (is_ascii_letter(c) .or. c == '_' .or. c == ':')) return
   k = i + 1_I4P
   do while (k <= n)
      c = text(k:k)
      if (.not. (is_ascii_letter(c) .or. is_ascii_digit(c) .or. c == '_' .or. c == '.' .or. c == ':' .or. &
                 c == '-')) exit
      k = k + 1_I4P
   enddo
   ! the value specification, if any
   e = skip_white(text, k)
   if (e > n) return
   if (text(e:e) /= '=') return
   e = skip_white(text, e + 1_I4P)
   if (e > n) return
   q = text(e:e)
   if (q == '"' .or. q == "'") then
      v = index(text(e+1:), q, kind=I4P)
      if (v == 0_I4P) return
      k = e + v + 1_I4P
   else
      v = e
      do while (v <= n)
         c = text(v:v)
         if (c == ' ' .or. c == TAB .or. c == LF .or. c == CR .or. c == '"' .or. c == "'" .or. c == '=' .or. &
             c == '<' .or. c == '>' .or. c == '`') exit
         v = v + 1_I4P
      enddo
      if (v == e) return
      k = v
   endif
   endfunction scan_attribute

   pure function skip_white(text, i) result(k)
   !< Return the first position at or after `i` that is not a space, a tab or a line ending.
   character(*), intent(in) :: text !< Text.
   integer(I4P), intent(in) :: i    !< Position.
   integer(I4P)             :: k    !< Position.

   k = i
   do while (k <= len(text, kind=I4P))
      if (text(k:k) /= ' ' .and. text(k:k) /= TAB .and. text(k:k) /= LF .and. text(k:k) /= CR) exit
      k = k + 1_I4P
   enddo
   endfunction skip_white

   elemental function is_tag_name_char(c) result(is)
   !< True for a character of a tag name after its first: an ASCII letter, a digit or `-`.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.

   is = is_ascii_letter(c) .or. is_ascii_digit(c) .or. c == '-'
   endfunction is_tag_name_char

   pure subroutine tag_name(line, k, name)
   !< The tag name at `k` (an ASCII letter, then letters, digits and `-`), in lower case; empty if none.
   character(*),              intent(in)  :: line !< Line.
   integer(I4P),              intent(in)  :: k    !< Position.
   character(:), allocatable, intent(out) :: name !< Name.
   integer(I4P)                           :: e    !< Position after the name.

   name = ''
   if (k > len(line, kind=I4P)) return
   if (.not. is_ascii_letter(line(k:k))) return
   e = k + 1_I4P
   do while (e <= len(line, kind=I4P))
      if (.not. is_tag_name_char(line(e:e))) exit
      e = e + 1_I4P
   enddo
   name = lower_ascii_string(line(k:e-1))
   endsubroutine tag_name

   pure function lower_ascii_string(s) result(l)
   !< A string in ASCII lower case.
   character(*), intent(in) :: s !< String.
   character(len(s))        :: l !< Result.
   integer                  :: k !< Counter.

   do k=1, len(s)
      l(k:k) = lower_ascii(s(k:k))
   enddo
   endfunction lower_ascii_string

   pure function is_type1_name(name) result(is)
   !< True for the tag names of HTML block type 1.
   character(*), intent(in) :: name !< Name, lower case.
   logical                  :: is   !< Result.

   select case(name)
   case('pre', 'script', 'style', 'textarea')
      is = .true.
   case default
      is = .false.
   endselect
   endfunction is_type1_name

   pure function is_type6_name(name) result(is)
   !< True for the tag names of HTML block type 6 (CommonMark 0.31.2, 4.6).
   character(*), intent(in) :: name !< Name, lower case.
   logical                  :: is   !< Result.

   select case(name)
   case('address', 'article', 'aside', 'base', 'basefont', 'blockquote', 'body', 'caption', 'center', 'col', &
        'colgroup', 'dd', 'details', 'dialog', 'dir', 'div', 'dl', 'dt', 'fieldset', 'figcaption', 'figure', 'footer', &
        'form', 'frame', 'frameset', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'head', 'header', 'hr', 'html', 'iframe', &
        'legend', 'li', 'link', 'main', 'menu', 'menuitem', 'nav', 'noframes', 'ol', 'optgroup', 'option', 'p', &
        'param', 'search', 'section', 'summary', 'table', 'tbody', 'td', 'tfoot', 'th', 'thead', 'title', 'tr', &
        'track', 'ul')
      is = .true.
   case default
      is = .false.
   endselect
   endfunction is_type6_name

   pure function starts_with(line, i, prefix) result(is)
   !< True if `line(i:)` starts with `prefix` (exact bytes).
   character(*), intent(in) :: line   !< Line.
   integer(I4P), intent(in) :: i      !< Position.
   character(*), intent(in) :: prefix !< Prefix.
   logical                  :: is     !< Result.

   is = .false.
   if (i + len(prefix, kind=I4P) - 1_I4P <= len(line, kind=I4P)) is = line(i:i+len(prefix)-1) == prefix
   endfunction starts_with

   pure function len_trim_space(s) result(n)
   !< The length of `s` without its trailing spaces and tabs: 0 if it holds nothing else.
   character(*), intent(in) :: s !< Text.
   integer(I4P)             :: n !< Length.

   n = len(s, kind=I4P)
   do while (n > 0_I4P)
      if (.not. is_space_or_tab(s(n:n))) exit
      n = n - 1_I4P
   enddo
   endfunction len_trim_space
endmodule baruch_blocks_scan
