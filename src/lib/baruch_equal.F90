!< BARUCH, tree equality: two documents compared by their meaning, spans and recorded style ignored.
submodule (baruch_document) baruch_equal
!< BARUCH, tree equality: two documents compared by their meaning, spans and recorded style ignored.
!<
!< `markdown_document%equal` walks the two trees in lockstep, through their links (no recursion, no stack: a 10^5-deep
!< nesting compares like a flat list), and stops at the first node that differs. What it compares, per kind:
!<
!< - the **kind**: a core kind by its number, an extension kind by its registered name (two documents may number the
!<   same extension's kinds differently);
!< - the **structure**: the children, in order. A run of adjacent text nodes is one text, its literals concatenated; a
!<   run whose text is empty is no node at all (how a tree was split into text nodes is not its meaning);
!< - the **processed text**: the literal of text, code, inline HTML, HTML blocks and code blocks; a code block's info
!<   string; the destination and the title of links, images and link reference definitions; a definition's label,
!<   normalised as matching normalises it (`[Foo]` and `[foo]` define the same thing);
!< - the **semantic integer slots**: a heading's level; a list's type, tightness and, for an ordered list, its start;
!<   every integer and string slot of an extension kind (the document's registry does not say which hold style, so
!<   all are meaning);
!< - the **attributes**: the same names with the same values, in any order (a text run: its first node's).
!<
!< Ignored: byte spans, the style slots (emphasis character, bullet and delimiter, heading form and closing `#`s,
!< fence character and length, code block form, thematic break character, link form and label), line endings inside
!< strings (CRLF, CR and LF are one line ending), tombstones and detached nodes (they are not in the tree).
!<
!< On a difference the path of the first differing node is built from the root: one step per node, its kind name,
!< with `[k]` (its position among the siblings of its kind) when its parent has more than one child of that kind:
!< `/document/list[2]/item[1]/paragraph/emph`. The path is in the first document, or in the second when the node is
!< only there.
use baruch_references, only : normalize_label
implicit none

character,    parameter :: LF        = achar(10) !< Line feed.
character,    parameter :: CR        = achar(13) !< Carriage return.
character,    parameter :: BSLASH    = achar(92) !< Backslash (a literal one is an escape for some compilers' defaults).
integer(I4P), parameter :: QUOTE_MAX = 40_I4P    !< Bytes of a value quoted in a difference.

contains
   module function equal(self, other, path, difference) result(is)
   !< Return true if two documents have the same meaning; on a difference, the path of the first differing node and
   !< what differs there.
   class(markdown_document),  intent(in)            :: self       !< Document.
   class(markdown_document),  intent(in)            :: other      !< The other document.
   character(:), allocatable, intent(out), optional :: path       !< Path of the first differing node, empty if equal.
   character(:), allocatable, intent(out), optional :: difference !< What differs there, empty if equal.
   logical                                          :: is         !< Result.
   character(:), allocatable                        :: what       !< What differs.
   integer(I4P)                                     :: ra         !< Root of the first document.
   integer(I4P)                                     :: rb         !< Root of the second document.
   integer(I4P)                                     :: x          !< Current node of the first document.
   integer(I4P)                                     :: y          !< Current node of the second document.
   integer(I4P)                                     :: cx         !< Next node of the first document.
   integer(I4P)                                     :: cy         !< Next node of the second document.

   is = .true.
   if (present(path)) path = ''
   if (present(difference)) difference = ''
   ra = self%root()
   rb = other%root()
   if (ra == 0_I4P .and. rb == 0_I4P) return
   if (rb == 0_I4P) then
      call differ(self, ra, 'only in the first document (the second is empty)')
      return
   elseif (ra == 0_I4P) then
      call differ(other, rb, 'only in the second document (the first is empty)')
      return
   endif
   x = ra
   y = rb
   what = compare_nodes(self, other, x, y)
   if (len(what) > 0) then
      call differ(self, x, what)
      return
   endif
   walk: do
      cx = first_logical(self, x)
      cy = first_logical(other, y)
      if (cx == 0_I4P .and. cy == 0_I4P) then ! no children: the next sibling of the node or of its nearest ancestor
         do
            if (x == ra) exit walk
            cx = next_logical(self, x)
            cy = next_logical(other, y)
            if (cx /= 0_I4P .or. cy /= 0_I4P) exit
            x = self%parent_(x)
            y = other%parent_(y)
         enddo
      endif
      if (cy == 0_I4P) then
         call differ(self, cx, 'only in the first document')
         return
      elseif (cx == 0_I4P) then
         call differ(other, cy, 'only in the second document')
         return
      endif
      x = cx
      y = cy
      what = compare_nodes(self, other, x, y)
      if (len(what) > 0) then
         call differ(self, x, what)
         return
      endif
   enddo walk

   contains
      subroutine differ(doc, n, why)
      !< Report a difference at a node of one of the documents.
      class(markdown_document), intent(in) :: doc  !< Document holding the node.
      integer(I4P),             intent(in) :: n    !< Node.
      character(*),             intent(in) :: why  !< What differs.
      character(:), allocatable            :: here !< Its path.

      is = .false.
      if (.not. (present(path) .or. present(difference))) return
      here = node_path(doc, n)
      if (present(path)) path = here
      if (present(difference)) difference = here//': '//why
      endsubroutine differ
   endfunction equal

   ! private
   function compare_nodes(a, b, x, y) result(what)
   !< Compare two nodes, not their children: their kinds, the semantic fields of the kind, their attributes. Return
   !< what differs, empty if nothing.
   class(markdown_document), intent(in) :: a    !< First document.
   class(markdown_document), intent(in) :: b    !< Second document.
   integer(I4P),             intent(in) :: x    !< Node of the first document.
   integer(I4P),             intent(in) :: y    !< Node of the second document.
   character(:), allocatable            :: what !< What differs, empty if nothing.
   integer(I4P)                         :: k    !< The kind.
   integer(I4P)                         :: s    !< Slot.
   logical                              :: ordered !< An ordered list.

   what = ''
   k = a%kind_(x)
   if (k >= NODE_EXTENSION_FIRST .or. b%kind_(y) >= NODE_EXTENSION_FIRST) then
      if ((k >= NODE_EXTENSION_FIRST .neqv. b%kind_(y) >= NODE_EXTENSION_FIRST) .or. &
          a%kind_name(k) /= b%kind_name(b%kind_(y)) .or.                             &
          len(a%kind_name(k)) /= len(b%kind_name(b%kind_(y)))) then
         what = 'kind '//a%kind_name(k)//' vs '//b%kind_name(b%kind_(y))
         return
      endif
   elseif (k /= b%kind_(y)) then
      what = 'kind '//a%kind_name(k)//' vs '//b%kind_name(b%kind_(y))
      return
   endif
   select case(k)
   case(NODE_TEXT)
      what = compare_text('literal', run_text(a, x), run_text(b, y))
   case(NODE_CODE, NODE_HTML_INLINE, NODE_HTML_BLOCK)
      what = compare_strings(a, b, x, y, STRING_LITERAL, 'literal')
   case(NODE_CODE_BLOCK)
      what = compare_strings(a, b, x, y, STRING_LITERAL, 'literal')
      if (len(what) == 0) what = compare_strings(a, b, x, y, STRING_INFO, 'info string')
   case(NODE_HEADING)
      if (a%ints_(SLOT_HEADING_LEVEL, x) /= b%ints_(SLOT_HEADING_LEVEL, y)) &
         what = 'heading level '//istr(a%ints_(SLOT_HEADING_LEVEL, x))//' vs '//istr(b%ints_(SLOT_HEADING_LEVEL, y))
   case(NODE_LIST)
      ordered = a%ints_(SLOT_LIST_TYPE, x) == LIST_ORDERED
      if (a%ints_(SLOT_LIST_TYPE, x) /= b%ints_(SLOT_LIST_TYPE, y)) then
         what = 'list type '//list_type_name(a%ints_(SLOT_LIST_TYPE, x))//' vs '// &
                list_type_name(b%ints_(SLOT_LIST_TYPE, y))
      elseif (ordered .and. a%ints_(SLOT_LIST_START, x) /= b%ints_(SLOT_LIST_START, y)) then
         what = 'list start '//istr(a%ints_(SLOT_LIST_START, x))//' vs '//istr(b%ints_(SLOT_LIST_START, y))
      elseif ((a%ints_(SLOT_LIST_TIGHT, x) /= 0_I4P) .neqv. (b%ints_(SLOT_LIST_TIGHT, y) /= 0_I4P)) then
         what = 'list '//tightness(a%ints_(SLOT_LIST_TIGHT, x))//' vs '//tightness(b%ints_(SLOT_LIST_TIGHT, y))
      endif
   case(NODE_LINK, NODE_IMAGE)
      what = compare_strings(a, b, x, y, STRING_DESTINATION, 'destination')
      if (len(what) == 0) what = compare_strings(a, b, x, y, STRING_TITLE, 'title')
   case(NODE_LINK_REFERENCE_DEFINITION)
      what = compare_text('label', normalize_label(a%get_string(a%strs_(STRING_LABEL, x))), &
                          normalize_label(b%get_string(b%strs_(STRING_LABEL, y))))
      if (len(what) == 0) what = compare_strings(a, b, x, y, STRING_DESTINATION, 'destination')
      if (len(what) == 0) what = compare_strings(a, b, x, y, STRING_TITLE, 'title')
   case default
      if (k >= NODE_EXTENSION_FIRST) then
         do s=1, NODE_INT_SLOTS
            if (a%ints_(s, x) /= b%ints_(s, y)) then
               what = 'integer slot '//istr(s)//' '//istr(a%ints_(s, x))//' vs '//istr(b%ints_(s, y))
               exit
            endif
         enddo
         if (len(what) == 0) then
            do s=1, NODE_STRING_SLOTS
               what = compare_strings(a, b, x, y, s, 'string slot '//istr(s))
               if (len(what) > 0) exit
            enddo
         endif
      endif
   endselect
   if (len(what) == 0) what = compare_attributes(a, b, x, y)
   endfunction compare_nodes

   function compare_strings(a, b, x, y, slot, field) result(what)
   !< Compare a string slot of two nodes, line endings aside; return what differs, empty if nothing.
   class(markdown_document), intent(in) :: a     !< First document.
   class(markdown_document), intent(in) :: b     !< Second document.
   integer(I4P),             intent(in) :: x     !< Node of the first document.
   integer(I4P),             intent(in) :: y     !< Node of the second document.
   integer(I4P),             intent(in) :: slot  !< String slot.
   character(*),             intent(in) :: field !< Its name, for the difference.
   character(:), allocatable            :: what  !< What differs, empty if nothing.

   what = compare_text(field, a%get_string(a%strs_(slot, x)), b%get_string(b%strs_(slot, y)))
   endfunction compare_strings

   function compare_attributes(a, b, x, y) result(what)
   !< Compare the attributes of two nodes as sets: the same names with the same values, in any order.
   class(markdown_document), intent(in) :: a     !< First document.
   class(markdown_document), intent(in) :: b     !< Second document.
   integer(I4P),             intent(in) :: x     !< Node of the first document.
   integer(I4P),             intent(in) :: y     !< Node of the second document.
   character(:), allocatable            :: what  !< What differs, empty if nothing.
   character(:), allocatable            :: name  !< An attribute's name.
   character(:), allocatable            :: value !< Its value in the other document.
   integer(I4P)                         :: r     !< Attribute record.
   logical                              :: found !< The other node has it.

   what = ''
   r = a%attrs_(x)
   do while (r /= 0_I4P)
      name = a%get_string(a%attr_name_(r))
      value = b%attribute(y, name, found)
      if (.not. found) then
         what = 'attribute "'//quote(name)//'" only in the first document'
         return
      endif
      what = compare_text('attribute "'//quote(name)//'"', a%get_string(a%attr_value_(r)), value)
      if (len(what) > 0) return
      r = a%attr_next_(r)
   enddo
   r = b%attrs_(y)
   do while (r /= 0_I4P)
      name = b%get_string(b%attr_name_(r))
      if (.not. a%has_attribute(x, name)) then
         what = 'attribute "'//quote(name)//'" only in the second document'
         return
      endif
      r = b%attr_next_(r)
   enddo
   endfunction compare_attributes

   function compare_text(field, u, v) result(what)
   !< Compare two strings, CRLF, CR and LF being one line ending; return what differs, empty if nothing.
   character(*), intent(in)  :: field !< What the strings are, for the difference.
   character(*), intent(in)  :: u     !< First string.
   character(*), intent(in)  :: v     !< Second string.
   character(:), allocatable :: what  !< What differs, empty if nothing.

   what = ''
   if (.not. same_text(u, v)) what = field//' "'//quote(u)//'" vs "'//quote(v)//'"'
   endfunction compare_text

   pure function same_text(u, v) result(same)
   !< True if two strings are equal, CRLF, CR and LF being one line ending.
   character(*), intent(in) :: u    !< First string.
   character(*), intent(in) :: v    !< Second string.
   logical                  :: same !< Result.
   integer(I4P)             :: i    !< Position in `u`.
   integer(I4P)             :: j    !< Position in `v`.
   character                :: cu   !< Character of `u`, its line ending as LF.
   character                :: cv   !< Character of `v`, its line ending as LF.

   i = 1_I4P
   j = 1_I4P
   same = .false.
   do while (i <= len(u, kind=I4P) .and. j <= len(v, kind=I4P))
      cu = u(i:i)
      if (cu == CR) then
         cu = LF
         if (i < len(u, kind=I4P)) then
            if (u(i+1:i+1) == LF) i = i + 1_I4P
         endif
      endif
      cv = v(j:j)
      if (cv == CR) then
         cv = LF
         if (j < len(v, kind=I4P)) then
            if (v(j+1:j+1) == LF) j = j + 1_I4P
         endif
      endif
      if (cu /= cv) return
      i = i + 1_I4P
      j = j + 1_I4P
   enddo
   same = i > len(u, kind=I4P) .and. j > len(v, kind=I4P)
   endfunction same_text

   function run_text(doc, n) result(text)
   !< Return the text of the run of adjacent text nodes starting at a node: their literals concatenated.
   class(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),             intent(in) :: n     !< First node of the run.
   character(:), allocatable            :: text  !< The run's text.
   integer(I4P)                         :: s     !< Node of the run.
   integer(I4P)                         :: bytes !< Bytes so far.
   integer(I4P)                         :: b     !< Bytes of a node.

   bytes = 0_I4P
   s = n
   do while (s /= 0_I4P)
      if (doc%kind_(s) /= NODE_TEXT) exit
      bytes = bytes + literal_bytes(doc, s)
      s = doc%next_(s)
   enddo
   allocate(character(bytes) :: text)
   bytes = 0_I4P
   s = n
   do while (s /= 0_I4P)
      if (doc%kind_(s) /= NODE_TEXT) exit
      b = literal_bytes(doc, s)
      if (b > 0_I4P) text(bytes+1:bytes+b) = doc%get_string(doc%strs_(STRING_LITERAL, s))
      bytes = bytes + b
      s = doc%next_(s)
   enddo
   endfunction run_text

   pure function literal_bytes(doc, n) result(bytes)
   !< Return the bytes of a node's literal.
   class(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),             intent(in) :: n     !< Node.
   integer(I4P)                         :: bytes !< Bytes.
   integer(I4P)                         :: id    !< String id.

   bytes = 0_I4P
   id = doc%strs_(STRING_LITERAL, n)
   if (id > 0_I4P) bytes = doc%string_len_(id)
   endfunction literal_bytes

   function first_logical(doc, n) result(c)
   !< Return the first child of a node as equality sees it: a run of empty texts skipped; 0 if none.
   class(markdown_document), intent(in) :: doc !< Document.
   integer(I4P),             intent(in) :: n   !< Node.
   integer(I4P)                         :: c   !< Child.

   c = skip_empty_runs(doc, doc%first_(n))
   endfunction first_logical

   function next_logical(doc, n) result(c)
   !< Return the next sibling of a node as equality sees it: after the whole run if the node starts a run of texts,
   !< a run of empty texts skipped; 0 if none.
   class(markdown_document), intent(in) :: doc !< Document.
   integer(I4P),             intent(in) :: n   !< Node.
   integer(I4P)                         :: c   !< Sibling.

   c = n
   if (doc%kind_(c) == NODE_TEXT) then
      do while (doc%next_(c) /= 0_I4P)
         if (doc%kind_(doc%next_(c)) /= NODE_TEXT) exit
         c = doc%next_(c)
      enddo
   endif
   c = skip_empty_runs(doc, doc%next_(c))
   endfunction next_logical

   function skip_empty_runs(doc, n) result(c)
   !< Return the node itself, or, if it starts a run of texts all empty, the node after the run (0 if none).
   class(markdown_document), intent(in) :: doc   !< Document.
   integer(I4P),             intent(in) :: n     !< Node (0: none).
   integer(I4P)                         :: c     !< Node.
   integer(I4P)                         :: s     !< Node of the run.

   c = n
   if (c == 0_I4P) return
   if (doc%kind_(c) /= NODE_TEXT) return
   s = c
   do while (s /= 0_I4P)
      if (doc%kind_(s) /= NODE_TEXT) exit
      if (literal_bytes(doc, s) > 0_I4P) return
      s = doc%next_(s)
   enddo
   c = s ! a run of empty texts is followed by a node that is not a text, or by nothing
   endfunction skip_empty_runs

   function node_path(doc, n) result(path)
   !< Return the path of a node from the root: `/document/list[2]/item/paragraph`, a step per node, its kind name and,
   !< when its parent has more than one child of its kind, its position among them.
   class(markdown_document), intent(in) :: doc     !< Document.
   integer(I4P),             intent(in) :: n       !< Node.
   character(:), allocatable            :: path    !< Path.
   integer(I4P), allocatable            :: chain(:) !< The node and its ancestors, the node first.
   integer(I4P)                         :: depth   !< Nodes in the chain.
   integer(I4P)                         :: m       !< Node of the chain.
   integer(I4P)                         :: s       !< Sibling.
   integer(I4P)                         :: k       !< Counter.
   integer(I4P)                         :: position !< Position among the siblings of its kind.
   integer(I4P)                         :: total   !< Siblings of its kind.
   integer(I4P)                         :: used    !< Bytes of the path.
   character(:), allocatable            :: step    !< One step.

   depth = 0_I4P
   m = n
   do while (m /= 0_I4P)
      depth = depth + 1_I4P
      m = doc%parent_(m)
   enddo
   allocate(chain(depth))
   m = n
   do k=1, depth
      chain(k) = m
      m = doc%parent_(m)
   enddo
   allocate(character(64) :: path)
   used = 0_I4P
   do k=depth, 1_I4P, -1_I4P
      m = chain(k)
      step = '/'//doc%kind_name(doc%kind_(m))
      if (doc%parent_(m) /= 0_I4P) then
         position = 0_I4P
         total = 0_I4P
         s = first_logical(doc, doc%parent_(m))
         do while (s /= 0_I4P)
            if (doc%kind_(s) == doc%kind_(m)) then
               total = total + 1_I4P
               if (s == m) position = total
            endif
            s = next_logical(doc, s)
         enddo
         if (total > 1_I4P) step = step//'['//istr(position)//']'
      endif
      call append(step)
   enddo
   path = path(1:used)

   contains
      subroutine append(piece)
      !< Append a piece to the path, its buffer doubled when full.
      character(*), intent(in)  :: piece !< Piece.
      character(:), allocatable :: grown !< Grown buffer.

      if (used + len(piece, kind=I4P) > len(path, kind=I4P)) then
         allocate(character(2_I4P * (used + len(piece, kind=I4P))) :: grown)
         grown(1:used) = path(1:used)
         call move_alloc(grown, path)
      endif
      path(used+1:used+len(piece)) = piece
      used = used + len(piece, kind=I4P)
      endsubroutine append
   endfunction node_path

   pure function quote(text) result(quoted)
   !< Return a value as quoted in a difference: at most `QUOTE_MAX` bytes (then `...`), line endings as `\n`, `\r`.
   character(*), intent(in)  :: text   !< Value.
   character(:), allocatable :: quoted !< Quoted.
   integer(I4P)              :: i      !< Position.

   quoted = ''
   do i=1, min(len(text, kind=I4P), QUOTE_MAX)
      select case(text(i:i))
      case(LF)   ; quoted = quoted//BSLASH//'n'
      case(CR)   ; quoted = quoted//BSLASH//'r'
      case default ; quoted = quoted//text(i:i)
      endselect
   enddo
   if (len(text, kind=I4P) > QUOTE_MAX) quoted = quoted//'...'
   endfunction quote

   pure function list_type_name(ltype) result(name)
   !< Return the name of a list type.
   integer(I4P), intent(in)  :: ltype !< List type.
   character(:), allocatable :: name  !< Name.

   select case(ltype)
   case(LIST_BULLET)  ; name = 'bullet'
   case(LIST_ORDERED) ; name = 'ordered'
   case default       ; name = istr(ltype)
   endselect
   endfunction list_type_name

   pure function tightness(tight) result(name)
   !< Return "tight" or "loose" for a list's tight slot.
   integer(I4P), intent(in)  :: tight !< Tight slot.
   character(:), allocatable :: name  !< Name.

   if (tight /= 0_I4P) then
      name = 'tight'
   else
      name = 'loose'
   endif
   endfunction tightness
endsubmodule baruch_equal
