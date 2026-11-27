!< BARUCH, the link reference definitions of a document as a map from normalised label to definition (CommonMark 4.7).
module baruch_references
!< BARUCH, the link reference definitions of a document as a map from normalised label to definition (CommonMark 4.7).
!<
!< The block parser leaves every link reference definition in the tree, a `link_reference_definition` node in place
!< holding its label as written, its destination and its title (`baruch_blocks_definitions`). Before the inline phase
!< the inline parser (`baruch_inlines`) builds a `reference_map` of them, once per parse; a full, collapsed or shortcut
!< reference looks its label up in it.
!<
!< - **Normalisation** (spec 4.7): spaces, tabs and line endings stripped at both ends, every internal run of them
!<   collapsed to one space, then the Unicode full case folding (StringiFor `casefold()`: `ẞ` and `SS` both fold to
!<   `ss`). Backslash escapes are not processed: `[foo\!]` does not match `[foo!]`.
!< - **The first definition wins**: the definitions are collected in document order (the tree's depth-first order,
!<   walked through its links with no recursion), and a label already in the map is not replaced.
!< - **A hash table** with open addressing (FNV-1a of the normalised bytes, linear probing, at most half full):
!<   building is linear in the bytes of the labels, a lookup costs the normalisation of its label (at most 999
!<   characters, longer labels never match) plus O(1) probes on average.
use penf,                only : I4P, I8P
use stringifor,          only : string
use baruch_document,     only : markdown_document
use baruch_kinds_nodes,  only : NODE_LINK_REFERENCE_DEFINITION
use baruch_blocks_definitions, only : LABEL_MAX
implicit none
private
public :: reference_map, normalize_label

integer(I8P), parameter :: FNV_OFFSET = 2166136261_I8P      !< FNV-1a 32-bit offset basis.
integer(I8P), parameter :: FNV_PRIME = 16777619_I8P         !< FNV-1a 32-bit prime.
integer(I8P), parameter :: TWO_32 = 4294967296_I8P          !< 2^32.
character,    parameter :: TAB = achar(9)                   !< Tab.
character,    parameter :: LF  = achar(10)                  !< Line feed.
character,    parameter :: CR  = achar(13)                  !< Carriage return.

type :: reference_map
   !< The link reference definitions of one document, by normalised label.
   integer(I4P)              :: entries = 0_I4P !< Definitions held (one per distinct label).
   character(:), allocatable :: keys            !< The normalised labels, concatenated.
   integer(I4P), allocatable :: key_first(:)    !< First byte of each entry's label in `keys`.
   integer(I4P), allocatable :: key_last(:)     !< Last byte.
   integer(I4P), allocatable :: definition(:)   !< Each entry's definition node.
   integer(I4P), allocatable :: slots(:)        !< The hash table: an entry, 0 for an empty slot (size a power of 2).
   contains
      procedure, pass(self) :: build  !< Collect the definitions of a document.
      procedure, pass(self) :: lookup !< The definition a label refers to, 0 if none.
endtype reference_map

contains
   subroutine build(self, doc)
   !< Collect the link reference definitions of a document, in document order; the first definition of a label wins.
   class(reference_map),    intent(inout) :: self  !< Map.
   type(markdown_document), intent(in)    :: doc   !< Document, its blocks parsed.
   integer(I4P)                           :: total !< Definitions in the document.
   integer(I4P)                           :: size_ !< Slots of the table.
   integer(I4P)                           :: n     !< Node.
   integer(I4P)                           :: used  !< Bytes of `keys` in use.
   character(:), allocatable              :: key   !< A normalised label.
   integer(I4P)                           :: s     !< Slot.
   logical                                :: found !< The label is in the map already.

   self%entries = 0_I4P
   total = 0_I4P
   do n=1, doc%last_node()
      if (.not. doc%is_node(n)) cycle
      if (doc%node_kind(n) == NODE_LINK_REFERENCE_DEFINITION) total = total + 1_I4P
   enddo
   size_ = 16_I4P
   do while (size_ < 2_I4P * total)
      size_ = 2_I4P * size_
   enddo
   if (allocated(self%slots)) deallocate(self%slots)
   if (allocated(self%key_first)) deallocate(self%key_first)
   if (allocated(self%key_last)) deallocate(self%key_last)
   if (allocated(self%definition)) deallocate(self%definition)
   if (allocated(self%keys)) deallocate(self%keys)
   allocate(self%slots(0:size_-1), self%key_first(max(total, 1_I4P)), self%key_last(max(total, 1_I4P)), &
            self%definition(max(total, 1_I4P)))
   self%slots = 0_I4P
   allocate(character(256) :: self%keys)
   used = 0_I4P
   if (total == 0_I4P) return
   ! the document order: from a node to its first child, else to the next sibling of it or of its nearest ancestor
   n = doc%root()
   do while (n /= 0_I4P)
      if (doc%node_kind(n) == NODE_LINK_REFERENCE_DEFINITION) then
         key = normalize_label(doc%label(n))
         if (len(key) > 0) then
            call probe(self, key, s, found)
            if (.not. found) call add_entry(self, key, n, s, used)
         endif
      endif
      n = next_in_order(doc, n)
   enddo
   endsubroutine build

   function lookup(self, label) result(definition)
   !< Return the definition a label (as written, without its brackets) refers to, 0 if none.
   class(reference_map), intent(in) :: self       !< Map.
   character(*),         intent(in) :: label      !< Label as written.
   integer(I4P)                     :: definition !< Definition node, 0 if none.
   character(:), allocatable        :: key        !< Normalised label.
   integer(I4P)                     :: s          !< Slot.
   logical                          :: found      !< Found.

   definition = 0_I4P
   if (self%entries == 0_I4P) return
   if (len(label) > LABEL_MAX) return
   key = normalize_label(label)
   if (len(key) == 0) return
   call probe(self, key, s, found)
   if (found) definition = self%definition(self%slots(s))
   endfunction lookup

   function normalize_label(label) result(key)
   !< Return the normalised form of a link label: spaces, tabs and line endings stripped at both ends, internal runs of
   !< them collapsed to one space, then the Unicode full case folding. Empty for a label of white space only.
   character(*), intent(in)  :: label !< Label as written, without its brackets.
   character(:), allocatable :: key   !< Normalised label.
   character(len(label))     :: buf   !< White space collapsed.
   type(string)              :: s     !< For the case folding.
   integer(I4P)              :: i     !< Position in the label.
   integer(I4P)              :: j     !< Bytes written.
   logical                   :: gap   !< White space seen since the last character written.

   j = 0_I4P
   gap = .false.
   do i=1, len(label, kind=I4P)
      if (is_label_space(label(i:i))) then
         gap = .true.
      else
         if (gap .and. j > 0_I4P) then
            j = j + 1_I4P
            buf(j:j) = ' '
         endif
         gap = .false.
         j = j + 1_I4P
         buf(j:j) = label(i:i)
      endif
   enddo
   if (j == 0_I4P) then
      key = ''
      return
   endif
   s = buf(1:j)
   s = s%casefold()
   key = s%chars()
   endfunction normalize_label

   ! private
   subroutine probe(self, key, s, found)
   !< The slot of a key: where it is (`found`), or the empty slot where it would go.
   type(reference_map), intent(in)  :: self  !< Map.
   character(*),        intent(in)  :: key   !< Normalised label.
   integer(I4P),        intent(out) :: s     !< Slot.
   logical,             intent(out) :: found !< The key is in the map.
   integer(I4P)                     :: mask  !< Slots minus 1.
   integer(I4P)                     :: e     !< Entry in the slot.

   found = .false.
   mask = size(self%slots, kind=I4P) - 1_I4P
   s = iand(hash(key), mask)
   do
      e = self%slots(s)
      if (e == 0_I4P) return
      if (self%keys(self%key_first(e):self%key_last(e)) == key .and. &
          self%key_last(e) - self%key_first(e) + 1_I4P == len(key, kind=I4P)) then
         found = .true.
         return
      endif
      s = iand(s + 1_I4P, mask)
   enddo
   endsubroutine probe

   subroutine add_entry(self, key, definition, s, used)
   !< Add an entry in the empty slot `s`.
   type(reference_map), intent(inout) :: self       !< Map.
   character(*),        intent(in)    :: key        !< Normalised label.
   integer(I4P),        intent(in)    :: definition !< Definition node.
   integer(I4P),        intent(in)    :: s          !< Empty slot.
   integer(I4P),        intent(inout) :: used       !< Bytes of `keys` in use.
   character(:), allocatable          :: tmp        !< Grown buffer.
   integer(I4P)                       :: need       !< Bytes needed.

   need = used + len(key, kind=I4P)
   if (need > len(self%keys, kind=I4P)) then ! grown by assignment, not move_alloc (nvfortran 26.9, szaghi/BARUCH#2 B1)
      allocate(character(max(need, 2_I4P * len(self%keys, kind=I4P))) :: tmp)
      tmp(1:used) = self%keys(1:used)
      self%keys = tmp
   endif
   self%keys(used+1:need) = key
   self%entries = self%entries + 1_I4P
   self%key_first(self%entries) = used + 1_I4P
   self%key_last(self%entries) = need
   self%definition(self%entries) = definition
   self%slots(s) = self%entries
   used = need
   endsubroutine add_entry

   pure function hash(key) result(h)
   !< The 32-bit FNV-1a hash of the bytes of a key, its high bits folded into the low ones (the table takes the low
   !< bits). Computed in 64 bits modulo 2^32: no overflow.
   character(*), intent(in) :: key !< Key.
   integer(I4P)             :: h   !< Hash, 0 to 2^31 - 1.
   integer(I8P)             :: a   !< Accumulator, 0 to 2^32 - 1.
   integer(I4P)             :: i   !< Position.

   a = FNV_OFFSET
   do i=1, len(key, kind=I4P)
      a = mod(ieor(a, int(iachar(key(i:i)), I8P)) * FNV_PRIME, TWO_32)
   enddo
   a = ieor(a, ishft(a, -15))
   h = int(iand(a, int(huge(1_I4P), I8P)), I4P)
   endfunction hash

   function next_in_order(doc, n) result(next)
   !< The node after `n` in document order, 0 after the last.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: n    !< Node.
   integer(I4P)                        :: next !< Next node.
   integer(I4P)                        :: c    !< Current node.

   next = doc%first_child(n)
   c = n
   do while (next == 0_I4P .and. c /= 0_I4P)
      next = doc%next_sibling(c)
      if (next == 0_I4P) c = doc%parent(c)
   enddo
   endfunction next_in_order

   elemental function is_label_space(c) result(is)
   !< True for the white space of a label: space, tab, line feed, carriage return.
   character, intent(in) :: c  !< Character.
   logical               :: is !< Result.

   is = c == ' ' .or. c == TAB .or. c == LF .or. c == CR
   endfunction is_label_space
endmodule baruch_references
