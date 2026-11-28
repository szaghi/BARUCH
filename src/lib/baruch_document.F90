!< BARUCH, the document: flat node arrays, a string pool, typed accessors.
module baruch_document
!< BARUCH, the document: flat node arrays, a string pool, typed accessors.
!<
!< A node is an integer index into its document. Each node has a kind, five links (parent, first and last child,
!< previous and next sibling: 0 is "none"), a byte span into the document's source, `NODE_INT_SLOTS` integer slots,
!< `NODE_STRING_SLOTS` string slots and a list of named attributes; their meaning per kind is in `baruch_kinds_nodes`.
!< The strings live in one pool (one character buffer, each string a first byte and a length), the attributes in a
!< pool of records (name, value, next). Node 1 is the root, of kind `NODE_DOCUMENT`, created by `initialize` (or by the
!< first edit of an empty document).
!<
!< Editing never renumbers: a node is created detached, linked by `append_child`, `prepend_child`, `insert_before`,
!< `insert_after` (which move a node already linked elsewhere), and `unlink` turns it and its subtree into tombstones
!< (kind `NODE_NONE`). Strings replaced or unlinked stay in the pool. `compact` is the one operation that renumbers:
!< it drops the tombstones and the dead strings, keeps the order of the live nodes, and returns the map old to new.
!<
!< The document also keeps the text it is read from, its **source**: `load` (a `character` or a StringiFor `string`) and
!< `load_file` sanitise it (`baruch_input`: any encoding to UTF-8, ill-formed sequences and U+0000 to U+FFFD, the byte
!< order mark removed) and index its lines. Line endings (LF, CR, CRLF) are kept as they are, so spans are byte ranges
!< of `source()`, and `line_first`/`line_last` give each line without its ending.
!<
!< Two documents compare by meaning with `equal` (submodule `baruch_equal`): kinds, structure, processed text, the
!< semantic slots and the attributes; spans, recorded style, line endings and tombstones are ignored.
!<
!< Nothing in the document recurses: subtrees are visited by following the links. Every component is allocatable or
!< a scalar, so assignment is a deep copy. Arrays grow by doubling. A misuse (a handle that is not a live node, an
!< accessor of another kind, a value out of range, an edit that would make a cycle) returns a nonzero `stat` and a
!< message, and the documented default result; nothing stops.
use penf,               only : I4P
use stringifor,         only : string
use baruch_input,       only : index_lines, read_bytes, sanitize
use baruch_kinds_nodes
use baruch_status
implicit none
private
public :: markdown_document
public :: insert_before_trusted, insert_after_trusted ! the library's own parsers only, not exported by `baruch`

integer(I4P), parameter :: CAPACITY_DEFAULT = 64_I4P !< Nodes allocated by `initialize` when no capacity is given.
integer(I4P), parameter :: LITERAL_KINDS(5) = [NODE_TEXT, NODE_CODE, NODE_HTML_INLINE, NODE_HTML_BLOCK, &
                                               NODE_CODE_BLOCK] !< Core kinds with a literal (extension kinds too).
character(*), parameter :: LITERAL_WHAT = 'a text, code, HTML, code block or extension node' !< Them, in words.
integer(I4P), parameter :: LINK_KINDS(3) = [NODE_LINK, NODE_IMAGE, NODE_LINK_REFERENCE_DEFINITION] !< Link kinds.
character(*), parameter :: LINK_WHAT = 'a link, an image or a link reference definition' !< Them, in words.

type :: markdown_document
   !< The document: flat node arrays, a string pool, typed accessors.
   private
   ! nodes
   integer(I4P)              :: nodes_last = 0_I4P !< Highest node index in use (live or tombstone).
   integer(I4P)              :: nodes_live = 0_I4P !< Live nodes.
   integer(I4P), allocatable :: kind_(:)           !< Kind of each node (`NODE_NONE`: tombstone).
   integer(I4P), allocatable :: parent_(:)         !< Parent.
   integer(I4P), allocatable :: first_(:)          !< First child.
   integer(I4P), allocatable :: last_(:)           !< Last child.
   integer(I4P), allocatable :: prev_(:)           !< Previous sibling.
   integer(I4P), allocatable :: next_(:)           !< Next sibling.
   integer(I4P), allocatable :: span_(:,:)         !< Span, `(1:2, node)`: first and last byte in the source.
   integer(I4P), allocatable :: ints_(:,:)         !< Integer slots, `(1:NODE_INT_SLOTS, node)`.
   integer(I4P), allocatable :: strs_(:,:)         !< String slots, `(1:NODE_STRING_SLOTS, node)`: string ids.
   integer(I4P), allocatable :: attrs_(:)          !< First attribute record of each node.
   ! string pool
   character(:), allocatable :: chars_                !< Bytes of every string.
   integer(I4P)              :: chars_used = 0_I4P    !< Bytes in use.
   integer(I4P), allocatable :: string_first_(:)      !< First byte of each string.
   integer(I4P), allocatable :: string_len_(:)        !< Length of each string.
   integer(I4P)              :: strings_used = 0_I4P  !< Strings in use.
   ! attribute pool
   integer(I4P), allocatable :: attr_name_(:)         !< Name of each attribute record (string id).
   integer(I4P), allocatable :: attr_value_(:)        !< Value of each attribute record (string id, 0: empty).
   integer(I4P), allocatable :: attr_next_(:)         !< Next record of the same node.
   integer(I4P)              :: attrs_used = 0_I4P    !< Records in use.
   ! extension kinds registered in this document
   integer(I4P), allocatable :: ext_kind_(:)          !< Kind number.
   integer(I4P), allocatable :: ext_name_(:)          !< Name (string id).
   integer(I4P), allocatable :: ext_inline_(:)        !< 1 inline, 0 block.
   integer(I4P)              :: ext_used = 0_I4P      !< Registered kinds.
   ! source
   character(:), allocatable :: source_               !< Sanitised UTF-8 the document is read from.
   integer(I4P), allocatable :: line_first_(:)        !< First byte of each line of the source.
   integer(I4P), allocatable :: line_last_(:)         !< Last byte of each line, its ending excluded.
   contains
      ! life cycle
      procedure, pass(self) :: initialize        !< Reset to a document holding only its root.
      procedure, pass(self) :: destroy           !< Free everything: an empty, uninitialised document.
      procedure, pass(self) :: root              !< Return the root node (1), 0 for an uninitialised document.
      procedure, pass(self) :: nodes_number      !< Return the number of live nodes.
      procedure, pass(self) :: last_node         !< Return the highest node index in use (live or tombstone).
      procedure, pass(self) :: is_node           !< Return true if a handle is a live node.
      procedure, pass(self) :: pool_bytes        !< Return the bytes the string pool holds (dead strings too).
      ! kinds
      procedure, pass(self) :: register_kind     !< Register an extension kind: number, name, block or inline.
      procedure, pass(self) :: is_known_kind     !< Return true for a core kind or a registered one.
      procedure, pass(self) :: kind_is_inline    !< Return true for an inline kind, core or registered.
      procedure, pass(self) :: kind_name         !< Return the name of a kind, core or registered.
      ! nodes and links
      procedure, pass(self) :: new_node          !< Create a detached node of a kind.
      procedure, pass(self) :: node_kind         !< Return the kind of a node.
      procedure, pass(self) :: parent            !< Return the parent of a node.
      procedure, pass(self) :: first_child       !< Return the first child of a node.
      procedure, pass(self) :: last_child        !< Return the last child of a node.
      procedure, pass(self) :: next_sibling      !< Return the next sibling of a node.
      procedure, pass(self) :: prev_sibling      !< Return the previous sibling of a node.
      procedure, pass(self) :: children_number   !< Return the number of children of a node.
      ! editing
      procedure, pass(self) :: append_child      !< Link a node as the last child of another.
      procedure, pass(self) :: prepend_child     !< Link a node as the first child of another.
      procedure, pass(self) :: insert_before     !< Link a node as the previous sibling of another.
      procedure, pass(self) :: insert_after      !< Link a node as the next sibling of another.
      procedure, pass(self) :: unlink            !< Unlink a node: it and its subtree become tombstones.
      procedure, pass(self) :: compact           !< Drop tombstones and dead strings, keep the order, map old to new.
      ! spans
      procedure, pass(self) :: set_span          !< Set the byte span of a node in the source.
      procedure, pass(self) :: span_first        !< Return the first byte of a node's span.
      procedure, pass(self) :: span_last         !< Return the last byte of a node's span.
      ! slots, any kind
      procedure, pass(self) :: int_slot          !< Return an integer slot of a node.
      procedure, pass(self) :: set_int_slot      !< Set an integer slot of a node.
      procedure, pass(self) :: string_slot       !< Return a string slot of a node.
      procedure, pass(self) :: set_string_slot   !< Set a string slot of a node.
      ! typed accessors
      procedure, pass(self) :: heading_level     !< Return the level of a heading.
      procedure, pass(self) :: set_heading_level !< Set the level of a heading (1 to 6).
      procedure, pass(self) :: list_type         !< Return the type of a list (`LIST_BULLET`, `LIST_ORDERED`).
      procedure, pass(self) :: set_list_type     !< Set the type of a list.
      procedure, pass(self) :: list_start        !< Return the start number of a list.
      procedure, pass(self) :: set_list_start    !< Set the start number of a list.
      procedure, pass(self) :: list_tight        !< Return true for a tight list.
      procedure, pass(self) :: set_list_tight    !< Set a list tight or loose.
      procedure, pass(self) :: literal           !< Return the content of a text, code, HTML or extension node.
      procedure, pass(self) :: set_literal       !< Set the content of a text, code, HTML or extension node.
      procedure, pass(self) :: info              !< Return the info string of a code block.
      procedure, pass(self) :: set_info          !< Set the info string of a code block.
      procedure, pass(self) :: destination       !< Return the destination of a link, image or definition.
      procedure, pass(self) :: set_destination   !< Set the destination of a link, image or definition.
      procedure, pass(self) :: title             !< Return the title of a link, image or definition.
      procedure, pass(self) :: set_title         !< Set the title of a link, image or definition.
      procedure, pass(self) :: label             !< Return the label of a link, image or definition.
      procedure, pass(self) :: set_label         !< Set the label of a link, image or definition.
      ! attributes
      procedure, pass(self) :: set_attribute     !< Set a named attribute of a node (replace its value if present).
      procedure, pass(self) :: attribute         !< Return the value of a named attribute of a node.
      procedure, pass(self) :: has_attribute     !< Return true if a node has a named attribute.
      procedure, pass(self) :: attributes_number !< Return the number of attributes of a node.
      procedure, pass(self) :: attribute_name    !< Return the name of the i-th attribute of a node.
      procedure, pass(self) :: attribute_value   !< Return the value of the i-th attribute of a node.
      procedure, pass(self) :: remove_attribute  !< Remove a named attribute of a node.
      ! source
      generic               :: load => load_character, load_string !< Load the source from a text: sanitised, indexed.
      procedure, pass(self) :: load_file         !< Load the source from a file: read whole, sanitised, indexed.
      procedure, pass(self) :: source            !< Return the source, the sanitised UTF-8.
      procedure, pass(self) :: source_length     !< Return the bytes of the source.
      procedure, pass(self) :: source_slice      !< Return the bytes of the source from one position to another.
      procedure, pass(self) :: lines_number      !< Return the number of lines of the source.
      procedure, pass(self) :: line              !< Return a line of the source, its ending excluded.
      procedure, pass(self) :: line_first        !< Return the first byte of a line.
      procedure, pass(self) :: line_last         !< Return the last byte of a line, its ending excluded.
      procedure, pass(self) :: line_ending       !< Return the ending of a line: LF, CR, CRLF or none.
      ! equality (submodule baruch_equal)
      procedure, pass(self) :: equal             !< Return true if two documents mean the same: spans, style ignored.
      ! private helpers
      procedure, pass(self), private :: allocate_pools   !< Allocate every array at its initial capacity.
      procedure, pass(self), private :: ensure_ready     !< Initialise an empty document before an edit.
      procedure, pass(self), private :: reserve_nodes    !< Grow the node arrays to hold a number of nodes.
      procedure, pass(self), private :: put_string       !< Add a string to the pool, return its id.
      procedure, pass(self), private :: get_string       !< Return a string of the pool by id.
      procedure, pass(self), private :: string_equals    !< Return true if a pooled string equals a value.
      procedure, pass(self), private :: node_ok          !< Check a handle, set the status.
      procedure, pass(self), private :: kind_ok          !< Check a handle and its kind, set the status.
      procedure, pass(self), private :: link_ok          !< Check that linking a node under a parent is legal.
      procedure, pass(self), private :: detach           !< Take a node out of its parent's children.
      procedure, pass(self), private :: find_attribute   !< Return the record of a named attribute and its previous.
      procedure, pass(self), private :: nth_attribute    !< Return the record of the i-th attribute.
      procedure, pass(self), private :: load_character   !< Load the source from a `character`.
      procedure, pass(self), private :: load_string      !< Load the source from a `string`.
      procedure, pass(self), private :: set_source       !< Sanitise bytes, keep them as the source, index the lines.
      procedure, pass(self), private :: line_ok          !< Check a line number, set the status.
endtype markdown_document

interface
   module function equal(self, other, path, difference) result(is)
   !< Return true if two documents have the same meaning (submodule `baruch_equal`): the same kinds in the same
   !< structure, the same processed text, the same semantic integer slots and attributes; spans, style slots, line
   !< endings, tombstones and detached nodes ignored. On a difference, `path` names the first differing node
   !< (`/document/list[2]/item/paragraph/emph`) and `difference` says what differs.
   class(markdown_document),  intent(in)            :: self       !< Document.
   class(markdown_document),  intent(in)            :: other      !< The other document.
   character(:), allocatable, intent(out), optional :: path       !< Path of the first differing node, empty if equal.
   character(:), allocatable, intent(out), optional :: difference !< What differs there, empty if equal.
   logical                                          :: is         !< Result.
   endfunction equal
endinterface

contains
   ! life cycle
   subroutine initialize(self, capacity, stat, errmsg)
   !< Reset to a document holding only its root (node 1, `NODE_DOCUMENT`); `capacity` nodes are allocated up front. The
   !< source, if one was loaded, is kept.
   class(markdown_document),  intent(inout)         :: self     !< Document.
   integer(I4P),              intent(in),  optional :: capacity !< Nodes to allocate (default 64; arrays grow anyway).
   integer(I4P),              intent(out), optional :: stat     !< Status.
   character(:), allocatable, intent(out), optional :: errmsg   !< Message.
   integer(I4P)                                     :: cap      !< Capacity.
   integer(I4P)                                     :: ierr     !< Allocation status.

   call status_ok(stat, errmsg)
   cap = CAPACITY_DEFAULT
   if (present(capacity)) cap = max(1_I4P, capacity)
   call destroy_tree(self)
   call self%allocate_pools(cap, ierr)
   if (ierr /= 0) then
      call destroy_tree(self)
      call status_error(BARUCH_ERR_ALLOCATION, 'markdown_document%initialize: cannot allocate '//istr(cap)//' nodes', &
                        stat, errmsg)
      return
   endif
   self%nodes_last = 1_I4P
   self%nodes_live = 1_I4P
   call clear_node(self, 1_I4P, NODE_DOCUMENT)
   endsubroutine initialize

   pure subroutine destroy(self)
   !< Free everything, the source too: an empty, uninitialised document (the next edit initialises it).
   class(markdown_document), intent(inout) :: self !< Document.

   call destroy_tree(self)
   if (allocated(self%source_)) deallocate(self%source_)
   if (allocated(self%line_first_)) deallocate(self%line_first_)
   if (allocated(self%line_last_)) deallocate(self%line_last_)
   endsubroutine destroy

   pure function root(self) result(n)
   !< Return the root node (1), 0 for an uninitialised document.
   class(markdown_document), intent(in) :: self !< Document.
   integer(I4P)                         :: n    !< Root.

   n = min(1_I4P, self%nodes_last)
   endfunction root

   pure function nodes_number(self) result(number)
   !< Return the number of live nodes (the root included).
   class(markdown_document), intent(in) :: self   !< Document.
   integer(I4P)                         :: number !< Live nodes.

   number = self%nodes_live
   endfunction nodes_number

   pure function last_node(self) result(n)
   !< Return the highest node index in use, live or tombstone: handles run from 1 to it.
   class(markdown_document), intent(in) :: self !< Document.
   integer(I4P)                         :: n    !< Highest index.

   n = self%nodes_last
   endfunction last_node

   pure function is_node(self, n) result(is)
   !< Return true if a handle is a live node of the document.
   class(markdown_document), intent(in) :: self !< Document.
   integer(I4P),             intent(in) :: n    !< Handle.
   logical                              :: is   !< Result.

   is = .false.
   if (n < 1_I4P .or. n > self%nodes_last) return
   is = self%kind_(n) /= NODE_NONE
   endfunction is_node

   pure function pool_bytes(self) result(bytes)
   !< Return the bytes the string pool holds, dead strings (replaced, unlinked) included until `compact`.
   class(markdown_document), intent(in) :: self  !< Document.
   integer(I4P)                         :: bytes !< Bytes.

   bytes = self%chars_used
   endfunction pool_bytes

   ! kinds
   subroutine register_kind(self, kind, name, inline, stat, errmsg)
   !< Register an extension kind: a number at `NODE_EXTENSION_FIRST` or above, a name unique in the document, and
   !< whether its nodes are inline (spans) or blocks.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: kind   !< Kind number.
   character(*),              intent(in)            :: name   !< Kind name.
   logical,                   intent(in)            :: inline !< Inline kind (else block).
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: e      !< Counter.
   integer(I4P)                                     :: id     !< Name id.
   integer(I4P)                                     :: ierr   !< Status.

   call status_ok(stat, errmsg)
   if (kind < NODE_EXTENSION_FIRST) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%register_kind: kind '//istr(kind)// &
                        ' is in the core range (extension kinds start at '//istr(NODE_EXTENSION_FIRST)//')', stat, errmsg)
      return
   endif
   if (len(name) == 0) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%register_kind: kind '//istr(kind)//' has no name', &
                        stat, errmsg)
      return
   endif
   do e=1, self%ext_used
      if (self%ext_kind_(e) == kind .or. self%string_equals(self%ext_name_(e), name)) then
         call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%register_kind: kind '//istr(kind)//' "'//name// &
                           '" clashes with the registered kind '//istr(self%ext_kind_(e))//' "'// &
                           self%get_string(self%ext_name_(e))//'"', stat, errmsg)
         return
      endif
   enddo
   call self%ensure_ready(ierr)
   if (ierr == 0 .and. self%ext_used == size(self%ext_kind_, kind=I4P)) then
      call grow_ints(self%ext_kind_, 2_I4P * self%ext_used, ierr)
      if (ierr == 0) call grow_ints(self%ext_name_, 2_I4P * self%ext_used, ierr)
      if (ierr == 0) call grow_ints(self%ext_inline_, 2_I4P * self%ext_used, ierr)
   endif
   if (ierr == 0) call self%put_string(name, id, ierr)
   if (ierr /= 0) then
      call status_error(BARUCH_ERR_ALLOCATION, 'markdown_document%register_kind: cannot allocate kind '//istr(kind), &
                        stat, errmsg)
      return
   endif
   self%ext_used = self%ext_used + 1_I4P
   self%ext_kind_(self%ext_used) = kind
   self%ext_name_(self%ext_used) = id
   self%ext_inline_(self%ext_used) = merge(1_I4P, 0_I4P, inline)
   endsubroutine register_kind

   pure function is_known_kind(self, kind) result(is)
   !< Return true for a core kind or a kind registered in the document (`NODE_NONE` is neither).
   class(markdown_document), intent(in) :: self !< Document.
   integer(I4P),             intent(in) :: kind !< Kind.
   logical                              :: is   !< Result.

   is = is_core_kind(kind) .or. ext_index(self, kind) > 0
   endfunction is_known_kind

   pure function kind_is_inline(self, kind) result(is)
   !< Return true for an inline kind, core or registered (false for a block kind and an unknown one).
   class(markdown_document), intent(in) :: self !< Document.
   integer(I4P),             intent(in) :: kind !< Kind.
   logical                              :: is   !< Result.
   integer(I4P)                         :: e    !< Registered index.

   is = is_core_inline_kind(kind)
   if (is .or. kind < NODE_EXTENSION_FIRST) return
   e = ext_index(self, kind)
   if (e > 0) is = self%ext_inline_(e) == 1_I4P
   endfunction kind_is_inline

   pure function kind_name(self, kind) result(name)
   !< Return the name of a kind, core (`paragraph`, `block_quote`...) or registered; empty for an unknown kind.
   class(markdown_document), intent(in) :: self !< Document.
   integer(I4P),             intent(in) :: kind !< Kind.
   character(:), allocatable            :: name !< Name.
   integer(I4P)                         :: e    !< Registered index.

   name = node_kind_name(kind)
   if (kind < NODE_EXTENSION_FIRST) return
   e = ext_index(self, kind)
   if (e > 0) name = self%get_string(self%ext_name_(e))
   endfunction kind_name

   ! nodes and links
   function new_node(self, kind, stat, errmsg) result(n)
   !< Create a detached node of a kind (core, not the document, or registered); 0 on error.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: kind   !< Kind.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: n      !< New node.
   integer(I4P)                                     :: ierr   !< Status.

   n = 0_I4P
   call status_ok(stat, errmsg)
   if (kind == NODE_DOCUMENT) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%new_node: the document node is the root, '// &
                        'there is only one', stat, errmsg)
      return
   endif
   if (.not. self%is_known_kind(kind)) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%new_node: kind '//istr(kind)// &
                        ' is neither a core kind nor a registered one', stat, errmsg)
      return
   endif
   call self%ensure_ready(ierr)
   if (ierr == 0) call self%reserve_nodes(self%nodes_last + 1_I4P, ierr)
   if (ierr /= 0) then
      call status_error(BARUCH_ERR_ALLOCATION, 'markdown_document%new_node: cannot grow beyond '// &
                        istr(self%nodes_last)//' nodes', stat, errmsg)
      return
   endif
   self%nodes_last = self%nodes_last + 1_I4P
   self%nodes_live = self%nodes_live + 1_I4P
   n = self%nodes_last
   call clear_node(self, n, kind)
   endfunction new_node

   function node_kind(self, n, stat, errmsg) result(kind)
   !< Return the kind of a node; `NODE_NONE` (and an error) for a handle that is not a live node.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: kind   !< Kind.

   kind = NODE_NONE
   if (self%node_ok(n, 'node_kind', stat, errmsg)) kind = self%kind_(n)
   endfunction node_kind

   function parent(self, n, stat, errmsg) result(p)
   !< Return the parent of a node, 0 for the root and a detached node.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: p      !< Parent.

   p = 0_I4P
   if (self%node_ok(n, 'parent', stat, errmsg)) p = self%parent_(n)
   endfunction parent

   function first_child(self, n, stat, errmsg) result(c)
   !< Return the first child of a node, 0 if none.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: c      !< Child.

   c = 0_I4P
   if (self%node_ok(n, 'first_child', stat, errmsg)) c = self%first_(n)
   endfunction first_child

   function last_child(self, n, stat, errmsg) result(c)
   !< Return the last child of a node, 0 if none.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: c      !< Child.

   c = 0_I4P
   if (self%node_ok(n, 'last_child', stat, errmsg)) c = self%last_(n)
   endfunction last_child

   function next_sibling(self, n, stat, errmsg) result(s)
   !< Return the next sibling of a node, 0 if none.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: s      !< Sibling.

   s = 0_I4P
   if (self%node_ok(n, 'next_sibling', stat, errmsg)) s = self%next_(n)
   endfunction next_sibling

   function prev_sibling(self, n, stat, errmsg) result(s)
   !< Return the previous sibling of a node, 0 if none.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: s      !< Sibling.

   s = 0_I4P
   if (self%node_ok(n, 'prev_sibling', stat, errmsg)) s = self%prev_(n)
   endfunction prev_sibling

   function children_number(self, n, stat, errmsg) result(number)
   !< Return the number of children of a node (counted along the sibling links).
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: number !< Children.
   integer(I4P)                                     :: c      !< Child.

   number = 0_I4P
   if (.not. self%node_ok(n, 'children_number', stat, errmsg)) return
   c = self%first_(n)
   do while (c /= 0_I4P)
      number = number + 1_I4P
      c = self%next_(c)
   enddo
   endfunction children_number

   ! editing
   subroutine append_child(self, parent, child, stat, errmsg)
   !< Link a node as the last child of another; a node linked elsewhere is moved, with its subtree.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: parent !< Parent.
   integer(I4P),              intent(in)            :: child  !< Node to link.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: l      !< Last child of the parent.

   if (.not. self%link_ok(parent, child, 'append_child', stat, errmsg)) return
   call self%detach(child)
   l = self%last_(parent)
   self%parent_(child) = parent
   self%prev_(child) = l
   self%next_(child) = 0_I4P
   if (l /= 0_I4P) then
      self%next_(l) = child
   else
      self%first_(parent) = child
   endif
   self%last_(parent) = child
   endsubroutine append_child

   subroutine prepend_child(self, parent, child, stat, errmsg)
   !< Link a node as the first child of another; a node linked elsewhere is moved, with its subtree.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: parent !< Parent.
   integer(I4P),              intent(in)            :: child  !< Node to link.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: f      !< First child of the parent.

   if (.not. self%link_ok(parent, child, 'prepend_child', stat, errmsg)) return
   call self%detach(child)
   f = self%first_(parent)
   self%parent_(child) = parent
   self%prev_(child) = 0_I4P
   self%next_(child) = f
   if (f /= 0_I4P) then
      self%prev_(f) = child
   else
      self%last_(parent) = child
   endif
   self%first_(parent) = child
   endsubroutine prepend_child

   subroutine insert_before(self, sibling, node, stat, errmsg)
   !< Link a node as the previous sibling of another (which must have a parent); a linked node is moved.
   class(markdown_document),  intent(inout)         :: self    !< Document.
   integer(I4P),              intent(in)            :: sibling !< Sibling.
   integer(I4P),              intent(in)            :: node    !< Node to link.
   integer(I4P),              intent(out), optional :: stat    !< Status.
   character(:), allocatable, intent(out), optional :: errmsg  !< Message.
   integer(I4P)                                     :: p       !< Parent.

   if (.not. sibling_ok(self, sibling, node, 'insert_before', stat, errmsg)) return
   p = self%parent_(sibling)
   if (.not. self%link_ok(p, node, 'insert_before', stat, errmsg)) return
   call insert_before_trusted(self, sibling, node)
   endsubroutine insert_before

   subroutine insert_after(self, sibling, node, stat, errmsg)
   !< Link a node as the next sibling of another (which must have a parent); a linked node is moved.
   class(markdown_document),  intent(inout)         :: self    !< Document.
   integer(I4P),              intent(in)            :: sibling !< Sibling.
   integer(I4P),              intent(in)            :: node    !< Node to link.
   integer(I4P),              intent(out), optional :: stat    !< Status.
   character(:), allocatable, intent(out), optional :: errmsg  !< Message.
   integer(I4P)                                     :: p       !< Parent.

   if (.not. sibling_ok(self, sibling, node, 'insert_after', stat, errmsg)) return
   p = self%parent_(sibling)
   if (.not. self%link_ok(p, node, 'insert_after', stat, errmsg)) return
   call insert_after_trusted(self, sibling, node)
   endsubroutine insert_after

   subroutine unlink(self, n, stat, errmsg)
   !< Unlink a node: it and its whole subtree become tombstones (their handles stop being nodes) until `compact`.
   !< The subtree is visited by its links, deepest first, with no recursion and no stack.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: c      !< Current node.
   integer(I4P)                                     :: s      !< Its next sibling.
   integer(I4P)                                     :: p      !< Its parent.

   if (.not. self%node_ok(n, 'unlink', stat, errmsg)) return
   if (self%kind_(n) == NODE_DOCUMENT) then
      call status_error(BARUCH_ERR_LINK, 'markdown_document%unlink: node '//istr(n)//' is the root', stat, errmsg)
      return
   endif
   call self%detach(n)
   c = n
   do
      do while (self%first_(c) /= 0_I4P) ! down to a node with no children left
         c = self%first_(c)
      enddo
      if (c == n) then
         call clear_node(self, c, NODE_NONE)
         self%nodes_live = self%nodes_live - 1_I4P
         exit
      endif
      s = self%next_(c)
      p = self%parent_(c)
      call clear_node(self, c, NODE_NONE)
      self%nodes_live = self%nodes_live - 1_I4P
      if (s /= 0_I4P) then
         c = s
      else ! every child of p is gone
         self%first_(p) = 0_I4P
         self%last_(p) = 0_I4P
         c = p
      endif
   enddo
   endsubroutine unlink

   subroutine compact(self, map, stat, errmsg)
   !< Drop the tombstones and the dead strings and attributes. The live nodes keep their order (a node's new index is
   !< the number of live nodes up to it), the root stays 1, detached live nodes are kept. `map(old)` is the new index
   !< of the old handle `old`, 0 for a tombstone: every handle the caller keeps must go through it.
   class(markdown_document),  intent(inout)                   :: self   !< Document.
   integer(I4P), allocatable, intent(out), optional           :: map(:) !< Old to new indices, `map(1:last_node())`.
   integer(I4P),              intent(out), optional           :: stat   !< Status.
   character(:), allocatable, intent(out), optional           :: errmsg !< Message.
   integer(I4P),              allocatable                     :: m(:)   !< Old to new.
   character(:),              allocatable                     :: chars  !< New string bytes.
   integer(I4P),              allocatable                     :: sfirst(:), slen(:)          !< New strings.
   integer(I4P),              allocatable                     :: aname(:), avalue(:), anext(:) !< New attributes.
   integer(I4P)                                               :: nbytes, nstrings, nattrs     !< New pool sizes.
   integer(I4P)                                               :: i, j, k, a, tail, ierr       !< Counters.

   call status_ok(stat, errmsg)
   if (self%nodes_last == 0_I4P) then
      if (present(map)) allocate(map(0))
      return
   endif
   allocate(m(self%nodes_last), stat=ierr)
   if (ierr /= 0) then
      call status_error(BARUCH_ERR_ALLOCATION, 'markdown_document%compact: cannot allocate the map of '// &
                        istr(self%nodes_last)//' nodes', stat, errmsg)
      return
   endif
   ! the pool of the live nodes, sized first
   nbytes = 0_I4P ; nstrings = 0_I4P ; nattrs = 0_I4P
   j = 0_I4P
   do i=1, self%nodes_last
      if (self%kind_(i) == NODE_NONE) then
         m(i) = 0_I4P
         cycle
      endif
      j = j + 1_I4P
      m(i) = j
      do k=1, NODE_STRING_SLOTS
         call count_string(self%strs_(k, i))
      enddo
      a = self%attrs_(i)
      do while (a /= 0_I4P)
         nattrs = nattrs + 1_I4P
         call count_string(self%attr_name_(a))
         call count_string(self%attr_value_(a))
         a = self%attr_next_(a)
      enddo
   enddo
   do k=1, self%ext_used
      call count_string(self%ext_name_(k))
   enddo
   allocate(character(max(nbytes, 1_I4P)) :: chars, stat=ierr)
   if (ierr == 0) allocate(sfirst(max(nstrings, 1_I4P)), slen(max(nstrings, 1_I4P)), stat=ierr)
   if (ierr == 0) allocate(aname(max(nattrs, 1_I4P)), avalue(max(nattrs, 1_I4P)), anext(max(nattrs, 1_I4P)), stat=ierr)
   if (ierr /= 0) then
      call status_error(BARUCH_ERR_ALLOCATION, 'markdown_document%compact: cannot allocate the new pool of '// &
                        istr(nbytes)//' bytes', stat, errmsg)
      return
   endif
   ! nodes moved down in place (m(i) <= i, ascending), links renumbered, strings and attributes copied in node order
   nbytes = 0_I4P ; nstrings = 0_I4P ; nattrs = 0_I4P
   do i=1, self%nodes_last
      j = m(i)
      if (j == 0_I4P) cycle
      self%kind_(j) = self%kind_(i)
      self%parent_(j) = renumber(self%parent_(i))
      self%first_(j) = renumber(self%first_(i))
      self%last_(j) = renumber(self%last_(i))
      self%prev_(j) = renumber(self%prev_(i))
      self%next_(j) = renumber(self%next_(i))
      self%span_(:, j) = self%span_(:, i)
      self%ints_(:, j) = self%ints_(:, i)
      do k=1, NODE_STRING_SLOTS
         self%strs_(k, j) = copy_string(self%strs_(k, i))
      enddo
      a = self%attrs_(i)
      self%attrs_(j) = 0_I4P
      tail = 0_I4P
      do while (a /= 0_I4P)
         nattrs = nattrs + 1_I4P
         aname(nattrs) = copy_string(self%attr_name_(a))
         avalue(nattrs) = copy_string(self%attr_value_(a))
         anext(nattrs) = 0_I4P
         if (tail == 0_I4P) then
            self%attrs_(j) = nattrs
         else
            anext(tail) = nattrs
         endif
         tail = nattrs
         a = self%attr_next_(a)
      enddo
   enddo
   do k=1, self%ext_used
      self%ext_name_(k) = copy_string(self%ext_name_(k))
   enddo
   self%nodes_last = self%nodes_live
   self%chars_ = chars ! assignment, not move_alloc: see put_string
   self%chars_used = nbytes
   call move_alloc(sfirst, self%string_first_)
   call move_alloc(slen, self%string_len_)
   self%strings_used = nstrings
   call move_alloc(aname, self%attr_name_)
   call move_alloc(avalue, self%attr_value_)
   call move_alloc(anext, self%attr_next_)
   self%attrs_used = nattrs
   if (present(map)) call move_alloc(m, map)
   contains
      subroutine count_string(id)
      !< Add a string to the sizes of the new pool.
      integer(I4P), intent(in) :: id !< String id (0: none).

      if (id <= 0_I4P) return
      nstrings = nstrings + 1_I4P
      nbytes = nbytes + self%string_len_(id)
      endsubroutine count_string

      function copy_string(id) result(new_id)
      !< Copy a string of the old pool into the new one.
      integer(I4P), intent(in) :: id     !< Old id (0: none).
      integer(I4P)             :: new_id !< New id.
      integer(I4P)             :: l      !< Length.

      new_id = 0_I4P
      if (id <= 0_I4P) return
      l = self%string_len_(id)
      nstrings = nstrings + 1_I4P
      sfirst(nstrings) = nbytes + 1_I4P
      slen(nstrings) = l
      chars(nbytes+1:nbytes+l) = self%chars_(self%string_first_(id):self%string_first_(id)+l-1)
      nbytes = nbytes + l
      new_id = nstrings
      endfunction copy_string

      pure function renumber(old) result(new)
      !< Renumber a link (0 stays 0).
      integer(I4P), intent(in) :: old !< Old index.
      integer(I4P)             :: new !< New index.

      new = 0_I4P
      if (old > 0_I4P) new = m(old)
      endfunction renumber
   endsubroutine compact

   ! spans
   subroutine set_span(self, n, first, last, stat, errmsg)
   !< Set the byte span of a node in the source: bytes `first` to `last` (`last = first - 1`: empty; 0, -1: none).
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: first  !< First byte.
   integer(I4P),              intent(in)            :: last   !< Last byte.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%node_ok(n, 'set_span', stat, errmsg)) return
   if (first < 0_I4P .or. last < first - 1_I4P) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%set_span: span '//istr(first)//':'//istr(last)// &
                        ' of node '//istr(n)//' is not a byte range', stat, errmsg)
      return
   endif
   self%span_(1, n) = first
   self%span_(2, n) = last
   endsubroutine set_span

   function span_first(self, n, stat, errmsg) result(first)
   !< Return the first byte of a node's span in the source, 0 if none.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: first  !< First byte.

   first = 0_I4P
   if (self%node_ok(n, 'span_first', stat, errmsg)) first = self%span_(1, n)
   endfunction span_first

   function span_last(self, n, stat, errmsg) result(last)
   !< Return the last byte of a node's span in the source, -1 if none.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: last   !< Last byte.

   last = -1_I4P
   if (self%node_ok(n, 'span_last', stat, errmsg)) last = self%span_(2, n)
   endfunction span_last

   ! slots, any kind
   function int_slot(self, n, slot, stat, errmsg) result(value)
   !< Return an integer slot of a node, any kind (the `SLOT_*` constants name them per kind); 0 on error.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: slot   !< Slot, 1 to `NODE_INT_SLOTS`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: value  !< Value.

   value = 0_I4P
   if (.not. self%node_ok(n, 'int_slot', stat, errmsg)) return
   if (.not. slot_ok(slot, NODE_INT_SLOTS, 'int_slot', stat, errmsg)) return
   value = self%ints_(slot, n)
   endfunction int_slot

   subroutine set_int_slot(self, n, slot, value, stat, errmsg)
   !< Set an integer slot of a node, any kind.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: slot   !< Slot, 1 to `NODE_INT_SLOTS`.
   integer(I4P),              intent(in)            :: value  !< Value.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%node_ok(n, 'set_int_slot', stat, errmsg)) return
   if (.not. slot_ok(slot, NODE_INT_SLOTS, 'set_int_slot', stat, errmsg)) return
   self%ints_(slot, n) = value
   endsubroutine set_int_slot

   function string_slot(self, n, slot, stat, errmsg) result(value)
   !< Return a string slot of a node, any kind (the `STRING_*` constants name them per kind); empty on error.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: slot   !< Slot, 1 to `NODE_STRING_SLOTS`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: value  !< Value.

   value = ''
   if (.not. self%node_ok(n, 'string_slot', stat, errmsg)) return
   if (.not. slot_ok(slot, NODE_STRING_SLOTS, 'string_slot', stat, errmsg)) return
   value = self%get_string(self%strs_(slot, n))
   endfunction string_slot

   subroutine set_string_slot(self, n, slot, value, stat, errmsg)
   !< Set a string slot of a node, any kind (the previous value stays in the pool until `compact`).
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: slot   !< Slot, 1 to `NODE_STRING_SLOTS`.
   character(*),              intent(in)            :: value  !< Value.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%node_ok(n, 'set_string_slot', stat, errmsg)) return
   if (.not. slot_ok(slot, NODE_STRING_SLOTS, 'set_string_slot', stat, errmsg)) return
   call store_string(self, n, slot, value, 'set_string_slot', stat, errmsg)
   endsubroutine set_string_slot

   ! typed accessors
   function heading_level(self, n, stat, errmsg) result(level)
   !< Return the level of a heading (0 if not set); 0 and an error for another kind.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Heading.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: level  !< Level.

   level = 0_I4P
   if (self%kind_ok(n, [NODE_HEADING], 'a heading', 'heading_level', stat, errmsg)) &
      level = self%ints_(SLOT_HEADING_LEVEL, n)
   endfunction heading_level

   subroutine set_heading_level(self, n, level, stat, errmsg)
   !< Set the level of a heading, 1 to 6.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Heading.
   integer(I4P),              intent(in)            :: level  !< Level.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, [NODE_HEADING], 'a heading', 'set_heading_level', stat, errmsg)) return
   if (level < 1_I4P .or. level > 6_I4P) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%set_heading_level: level '//istr(level)// &
                        ' is not 1 to 6', stat, errmsg)
      return
   endif
   self%ints_(SLOT_HEADING_LEVEL, n) = level
   endsubroutine set_heading_level

   function list_type(self, n, stat, errmsg) result(ltype)
   !< Return the type of a list, `LIST_BULLET` or `LIST_ORDERED` (0 if not set); 0 and an error for another kind.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< List.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: ltype  !< Type.

   ltype = 0_I4P
   if (self%kind_ok(n, [NODE_LIST], 'a list', 'list_type', stat, errmsg)) ltype = self%ints_(SLOT_LIST_TYPE, n)
   endfunction list_type

   subroutine set_list_type(self, n, ltype, stat, errmsg)
   !< Set the type of a list, `LIST_BULLET` or `LIST_ORDERED`.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< List.
   integer(I4P),              intent(in)            :: ltype  !< Type.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, [NODE_LIST], 'a list', 'set_list_type', stat, errmsg)) return
   if (ltype /= LIST_BULLET .and. ltype /= LIST_ORDERED) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%set_list_type: type '//istr(ltype)// &
                        ' is neither LIST_BULLET nor LIST_ORDERED', stat, errmsg)
      return
   endif
   self%ints_(SLOT_LIST_TYPE, n) = ltype
   endsubroutine set_list_type

   function list_start(self, n, stat, errmsg) result(start)
   !< Return the start number of a list; 0 and an error for another kind.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< List.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: start  !< Start number.

   start = 0_I4P
   if (self%kind_ok(n, [NODE_LIST], 'a list', 'list_start', stat, errmsg)) start = self%ints_(SLOT_LIST_START, n)
   endfunction list_start

   subroutine set_list_start(self, n, start, stat, errmsg)
   !< Set the start number of a list (0 or more).
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< List.
   integer(I4P),              intent(in)            :: start  !< Start number.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, [NODE_LIST], 'a list', 'set_list_start', stat, errmsg)) return
   if (start < 0_I4P) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%set_list_start: start '//istr(start)// &
                        ' is negative', stat, errmsg)
      return
   endif
   self%ints_(SLOT_LIST_START, n) = start
   endsubroutine set_list_start

   function list_tight(self, n, stat, errmsg) result(tight)
   !< Return true for a tight list; false and an error for another kind.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< List.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   logical                                          :: tight  !< Tight.

   tight = .false.
   if (self%kind_ok(n, [NODE_LIST], 'a list', 'list_tight', stat, errmsg)) tight = self%ints_(SLOT_LIST_TIGHT, n) == 1_I4P
   endfunction list_tight

   subroutine set_list_tight(self, n, tight, stat, errmsg)
   !< Set a list tight (true) or loose (false).
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< List.
   logical,                   intent(in)            :: tight  !< Tight.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, [NODE_LIST], 'a list', 'set_list_tight', stat, errmsg)) return
   self%ints_(SLOT_LIST_TIGHT, n) = merge(1_I4P, 0_I4P, tight)
   endsubroutine set_list_tight

   function literal(self, n, stat, errmsg) result(value)
   !< Return the content of a text, code span, inline HTML, HTML block, code block or extension node.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: value  !< Content.

   value = ''
   if (self%kind_ok(n, LITERAL_KINDS, LITERAL_WHAT, 'literal', stat, errmsg, extensions=.true.)) &
      value = self%get_string(self%strs_(STRING_LITERAL, n))
   endfunction literal

   subroutine set_literal(self, n, value, stat, errmsg)
   !< Set the content of a text, code span, inline HTML, HTML block, code block or extension node.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   character(*),              intent(in)            :: value  !< Content.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, LITERAL_KINDS, LITERAL_WHAT, 'set_literal', stat, errmsg, extensions=.true.)) return
   call store_string(self, n, STRING_LITERAL, value, 'set_literal', stat, errmsg)
   endsubroutine set_literal

   function info(self, n, stat, errmsg) result(value)
   !< Return the info string of a code block.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Code block.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: value  !< Info string.

   value = ''
   if (self%kind_ok(n, [NODE_CODE_BLOCK], 'a code block', 'info', stat, errmsg)) &
      value = self%get_string(self%strs_(STRING_INFO, n))
   endfunction info

   subroutine set_info(self, n, value, stat, errmsg)
   !< Set the info string of a code block.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Code block.
   character(*),              intent(in)            :: value  !< Info string.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, [NODE_CODE_BLOCK], 'a code block', 'set_info', stat, errmsg)) return
   call store_string(self, n, STRING_INFO, value, 'set_info', stat, errmsg)
   endsubroutine set_info

   function destination(self, n, stat, errmsg) result(value)
   !< Return the destination of a link, an image or a link reference definition.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: value  !< Destination.

   value = ''
   if (self%kind_ok(n, LINK_KINDS, LINK_WHAT, 'destination', stat, errmsg)) &
      value = self%get_string(self%strs_(STRING_DESTINATION, n))
   endfunction destination

   subroutine set_destination(self, n, value, stat, errmsg)
   !< Set the destination of a link, an image or a link reference definition.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   character(*),              intent(in)            :: value  !< Destination.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, LINK_KINDS, LINK_WHAT, 'set_destination', stat, errmsg)) return
   call store_string(self, n, STRING_DESTINATION, value, 'set_destination', stat, errmsg)
   endsubroutine set_destination

   function title(self, n, stat, errmsg) result(value)
   !< Return the title of a link, an image or a link reference definition.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: value  !< Title.

   value = ''
   if (self%kind_ok(n, LINK_KINDS, LINK_WHAT, 'title', stat, errmsg)) &
      value = self%get_string(self%strs_(STRING_TITLE, n))
   endfunction title

   subroutine set_title(self, n, value, stat, errmsg)
   !< Set the title of a link, an image or a link reference definition.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   character(*),              intent(in)            :: value  !< Title.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, LINK_KINDS, LINK_WHAT, 'set_title', stat, errmsg)) return
   call store_string(self, n, STRING_TITLE, value, 'set_title', stat, errmsg)
   endsubroutine set_title

   function label(self, n, stat, errmsg) result(value)
   !< Return the label of a link, an image (full, collapsed or shortcut reference) or a link reference definition.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: value  !< Label.

   value = ''
   if (self%kind_ok(n, LINK_KINDS, LINK_WHAT, 'label', stat, errmsg)) &
      value = self%get_string(self%strs_(STRING_LABEL, n))
   endfunction label

   subroutine set_label(self, n, value, stat, errmsg)
   !< Set the label of a link, an image or a link reference definition.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   character(*),              intent(in)            :: value  !< Label.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (.not. self%kind_ok(n, LINK_KINDS, LINK_WHAT, 'set_label', stat, errmsg)) return
   call store_string(self, n, STRING_LABEL, value, 'set_label', stat, errmsg)
   endsubroutine set_label

   ! attributes
   subroutine set_attribute(self, n, name, value, stat, errmsg)
   !< Set a named attribute of a node, any kind: its value is replaced if the node has it, else it is appended (the
   !< attributes keep the order they were first set in).
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   character(*),              intent(in)            :: name   !< Name (not empty).
   character(*),              intent(in)            :: value  !< Value (may be empty).
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: a      !< Record.
   integer(I4P)                                     :: prev   !< Previous record (unused).
   integer(I4P)                                     :: tail   !< Last record of the node.
   integer(I4P)                                     :: id     !< String id.
   integer(I4P)                                     :: name_id !< Name id.
   integer(I4P)                                     :: ierr   !< Status.

   if (.not. self%node_ok(n, 'set_attribute', stat, errmsg)) return
   if (len(name) == 0) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%set_attribute: an attribute of node '//istr(n)// &
                        ' has no name', stat, errmsg)
      return
   endif
   call self%find_attribute(n, name, a, prev)
   if (a /= 0_I4P) then
      call self%put_string(value, id, ierr)
      if (ierr == 0) self%attr_value_(a) = id
   else
      ierr = 0
      if (self%attrs_used == size(self%attr_name_, kind=I4P)) then
         call grow_ints(self%attr_name_, 2_I4P * self%attrs_used, ierr)
         if (ierr == 0) call grow_ints(self%attr_value_, 2_I4P * self%attrs_used, ierr)
         if (ierr == 0) call grow_ints(self%attr_next_, 2_I4P * self%attrs_used, ierr)
      endif
      if (ierr == 0) call self%put_string(name, name_id, ierr)
      if (ierr == 0) call self%put_string(value, id, ierr)
      if (ierr == 0) then
         self%attrs_used = self%attrs_used + 1_I4P
         a = self%attrs_used
         self%attr_name_(a) = name_id
         self%attr_value_(a) = id
         self%attr_next_(a) = 0_I4P
         tail = self%attrs_(n)
         if (tail == 0_I4P) then
            self%attrs_(n) = a
         else
            do while (self%attr_next_(tail) /= 0_I4P)
               tail = self%attr_next_(tail)
            enddo
            self%attr_next_(tail) = a
         endif
      endif
   endif
   if (ierr /= 0) call status_error(BARUCH_ERR_ALLOCATION, 'markdown_document%set_attribute: cannot store attribute "'// &
                                    name//'" of node '//istr(n), stat, errmsg)
   endsubroutine set_attribute

   function attribute(self, n, name, found, stat, errmsg) result(value)
   !< Return the value of a named attribute of a node; empty if the node does not have it (`found` tells).
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   character(*),              intent(in)            :: name   !< Name.
   logical,                   intent(out), optional :: found  !< The node has the attribute.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: value  !< Value.
   integer(I4P)                                     :: a      !< Record.
   integer(I4P)                                     :: prev   !< Previous record.

   value = ''
   if (present(found)) found = .false.
   if (.not. self%node_ok(n, 'attribute', stat, errmsg)) return
   call self%find_attribute(n, name, a, prev)
   if (a == 0_I4P) return
   if (present(found)) found = .true.
   value = self%get_string(self%attr_value_(a))
   endfunction attribute

   function has_attribute(self, n, name, stat, errmsg) result(has)
   !< Return true if a node has a named attribute.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   character(*),              intent(in)            :: name   !< Name.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   logical                                          :: has    !< Result.
   integer(I4P)                                     :: a      !< Record.
   integer(I4P)                                     :: prev   !< Previous record.

   has = .false.
   if (.not. self%node_ok(n, 'has_attribute', stat, errmsg)) return
   call self%find_attribute(n, name, a, prev)
   has = a /= 0_I4P
   endfunction has_attribute

   function attributes_number(self, n, stat, errmsg) result(number)
   !< Return the number of attributes of a node.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: number !< Attributes.
   integer(I4P)                                     :: a      !< Record.

   number = 0_I4P
   if (.not. self%node_ok(n, 'attributes_number', stat, errmsg)) return
   a = self%attrs_(n)
   do while (a /= 0_I4P)
      number = number + 1_I4P
      a = self%attr_next_(a)
   enddo
   endfunction attributes_number

   function attribute_name(self, n, i, stat, errmsg) result(name)
   !< Return the name of the i-th attribute of a node (in the order they were first set).
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: i      !< Position, 1 to `attributes_number(n)`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: name   !< Name.
   integer(I4P)                                     :: a      !< Record.

   name = ''
   a = self%nth_attribute(n, i, 'attribute_name', stat, errmsg)
   if (a /= 0_I4P) name = self%get_string(self%attr_name_(a))
   endfunction attribute_name

   function attribute_value(self, n, i, stat, errmsg) result(value)
   !< Return the value of the i-th attribute of a node (in the order they were first set).
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: i      !< Position, 1 to `attributes_number(n)`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: value  !< Value.
   integer(I4P)                                     :: a      !< Record.

   value = ''
   a = self%nth_attribute(n, i, 'attribute_value', stat, errmsg)
   if (a /= 0_I4P) value = self%get_string(self%attr_value_(a))
   endfunction attribute_value

   subroutine remove_attribute(self, n, name, stat, errmsg)
   !< Remove a named attribute of a node (nothing happens if the node does not have it).
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   character(*),              intent(in)            :: name   !< Name.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: a      !< Record.
   integer(I4P)                                     :: prev   !< Previous record.

   if (.not. self%node_ok(n, 'remove_attribute', stat, errmsg)) return
   call self%find_attribute(n, name, a, prev)
   if (a == 0_I4P) return
   if (prev == 0_I4P) then
      self%attrs_(n) = self%attr_next_(a)
   else
      self%attr_next_(prev) = self%attr_next_(a)
   endif
   endsubroutine remove_attribute

   ! source
   subroutine load_character(self, text, encoding, stat, errmsg)
   !< Load the source from a text: `text` read in `encoding` (absent or blank: a byte order mark decides, else UTF-8),
   !< sanitised to UTF-8 and its lines indexed; the tree is reset to the root alone. An unsupported encoding gives
   !< `BARUCH_ERR_ARGUMENT` and leaves the document as it was.
   class(markdown_document),  intent(inout)         :: self     !< Document.
   character(*),              intent(in)            :: text     !< Text, any bytes.
   character(*),              intent(in),  optional :: encoding !< Encoding name or alias.
   integer(I4P),              intent(out), optional :: stat     !< Status.
   character(:), allocatable, intent(out), optional :: errmsg   !< Message.

   call self%set_source(text, 'load', encoding, stat, errmsg)
   endsubroutine load_character

   subroutine load_string(self, text, encoding, stat, errmsg)
   !< Load the source from a StringiFor `string` (an unallocated one is an empty text), as the `character` version.
   class(markdown_document),  intent(inout)         :: self     !< Document.
   type(string),              intent(in)            :: text     !< Text, any bytes.
   character(*),              intent(in),  optional :: encoding !< Encoding name or alias.
   integer(I4P),              intent(out), optional :: stat     !< Status.
   character(:), allocatable, intent(out), optional :: errmsg   !< Message.

   if (allocated(text%raw)) then
      call self%set_source(text%raw, 'load', encoding, stat, errmsg)
   else
      call self%set_source('', 'load', encoding, stat, errmsg)
   endif
   endsubroutine load_string

   subroutine load_file(self, path, encoding, stat, errmsg)
   !< Load the source from a file, read whole (stream access, `newunit=`), as `load`. A file that cannot be opened or
   !< read gives `BARUCH_ERR_IO` and leaves the document as it was.
   class(markdown_document),  intent(inout)         :: self     !< Document.
   character(*),              intent(in)            :: path     !< File name.
   character(*),              intent(in),  optional :: encoding !< Encoding name or alias.
   integer(I4P),              intent(out), optional :: stat     !< Status.
   character(:), allocatable, intent(out), optional :: errmsg   !< Message.
   character(:), allocatable                        :: bytes    !< Contents of the file.
   character(:), allocatable                        :: msg      !< Message of the read.
   integer(I4P)                                     :: ierr     !< Status of the read.

   call read_bytes(path, bytes, ierr, msg)
   if (ierr /= 0_I4P) then
      call status_error(BARUCH_ERR_IO, 'markdown_document%load_file: '//msg, stat, errmsg)
      return
   endif
   call self%set_source(bytes, 'load_file', encoding, stat, errmsg)
   endsubroutine load_file

   pure function source(self) result(text)
   !< Return the source: the sanitised UTF-8 the document was loaded from, its line endings as they were; empty if none.
   class(markdown_document), intent(in) :: self !< Document.
   character(:), allocatable            :: text !< Source.

   if (allocated(self%source_)) then
      text = self%source_
   else
      text = ''
   endif
   endfunction source

   pure function source_length(self) result(length)
   !< Return the bytes of the source, 0 if none was loaded.
   class(markdown_document), intent(in) :: self   !< Document.
   integer(I4P)                         :: length !< Bytes.

   length = 0_I4P
   if (allocated(self%source_)) length = len(self%source_, kind=I4P)
   endfunction source_length

   function source_slice(self, first, last, stat, errmsg) result(text)
   !< Return the bytes `first` to `last` of the source (a span, a line): empty for `last = first - 1`; a range outside
   !< the source gives `BARUCH_ERR_ARGUMENT` and an empty text.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: first  !< First byte.
   integer(I4P),              intent(in)            :: last   !< Last byte.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: text   !< Bytes.

   text = ''
   if (first < 1_I4P .or. last < first - 1_I4P .or. last > self%source_length()) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%source_slice: bytes '//istr(first)//' to '// &
                        istr(last)//' are not a range of the source (1 to '//istr(self%source_length())//')', &
                        stat, errmsg)
      return
   endif
   call status_ok(stat, errmsg)
   if (last >= first) text = self%source_(first:last)
   endfunction source_slice

   pure function lines_number(self) result(number)
   !< Return the number of lines of the source: a line ends at LF, CR, CRLF or the end of the source, and a final line
   !< ending does not start one more (CommonMark 2.1); 0 for an empty source.
   class(markdown_document), intent(in) :: self   !< Document.
   integer(I4P)                         :: number !< Lines.

   number = 0_I4P
   if (allocated(self%line_first_)) number = size(self%line_first_, kind=I4P)
   endfunction lines_number

   function line(self, i, stat, errmsg) result(text)
   !< Return the line `i` of the source, its ending excluded; empty, with `BARUCH_ERR_ARGUMENT`, for no such line.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: i      !< Line number, 1 to `lines_number()`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: text   !< Line.

   text = ''
   if (.not. self%line_ok(i, 'line', stat, errmsg)) return
   text = self%source_(self%line_first_(i):self%line_last_(i))
   endfunction line

   function line_first(self, i, stat, errmsg) result(b)
   !< Return the first byte of the line `i` in the source; 0, with `BARUCH_ERR_ARGUMENT`, for no such line.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: i      !< Line number, 1 to `lines_number()`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: b      !< First byte.

   b = 0_I4P
   if (self%line_ok(i, 'line_first', stat, errmsg)) b = self%line_first_(i)
   endfunction line_first

   function line_last(self, i, stat, errmsg) result(b)
   !< Return the last byte of the line `i` in the source, its ending excluded (`line_first(i) - 1` for an empty line);
   !< 0, with `BARUCH_ERR_ARGUMENT`, for no such line.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: i      !< Line number, 1 to `lines_number()`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: b      !< Last byte.

   b = 0_I4P
   if (self%line_ok(i, 'line_last', stat, errmsg)) b = self%line_last_(i)
   endfunction line_last

   function line_ending(self, i, stat, errmsg) result(ending)
   !< Return the ending of the line `i`: LF, CR or CRLF as in the source, empty for the last line of a source that does
   !< not end with one (and, with `BARUCH_ERR_ARGUMENT`, for no such line).
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: i      !< Line number, 1 to `lines_number()`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: ending !< Ending.
   integer(I4P)                                     :: next   !< First byte after the ending.

   ending = ''
   if (.not. self%line_ok(i, 'line_ending', stat, errmsg)) return
   next = self%source_length() + 1_I4P
   if (i < self%lines_number()) next = self%line_first_(i + 1_I4P)
   ending = self%source_(self%line_last_(i) + 1_I4P:next - 1_I4P)
   endfunction line_ending

   ! private helpers
   pure subroutine allocate_pools(self, capacity, ierr)
   !< Allocate every array at its initial capacity (`capacity` nodes; strings, attributes, kinds sized from it).
   class(markdown_document), intent(inout) :: self     !< Document.
   integer(I4P),             intent(in)    :: capacity !< Nodes.
   integer(I4P),             intent(out)   :: ierr     !< Status.

   allocate(self%kind_(capacity), self%parent_(capacity), self%first_(capacity), self%last_(capacity),       &
            self%prev_(capacity), self%next_(capacity), self%span_(2, capacity), self%ints_(NODE_INT_SLOTS, capacity), &
            self%strs_(NODE_STRING_SLOTS, capacity), self%attrs_(capacity), stat=ierr)
   if (ierr /= 0) return
   allocate(character(16_I4P * capacity) :: self%chars_, stat=ierr)
   if (ierr /= 0) return
   allocate(self%string_first_(capacity), self%string_len_(capacity), stat=ierr)
   if (ierr /= 0) return
   allocate(self%attr_name_(8), self%attr_value_(8), self%attr_next_(8), stat=ierr)
   if (ierr /= 0) return
   allocate(self%ext_kind_(4), self%ext_name_(4), self%ext_inline_(4), stat=ierr)
   endsubroutine allocate_pools

   subroutine ensure_ready(self, ierr)
   !< Initialise an empty document (its root only) before the first edit.
   class(markdown_document), intent(inout) :: self !< Document.
   integer(I4P),             intent(out)   :: ierr !< Status.

   ierr = 0
   if (self%nodes_last > 0_I4P) return
   call self%initialize(stat=ierr)
   endsubroutine ensure_ready

   pure subroutine reserve_nodes(self, needed, ierr)
   !< Grow the node arrays, by doubling, to hold at least `needed` nodes.
   class(markdown_document), intent(inout) :: self   !< Document.
   integer(I4P),             intent(in)    :: needed !< Nodes needed.
   integer(I4P),             intent(out)   :: ierr   !< Status.
   integer(I4P)                            :: cap    !< New capacity.

   ierr = 0
   cap = size(self%kind_, kind=I4P)
   if (needed <= cap) return
   cap = new_capacity(cap, needed)
   if (cap < needed) then
      ierr = 1
      return
   endif
   call grow_ints(self%kind_, cap, ierr)
   if (ierr == 0) call grow_ints(self%parent_, cap, ierr)
   if (ierr == 0) call grow_ints(self%first_, cap, ierr)
   if (ierr == 0) call grow_ints(self%last_, cap, ierr)
   if (ierr == 0) call grow_ints(self%prev_, cap, ierr)
   if (ierr == 0) call grow_ints(self%next_, cap, ierr)
   if (ierr == 0) call grow_ints_2d(self%span_, cap, ierr)
   if (ierr == 0) call grow_ints_2d(self%ints_, cap, ierr)
   if (ierr == 0) call grow_ints_2d(self%strs_, cap, ierr)
   if (ierr == 0) call grow_ints(self%attrs_, cap, ierr)
   endsubroutine reserve_nodes

   pure subroutine put_string(self, value, id, ierr)
   !< Add a string to the pool and return its id (0 for an empty string: no storage).
   class(markdown_document), intent(inout) :: self  !< Document.
   character(*),             intent(in)    :: value !< String.
   integer(I4P),             intent(out)   :: id    !< Id.
   integer(I4P),             intent(out)   :: ierr  !< Status.
   integer(I4P)                            :: l     !< Length.
   integer(I4P)                            :: cap   !< Capacity.
   character(:), allocatable               :: chars !< Grown buffer.

   id = 0_I4P
   ierr = 0
   l = len(value, kind=I4P)
   if (l == 0_I4P) return
   if (l > huge(1_I4P) - self%chars_used) then
      ierr = 1
      return
   endif
   cap = len(self%chars_, kind=I4P)
   if (self%chars_used + l > cap) then
      cap = new_capacity(cap, self%chars_used + l)
      allocate(character(cap) :: chars, stat=ierr)
      if (ierr /= 0) return
      chars(1:self%chars_used) = self%chars_(1:self%chars_used)
      ! assignment, not move_alloc: nvfortran 26.9 does not move the length of a deferred-length character (the
      ! pool would grow at every string, and write beyond its storage after compact); reproducer in BARUCH's notes/
      self%chars_ = chars
   endif
   if (self%strings_used == size(self%string_first_, kind=I4P)) then
      cap = new_capacity(self%strings_used, self%strings_used + 1_I4P)
      call grow_ints(self%string_first_, cap, ierr)
      if (ierr == 0) call grow_ints(self%string_len_, cap, ierr)
      if (ierr /= 0) return
   endif
   self%strings_used = self%strings_used + 1_I4P
   id = self%strings_used
   self%string_first_(id) = self%chars_used + 1_I4P
   self%string_len_(id) = l
   self%chars_(self%chars_used+1:self%chars_used+l) = value
   self%chars_used = self%chars_used + l
   endsubroutine put_string

   pure function get_string(self, id) result(value)
   !< Return a string of the pool by id (empty for 0).
   class(markdown_document), intent(in) :: self  !< Document.
   integer(I4P),             intent(in) :: id    !< Id.
   character(:), allocatable            :: value !< String.

   if (id <= 0_I4P) then
      value = ''
   else
      value = self%chars_(self%string_first_(id):self%string_first_(id)+self%string_len_(id)-1)
   endif
   endfunction get_string

   pure function string_equals(self, id, value) result(equal)
   !< Return true if a pooled string equals a value, byte by byte (no blank padding).
   class(markdown_document), intent(in) :: self  !< Document.
   integer(I4P),             intent(in) :: id    !< Id.
   character(*),             intent(in) :: value !< Value.
   logical                              :: equal !< Result.
   integer(I4P)                         :: f     !< First byte.

   if (id <= 0_I4P) then
      equal = len(value) == 0
      return
   endif
   equal = self%string_len_(id) == len(value, kind=I4P)
   if (.not. equal) return
   f = self%string_first_(id)
   equal = self%chars_(f:f+self%string_len_(id)-1) == value
   endfunction string_equals

   function node_ok(self, n, proc, stat, errmsg) result(ok)
   !< Check that a handle is a live node: on failure set the status (`BARUCH_ERR_NODE`), else clear it.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Handle.
   character(*),              intent(in)            :: proc   !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   logical                                          :: ok     !< Result.

   ok = .false.
   if (n < 1_I4P .or. n > self%nodes_last) then
      call status_error(BARUCH_ERR_NODE, 'markdown_document%'//proc//': '//istr(n)//' is not a node (the document has '// &
                        istr(self%nodes_last)//')', stat, errmsg)
   elseif (self%kind_(n) == NODE_NONE) then
      call status_error(BARUCH_ERR_NODE, 'markdown_document%'//proc//': node '//istr(n)// &
                        ' was unlinked (a tombstone until compact)', stat, errmsg)
   else
      ok = .true.
      call status_ok(stat, errmsg)
   endif
   endfunction node_ok

   function kind_ok(self, n, kinds, what, proc, stat, errmsg, extensions) result(ok)
   !< Check that a handle is a live node of one of `kinds` (or of an extension kind, if `extensions`).
   class(markdown_document),  intent(in)            :: self       !< Document.
   integer(I4P),              intent(in)            :: n          !< Handle.
   integer(I4P),              intent(in)            :: kinds(:)   !< Accepted kinds.
   character(*),              intent(in)            :: what       !< The accepted kinds, in words.
   character(*),              intent(in)            :: proc       !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat       !< Status.
   character(:), allocatable, intent(out), optional :: errmsg     !< Message.
   logical,                   intent(in),  optional :: extensions !< Accept extension kinds too.
   logical                                          :: ok         !< Result.
   logical                                          :: ext        !< Accept extension kinds.

   ok = self%node_ok(n, proc, stat, errmsg)
   if (.not. ok) return
   ext = .false.
   if (present(extensions)) ext = extensions
   ok = any(kinds == self%kind_(n)) .or. (ext .and. self%kind_(n) >= NODE_EXTENSION_FIRST)
   if (.not. ok) call status_error(BARUCH_ERR_KIND, 'markdown_document%'//proc//': node '//istr(n)//' is '// &
                                   article(self%kind_name(self%kind_(n)))//', not '//what, stat, errmsg)
   endfunction kind_ok

   function link_ok(self, parent, child, proc, stat, errmsg) result(ok)
   !< Check that `child` may become a child of `parent`: both live, `child` not the root, not `parent` itself nor
   !< one of its ancestors (the ancestors are walked only when `child` has children: linking a leaf is O(1)).
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: parent !< Parent to be.
   integer(I4P),              intent(in)            :: child  !< Node to link.
   character(*),              intent(in)            :: proc   !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   logical                                          :: ok     !< Result.
   integer(I4P)                                     :: a      !< Ancestor.

   ok = self%node_ok(parent, proc, stat, errmsg)
   if (ok) ok = self%node_ok(child, proc, stat, errmsg)
   if (.not. ok) return
   ok = .false.
   if (self%kind_(child) == NODE_DOCUMENT) then
      call status_error(BARUCH_ERR_LINK, 'markdown_document%'//proc//': node '//istr(child)//' is the root', stat, errmsg)
      return
   endif
   if (child == parent) then
      call status_error(BARUCH_ERR_LINK, 'markdown_document%'//proc//': node '//istr(child)// &
                        ' cannot be its own child', stat, errmsg)
      return
   endif
   if (self%first_(child) /= 0_I4P) then
      a = self%parent_(parent)
      do while (a /= 0_I4P)
         if (a == child) then
            call status_error(BARUCH_ERR_LINK, 'markdown_document%'//proc//': node '//istr(child)// &
                              ' is an ancestor of node '//istr(parent)//' (a cycle)', stat, errmsg)
            return
         endif
         a = self%parent_(a)
      enddo
   endif
   ok = .true.
   endfunction link_ok

   pure subroutine insert_before_trusted(self, sibling, node)
   !< `insert_before` with no check, for the library's own parsers (not exported by `baruch`): the caller guarantees
   !< that `sibling` is a live node with a parent and `node` a live node that is neither `sibling`'s parent nor one of
   !< its ancestors. `insert_before`'s cycle check walks the ancestors of the parent when `node` has children: O(depth)
   !< per call, quadratic for a parser that links one node with children per emphasis or link of a paragraph nested
   !< 10^5 block quotes deep (plan szaghi/BARUCH#1, step 2.6, "emph in deep blockquote").
   class(markdown_document), intent(inout) :: self    !< Document.
   integer(I4P),             intent(in)    :: sibling !< Sibling.
   integer(I4P),             intent(in)    :: node    !< Node to link.
   integer(I4P)                            :: p       !< Parent.
   integer(I4P)                            :: s       !< Previous sibling of `sibling`.

   call self%detach(node)
   p = self%parent_(sibling)
   s = self%prev_(sibling)
   self%parent_(node) = p
   self%prev_(node) = s
   self%next_(node) = sibling
   self%prev_(sibling) = node
   if (s /= 0_I4P) then
      self%next_(s) = node
   else
      self%first_(p) = node
   endif
   endsubroutine insert_before_trusted

   pure subroutine insert_after_trusted(self, sibling, node)
   !< `insert_after` with no check, for the library's own parsers (not exported by `baruch`); the caller's guarantees
   !< are those of `insert_before_trusted`.
   class(markdown_document), intent(inout) :: self    !< Document.
   integer(I4P),             intent(in)    :: sibling !< Sibling.
   integer(I4P),             intent(in)    :: node    !< Node to link.
   integer(I4P)                            :: p       !< Parent.
   integer(I4P)                            :: s       !< Next sibling of `sibling`.

   call self%detach(node)
   p = self%parent_(sibling)
   s = self%next_(sibling)
   self%parent_(node) = p
   self%prev_(node) = sibling
   self%next_(node) = s
   self%next_(sibling) = node
   if (s /= 0_I4P) then
      self%prev_(s) = node
   else
      self%last_(p) = node
   endif
   endsubroutine insert_after_trusted

   pure subroutine detach(self, n)
   !< Take a node out of its parent's children (a detached node is left as it is); its subtree goes with it.
   class(markdown_document), intent(inout) :: self !< Document.
   integer(I4P),             intent(in)    :: n    !< Node.
   integer(I4P)                            :: p    !< Parent.
   integer(I4P)                            :: s    !< Previous sibling.
   integer(I4P)                            :: t    !< Next sibling.

   p = self%parent_(n)
   s = self%prev_(n)
   t = self%next_(n)
   if (s /= 0_I4P) then
      self%next_(s) = t
   elseif (p /= 0_I4P) then
      self%first_(p) = t
   endif
   if (t /= 0_I4P) then
      self%prev_(t) = s
   elseif (p /= 0_I4P) then
      self%last_(p) = s
   endif
   self%parent_(n) = 0_I4P
   self%prev_(n) = 0_I4P
   self%next_(n) = 0_I4P
   endsubroutine detach

   pure subroutine find_attribute(self, n, name, a, prev)
   !< Return the record of a named attribute of a live node (0 if none) and the record before it (0 if first).
   class(markdown_document), intent(in)  :: self !< Document.
   integer(I4P),             intent(in)  :: n    !< Node.
   character(*),             intent(in)  :: name !< Name.
   integer(I4P),             intent(out) :: a    !< Record.
   integer(I4P),             intent(out) :: prev !< Previous record.

   prev = 0_I4P
   a = self%attrs_(n)
   do while (a /= 0_I4P)
      if (self%string_equals(self%attr_name_(a), name)) return
      prev = a
      a = self%attr_next_(a)
   enddo
   prev = 0_I4P
   endsubroutine find_attribute

   function nth_attribute(self, n, i, proc, stat, errmsg) result(a)
   !< Return the record of the i-th attribute of a node; 0 and an error if `i` is out of range.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: i      !< Position.
   character(*),              intent(in)            :: proc   !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: a      !< Record.
   integer(I4P)                                     :: k      !< Counter.

   a = 0_I4P
   if (.not. self%node_ok(n, proc, stat, errmsg)) return
   if (i >= 1_I4P) then
      a = self%attrs_(n)
      k = 1_I4P
      do while (a /= 0_I4P .and. k < i)
         a = self%attr_next_(a)
         k = k + 1_I4P
      enddo
   endif
   if (a == 0_I4P) call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%'//proc//': node '//istr(n)// &
                                     ' has no attribute '//istr(i), stat, errmsg)
   endfunction nth_attribute

   subroutine set_source(self, bytes, proc, encoding, stat, errmsg)
   !< Sanitise bytes, keep them as the source, index the lines and reset the tree to its root; an unsupported encoding
   !< leaves the document as it was.
   class(markdown_document),  intent(inout)         :: self     !< Document.
   character(*),              intent(in)            :: bytes    !< Bytes.
   character(*),              intent(in)            :: proc     !< Name of the calling procedure.
   character(*),              intent(in),  optional :: encoding !< Encoding name or alias.
   integer(I4P),              intent(out), optional :: stat     !< Status.
   character(:), allocatable, intent(out), optional :: errmsg   !< Message.
   character(:), allocatable                        :: text     !< Sanitised bytes.
   character(:), allocatable                        :: msg      !< Message of the sanitising.
   integer(I4P)                                     :: ierr     !< Status of the sanitising.

   call sanitize(bytes, encoding, text, ierr, msg)
   if (ierr /= 0_I4P) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%'//proc//': '//msg, stat, errmsg)
      return
   endif
   call self%initialize(stat=ierr)
   if (ierr /= 0_I4P) then
      call status_error(BARUCH_ERR_ALLOCATION, 'markdown_document%'//proc//': cannot allocate the document', &
                        stat, errmsg)
      return
   endif
   self%source_ = text
   call index_lines(self%source_, self%line_first_, self%line_last_)
   call status_ok(stat, errmsg)
   endsubroutine set_source

   function line_ok(self, i, proc, stat, errmsg) result(ok)
   !< Check that a line number is a line of the source: on failure set the status (`BARUCH_ERR_ARGUMENT`), else clear
   !< it.
   class(markdown_document),  intent(in)            :: self   !< Document.
   integer(I4P),              intent(in)            :: i      !< Line number.
   character(*),              intent(in)            :: proc   !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   logical                                          :: ok     !< Result.

   ok = i >= 1_I4P .and. i <= self%lines_number()
   if (ok) then
      call status_ok(stat, errmsg)
   else
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%'//proc//': '//istr(i)// &
                        ' is not a line (the source has '//istr(self%lines_number())//')', stat, errmsg)
   endif
   endfunction line_ok

   ! non type-bound helpers
   pure subroutine destroy_tree(self)
   !< Free the nodes, the pools and the registered kinds; keep the source.
   class(markdown_document), intent(inout) :: self !< Document.

   self%nodes_last = 0_I4P
   self%nodes_live = 0_I4P
   if (allocated(self%kind_)) deallocate(self%kind_)
   if (allocated(self%parent_)) deallocate(self%parent_)
   if (allocated(self%first_)) deallocate(self%first_)
   if (allocated(self%last_)) deallocate(self%last_)
   if (allocated(self%prev_)) deallocate(self%prev_)
   if (allocated(self%next_)) deallocate(self%next_)
   if (allocated(self%span_)) deallocate(self%span_)
   if (allocated(self%ints_)) deallocate(self%ints_)
   if (allocated(self%strs_)) deallocate(self%strs_)
   if (allocated(self%attrs_)) deallocate(self%attrs_)
   if (allocated(self%chars_)) deallocate(self%chars_)
   self%chars_used = 0_I4P
   if (allocated(self%string_first_)) deallocate(self%string_first_)
   if (allocated(self%string_len_)) deallocate(self%string_len_)
   self%strings_used = 0_I4P
   if (allocated(self%attr_name_)) deallocate(self%attr_name_)
   if (allocated(self%attr_value_)) deallocate(self%attr_value_)
   if (allocated(self%attr_next_)) deallocate(self%attr_next_)
   self%attrs_used = 0_I4P
   if (allocated(self%ext_kind_)) deallocate(self%ext_kind_)
   if (allocated(self%ext_name_)) deallocate(self%ext_name_)
   if (allocated(self%ext_inline_)) deallocate(self%ext_inline_)
   self%ext_used = 0_I4P
   endsubroutine destroy_tree

   pure subroutine clear_node(self, n, kind)
   !< Give a node a kind and nothing else: no links, no span, zero slots, no attributes.
   type(markdown_document), intent(inout) :: self !< Document.
   integer(I4P),            intent(in)    :: n    !< Node.
   integer(I4P),            intent(in)    :: kind !< Kind (`NODE_NONE`: a tombstone).

   self%kind_(n) = kind
   self%parent_(n) = 0_I4P
   self%first_(n) = 0_I4P
   self%last_(n) = 0_I4P
   self%prev_(n) = 0_I4P
   self%next_(n) = 0_I4P
   self%span_(1, n) = 0_I4P
   self%span_(2, n) = -1_I4P
   self%ints_(:, n) = 0_I4P
   self%strs_(:, n) = 0_I4P
   self%attrs_(n) = 0_I4P
   endsubroutine clear_node

   subroutine store_string(self, n, slot, value, proc, stat, errmsg)
   !< Put a string in the pool and in a string slot of a node.
   class(markdown_document),  intent(inout)         :: self   !< Document.
   integer(I4P),              intent(in)            :: n      !< Node.
   integer(I4P),              intent(in)            :: slot   !< Slot.
   character(*),              intent(in)            :: value  !< Value.
   character(*),              intent(in)            :: proc   !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: id     !< String id.
   integer(I4P)                                     :: ierr   !< Status.

   call self%put_string(value, id, ierr)
   if (ierr /= 0) then
      call status_error(BARUCH_ERR_ALLOCATION, 'markdown_document%'//proc//': cannot store '//istr(len(value, I4P))// &
                        ' bytes for node '//istr(n), stat, errmsg)
      return
   endif
   self%strs_(slot, n) = id
   endsubroutine store_string

   function sibling_ok(self, sibling, node, proc, stat, errmsg) result(ok)
   !< Check the sibling of an insertion: a live node with a parent, not the node to insert.
   class(markdown_document),  intent(in)            :: self    !< Document.
   integer(I4P),              intent(in)            :: sibling !< Sibling.
   integer(I4P),              intent(in)            :: node    !< Node to insert.
   character(*),              intent(in)            :: proc    !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat    !< Status.
   character(:), allocatable, intent(out), optional :: errmsg  !< Message.
   logical                                          :: ok      !< Result.

   ok = self%node_ok(sibling, proc, stat, errmsg)
   if (.not. ok) return
   ok = .false.
   if (self%parent_(sibling) == 0_I4P) then
      call status_error(BARUCH_ERR_LINK, 'markdown_document%'//proc//': node '//istr(sibling)// &
                        ' has no parent (the root or a detached node)', stat, errmsg)
   elseif (sibling == node) then
      call status_error(BARUCH_ERR_LINK, 'markdown_document%'//proc//': node '//istr(node)// &
                        ' cannot be its own sibling', stat, errmsg)
   else
      ok = .true.
   endif
   endfunction sibling_ok

   function slot_ok(slot, slots, proc, stat, errmsg) result(ok)
   !< Check a slot number.
   integer(I4P),              intent(in)            :: slot   !< Slot.
   integer(I4P),              intent(in)            :: slots  !< Slots of a node.
   character(*),              intent(in)            :: proc   !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   logical                                          :: ok     !< Result.

   ok = slot >= 1_I4P .and. slot <= slots
   if (.not. ok) call status_error(BARUCH_ERR_ARGUMENT, 'markdown_document%'//proc//': slot '//istr(slot)// &
                                   ' is not 1 to '//istr(slots), stat, errmsg)
   endfunction slot_ok

   pure function ext_index(self, kind) result(e)
   !< Return the index of a registered kind, 0 if not registered.
   type(markdown_document), intent(in) :: self !< Document.
   integer(I4P),            intent(in) :: kind !< Kind.
   integer(I4P)                        :: e    !< Index.

   do e=1, self%ext_used
      if (self%ext_kind_(e) == kind) return
   enddo
   e = 0_I4P
   endfunction ext_index

   pure function new_capacity(current, needed) result(cap)
   !< Double a capacity until it holds `needed` (capped at the largest I4P).
   integer(I4P), intent(in) :: current !< Current capacity.
   integer(I4P), intent(in) :: needed  !< Needed.
   integer(I4P)             :: cap     !< New capacity.

   cap = max(1_I4P, current)
   do while (cap < needed)
      if (cap > huge(1_I4P) - cap) then ! doubling would overflow
         cap = huge(1_I4P)
         exit
      endif
      cap = 2_I4P * cap
   enddo
   endfunction new_capacity

   pure subroutine grow_ints(array, capacity, ierr)
   !< Grow an integer array to a capacity, keeping its values, the new ones zero.
   integer(I4P), allocatable, intent(inout) :: array(:) !< Array.
   integer(I4P),              intent(in)    :: capacity !< New capacity.
   integer(I4P),              intent(out)   :: ierr     !< Status.
   integer(I4P), allocatable                :: grown(:) !< Grown array.
   integer(I4P)                             :: n        !< Old size.

   n = size(array, kind=I4P)
   allocate(grown(max(capacity, n)), stat=ierr)
   if (ierr /= 0) return
   grown(1:n) = array
   grown(n+1:) = 0_I4P
   call move_alloc(grown, array)
   endsubroutine grow_ints

   pure subroutine grow_ints_2d(array, capacity, ierr)
   !< Grow the second dimension of an integer array to a capacity, keeping its values, the new ones zero.
   integer(I4P), allocatable, intent(inout) :: array(:,:) !< Array.
   integer(I4P),              intent(in)    :: capacity   !< New capacity.
   integer(I4P),              intent(out)   :: ierr       !< Status.
   integer(I4P), allocatable                :: grown(:,:) !< Grown array.
   integer(I4P)                             :: n          !< Old size.

   n = size(array, dim=2, kind=I4P)
   allocate(grown(size(array, dim=1), max(capacity, n)), stat=ierr)
   if (ierr /= 0) return
   grown(:, 1:n) = array
   grown(:, n+1:) = 0_I4P
   call move_alloc(grown, array)
   endsubroutine grow_ints_2d

   pure function istr(i) result(s)
   !< An integer as text, no blanks.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(16)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr

   pure function article(name) result(phrase)
   !< A kind name with its article ("a paragraph", "an image").
   character(*), intent(in)  :: name   !< Name.
   character(:), allocatable :: phrase !< Phrase.

   if (len(name) == 0) then
      phrase = 'of an unknown kind'
   elseif (index('aeiou', name(1:1)) > 0) then
      phrase = 'an '//name
   else
      phrase = 'a '//name
   endif
   endfunction article
endmodule baruch_document
