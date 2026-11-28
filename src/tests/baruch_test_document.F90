!< BARUCH test: the document, its links, accessors, attributes, kinds, copy and compaction.
program baruch_test_document
!< BARUCH test: the document, its links, accessors, attributes, kinds, copy and compaction.
!<
!< The gate of plan step 1.1: a 10^6-node tree, about half of it unlinked, compacted: node count, sibling order and
!< the pool strings checked exactly; accessors of the wrong kind return a nonzero `stat` and a message, never stop;
!< a 10^5-deep chain is unlinked and compacted with no recursion.
use baruch
use penf, only : I4P
implicit none

call test_empty
call test_by_hand
call test_misuse
call test_editing
call test_attributes
call test_kinds
call test_copy
call test_million
call test_deep_chain
print '(A)', 'baruch_test_document: ok'

contains
   subroutine test_empty()
   !< An empty document: no root, every handle is an error, the first edit creates the root.
   type(markdown_document)   :: doc    !< Document.
   integer(I4P)              :: n      !< Node.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.

   call check(doc%root() == 0_I4P, 'empty: root is not 0')
   call check(doc%nodes_number() == 0_I4P, 'empty: nodes_number is not 0')
   n = doc%parent(1_I4P, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_NODE .and. len(errmsg) > 0, 'empty: parent(1) is not BARUCH_ERR_NODE')
   n = doc%new_node(NODE_PARAGRAPH, stat=stat)
   call check(stat == BARUCH_OK .and. n == 2_I4P, 'empty: the first new_node is not 2')
   call check(doc%root() == 1_I4P .and. doc%node_kind(1_I4P) == NODE_DOCUMENT, 'empty: the root is not a document')
   call check(doc%nodes_number() == 2_I4P, 'empty: nodes_number is not 2')
   call doc%destroy
   call check(doc%root() == 0_I4P .and. doc%last_node() == 0_I4P, 'empty: destroy left nodes')
   endsubroutine test_empty

   subroutine test_by_hand()
   !< A heading and a paragraph with emphasis, built node by node and read back.
   type(markdown_document) :: doc       !< Document.
   integer(I4P)            :: h, p, e   !< Heading, paragraph, emphasis.
   integer(I4P)            :: t(4)      !< Texts.
   integer(I4P)            :: c         !< Child.
   integer(I4P)            :: stat      !< Status.

   call doc%initialize(capacity=2_I4P, stat=stat) ! a tiny capacity: every array grows
   call check(stat == BARUCH_OK, 'by hand: initialize')
   h = doc%new_node(NODE_HEADING)
   call doc%set_heading_level(h, 2_I4P, stat=stat)
   call check(stat == BARUCH_OK, 'by hand: set_heading_level')
   call doc%set_int_slot(h, SLOT_HEADING_FORM, HEADING_ATX)
   t(1) = doc%new_node(NODE_TEXT)
   call doc%set_literal(t(1), 'Title')
   call doc%append_child(h, t(1))
   call doc%append_child(doc%root(), h)
   p = doc%new_node(NODE_PARAGRAPH)
   call doc%append_child(doc%root(), p)
   t(2) = doc%new_node(NODE_TEXT)
   call doc%set_literal(t(2), 'Some ')
   e = doc%new_node(NODE_EMPH)
   call doc%set_int_slot(e, SLOT_EMPHASIS_CHAR, iachar('*', I4P))
   t(3) = doc%new_node(NODE_TEXT)
   call doc%set_literal(t(3), 'emphasis')
   t(4) = doc%new_node(NODE_TEXT)
   call doc%set_literal(t(4), '.')
   call doc%append_child(p, t(2))
   call doc%append_child(p, t(4))
   call doc%insert_before(t(4), e, stat=stat)
   call check(stat == BARUCH_OK, 'by hand: insert_before')
   call doc%append_child(e, t(3))
   call doc%set_span(t(3), 9_I4P, 16_I4P)
   call check(doc%nodes_number() == 8_I4P, 'by hand: nodes_number is not 8')
   call check(doc%heading_level(h) == 2_I4P, 'by hand: heading_level')
   call check(doc%int_slot(h, SLOT_HEADING_FORM) == HEADING_ATX, 'by hand: heading form')
   call check(doc%int_slot(e, SLOT_EMPHASIS_CHAR) == iachar('*', I4P), 'by hand: emphasis char')
   call check(doc%first_child(doc%root()) == h .and. doc%last_child(doc%root()) == p, 'by hand: root children')
   call check(doc%children_number(p) == 3_I4P, 'by hand: paragraph children')
   c = doc%first_child(p)
   call check(c == t(2) .and. doc%next_sibling(c) == e .and. doc%next_sibling(e) == t(4), 'by hand: sibling order')
   call check(doc%prev_sibling(t(4)) == e .and. doc%prev_sibling(t(2)) == 0_I4P, 'by hand: prev_sibling')
   call check(doc%parent(t(3)) == e .and. doc%parent(e) == p .and. doc%parent(doc%root()) == 0_I4P, 'by hand: parent')
   call check_string(doc%literal(t(1))//doc%literal(t(2))//doc%literal(t(3))//doc%literal(t(4)), &
                     'TitleSome emphasis.', 'by hand: literals')
   call check(doc%span_first(t(3)) == 9_I4P .and. doc%span_last(t(3)) == 16_I4P, 'by hand: span')
   call check(doc%span_first(t(2)) == 0_I4P .and. doc%span_last(t(2)) == -1_I4P, 'by hand: no span')
   call check_string(doc%kind_name(doc%node_kind(e)), 'emph', 'by hand: kind_name')
   call check(doc%kind_is_inline(NODE_TEXT) .and. .not. doc%kind_is_inline(NODE_PARAGRAPH), 'by hand: kind_is_inline')
   endsubroutine test_by_hand

   subroutine test_misuse()
   !< Accessors of the wrong kind, handles that are not nodes, values out of range: a status and a message, no stop.
   type(markdown_document)   :: doc    !< Document.
   integer(I4P)              :: p, l, t !< Paragraph, list, text.
   integer(I4P)              :: v      !< Value.
   integer(I4P)              :: stat   !< Status.
   character(:), allocatable :: errmsg !< Message.
   character(:), allocatable :: s      !< String.
   logical                   :: tight  !< A list's tightness.

   p = doc%new_node(NODE_PARAGRAPH)
   l = doc%new_node(NODE_LIST)
   t = doc%new_node(NODE_TEXT)
   ! wrong kind
   v = doc%heading_level(p, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_KIND .and. v == 0_I4P, 'misuse: heading_level of a paragraph')
   call check_string(errmsg, 'markdown_document%heading_level: node 2 is a paragraph, not a heading', &
                     'misuse: the heading_level message')
   v = doc%heading_level(p) ! no stat: the default result, no stop
   call check(v == 0_I4P, 'misuse: heading_level without stat')
   s = doc%literal(p, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_KIND .and. len(s) == 0 .and. index(errmsg, 'paragraph') > 0, 'misuse: literal')
   call doc%set_literal(l, 'x', stat=stat)
   call check(stat == BARUCH_ERR_KIND, 'misuse: set_literal of a list')
   s = doc%destination(t, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_KIND .and. index(errmsg, 'a text, not a link') > 0, 'misuse: destination of a text')
   call doc%set_info(t, 'fortran', stat=stat)
   call check(stat == BARUCH_ERR_KIND, 'misuse: set_info of a text')
   tight = doc%list_tight(p, stat=stat)
   call check(.not. tight .and. stat == BARUCH_ERR_KIND, 'misuse: list_tight of a paragraph')
   ! values out of range
   call doc%set_list_type(l, 7_I4P, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'misuse: list type 7')
   call doc%set_list_start(l, -1_I4P, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'misuse: list start -1')
   v = doc%int_slot(p, NODE_INT_SLOTS + 1_I4P, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'misuse: int slot out of range')
   call doc%set_string_slot(p, 0_I4P, 'x', stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'misuse: string slot 0')
   call doc%set_span(p, 5_I4P, 3_I4P, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'misuse: span 5:3')
   v = doc%new_node(NODE_NONE, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT .and. v == 0_I4P, 'misuse: new_node of NODE_NONE')
   v = doc%new_node(NODE_DOCUMENT, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT .and. v == 0_I4P, 'misuse: a second document node')
   v = doc%new_node(NODE_EXTENSION_FIRST, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'misuse: new_node of an unregistered kind')
   ! handles that are not nodes
   v = doc%node_kind(0_I4P, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_NODE .and. v == NODE_NONE .and. len(errmsg) > 0, 'misuse: node 0')
   v = doc%first_child(1000_I4P, stat=stat)
   call check(stat == BARUCH_ERR_NODE, 'misuse: node 1000')
   call doc%append_child(doc%root(), t)
   call doc%unlink(t, stat=stat)
   call check(stat == BARUCH_OK .and. .not. doc%is_node(t), 'misuse: unlink')
   s = doc%literal(t, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_NODE .and. index(errmsg, 'tombstone') > 0, 'misuse: literal of a tombstone')
   ! success clears the status
   call doc%set_list_type(l, LIST_ORDERED, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK .and. len(errmsg) == 0, 'misuse: success does not clear the status')
   endsubroutine test_misuse

   subroutine test_editing()
   !< Prepend, insert after, moves, cycles refused, the root protected, subtree unlink, typed setters.
   type(markdown_document) :: doc        !< Document.
   integer(I4P)            :: q, l, i(3) !< Quote, list, items.
   integer(I4P)            :: p, c       !< Paragraph, code block.
   integer(I4P)            :: stat       !< Status.

   q = doc%new_node(NODE_BLOCK_QUOTE)
   l = doc%new_node(NODE_LIST)
   call doc%set_list_type(l, LIST_ORDERED)
   call doc%set_list_start(l, 3_I4P)
   call doc%set_list_tight(l, .true.)
   call doc%set_int_slot(l, SLOT_LIST_MARKER, iachar(')', I4P))
   i(1) = doc%new_node(NODE_ITEM)
   i(2) = doc%new_node(NODE_ITEM)
   i(3) = doc%new_node(NODE_ITEM)
   call doc%append_child(doc%root(), q)
   call doc%append_child(q, l)
   call doc%append_child(l, i(2))
   call doc%prepend_child(l, i(1))
   call doc%insert_after(i(2), i(3))
   call check(doc%first_child(l) == i(1) .and. doc%next_sibling(i(1)) == i(2) .and. &
              doc%next_sibling(i(2)) == i(3) .and. doc%last_child(l) == i(3), 'editing: item order')
   call check(doc%list_type(l) == LIST_ORDERED .and. doc%list_start(l) == 3_I4P .and. doc%list_tight(l), &
              'editing: list fields')
   ! a move: the first item becomes the last, no tombstone
   call doc%append_child(l, i(1))
   call check(doc%first_child(l) == i(2) .and. doc%last_child(l) == i(1) .and. doc%prev_sibling(i(1)) == i(3) .and. &
              doc%nodes_number() == 6_I4P, 'editing: move')
   ! a cycle: the quote under its own grandchild
   call doc%append_child(i(2), q, stat=stat)
   call check(stat == BARUCH_ERR_LINK, 'editing: cycle not refused')
   call doc%append_child(q, q, stat=stat)
   call check(stat == BARUCH_ERR_LINK, 'editing: self child not refused')
   call doc%insert_before(doc%root(), l, stat=stat)
   call check(stat == BARUCH_ERR_LINK, 'editing: insert before the root not refused')
   call doc%append_child(q, doc%root(), stat=stat)
   call check(stat == BARUCH_ERR_LINK, 'editing: root moved')
   call doc%unlink(doc%root(), stat=stat)
   call check(stat == BARUCH_ERR_LINK, 'editing: root unlinked')
   ! typed strings
   c = doc%new_node(NODE_CODE_BLOCK)
   call doc%set_literal(c, 'print *, 1'//new_line('a'))
   call doc%set_info(c, 'fortran')
   call doc%set_int_slot(c, SLOT_CODE_FORM, CODE_FENCED)
   call doc%append_child(i(3), c)
   call check_string(doc%info(c), 'fortran', 'editing: info')
   p = doc%new_node(NODE_LINK)
   call doc%set_destination(p, 'https://commonmark.org')
   call doc%set_title(p, 'CommonMark')
   call doc%set_label(p, 'cm')
   call check_string(doc%destination(p)//'|'//doc%title(p)//'|'//doc%label(p), 'https://commonmark.org|CommonMark|cm', &
                     'editing: link strings')
   call check_string(doc%string_slot(p, STRING_TITLE), 'CommonMark', 'editing: string_slot')
   ! unlink a subtree: the list, three items and the code block
   call doc%unlink(l)
   call check(doc%nodes_number() == 3_I4P .and. doc%first_child(q) == 0_I4P, 'editing: subtree unlink')
   call check(.not. (doc%is_node(l) .or. doc%is_node(i(1)) .or. doc%is_node(c)), 'editing: tombstones')
   endsubroutine test_editing

   subroutine test_attributes()
   !< Named attributes: set, replace, order, remove, empty values.
   type(markdown_document)   :: doc    !< Document.
   integer(I4P)              :: h      !< Heading.
   integer(I4P)              :: stat   !< Status.
   logical                   :: found  !< Found.
   character(:), allocatable :: s      !< String.

   h = doc%new_node(NODE_HEADING)
   call doc%set_attribute(h, 'id', 'intro')
   call doc%set_attribute(h, 'class', 'note')
   call doc%set_attribute(h, 'hidden', '')
   call doc%set_attribute(h, 'id', 'overview')
   call check(doc%attributes_number(h) == 3_I4P, 'attributes: number')
   call check_string(doc%attribute_name(h, 1_I4P)//'='//doc%attribute_value(h, 1_I4P), 'id=overview', 'attributes: 1')
   call check_string(doc%attribute_name(h, 2_I4P)//'='//doc%attribute_value(h, 2_I4P), 'class=note', 'attributes: 2')
   s = doc%attribute(h, 'hidden', found=found)
   call check(found .and. len(s) == 0, 'attributes: empty value')
   s = doc%attribute(h, 'id ', found=found) ! no blank padding in names
   call check(.not. found, 'attributes: "id " found')
   s = doc%attribute_name(h, 4_I4P, stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'attributes: a fourth one')
   call doc%remove_attribute(h, 'class')
   call check(doc%attributes_number(h) == 2_I4P .and. .not. doc%has_attribute(h, 'class') .and. &
              doc%has_attribute(h, 'hidden'), 'attributes: remove')
   call doc%set_attribute(h, '', 'x', stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'attributes: empty name')
   endsubroutine test_attributes

   subroutine test_kinds()
   !< Registered extension kinds.
   type(markdown_document) :: doc   !< Document.
   integer(I4P)            :: n, m  !< Nodes.
   integer(I4P)            :: stat  !< Status.

   call doc%register_kind(NODE_EXTENSION_FIRST, 'math_block', inline=.false., stat=stat)
   call check(stat == BARUCH_OK, 'kinds: register math_block')
   call doc%register_kind(NODE_EXTENSION_FIRST + 1_I4P, 'strikethrough', inline=.true.)
   call doc%register_kind(NODE_EXTENSION_FIRST, 'other', inline=.false., stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'kinds: number registered twice')
   call doc%register_kind(NODE_EXTENSION_FIRST + 2_I4P, 'math_block', inline=.false., stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'kinds: name registered twice')
   call doc%register_kind(NODE_PARAGRAPH, 'para', inline=.false., stat=stat)
   call check(stat == BARUCH_ERR_ARGUMENT, 'kinds: core range')
   n = doc%new_node(NODE_EXTENSION_FIRST, stat=stat)
   call check(stat == BARUCH_OK .and. doc%node_kind(n) == NODE_EXTENSION_FIRST, 'kinds: new_node of an extension')
   call doc%set_literal(n, 'e^{i pi}+1=0', stat=stat)
   call check(stat == BARUCH_OK .and. doc%literal(n) == 'e^{i pi}+1=0', 'kinds: literal of an extension node')
   m = doc%new_node(NODE_EXTENSION_FIRST + 1_I4P)
   call check_string(doc%kind_name(doc%node_kind(m)), 'strikethrough', 'kinds: kind_name')
   call check(doc%kind_is_inline(NODE_EXTENSION_FIRST + 1_I4P) .and. .not. doc%kind_is_inline(NODE_EXTENSION_FIRST), &
              'kinds: kind_is_inline')
   call check(doc%is_known_kind(NODE_EXTENSION_FIRST) .and. .not. doc%is_known_kind(NODE_EXTENSION_FIRST + 5_I4P) .and. &
              .not. doc%is_known_kind(NODE_NONE), 'kinds: is_known_kind')
   call doc%compact
   call check_string(doc%kind_name(NODE_EXTENSION_FIRST), 'math_block', 'kinds: name after compact')
   endsubroutine test_kinds

   subroutine test_copy()
   !< Assignment is a deep copy: editing the copy leaves the original as it was.
   type(markdown_document) :: doc, copy !< Documents.
   integer(I4P)            :: p, t      !< Paragraph, text.

   p = doc%new_node(NODE_PARAGRAPH)
   t = doc%new_node(NODE_TEXT)
   call doc%set_literal(t, 'original')
   call doc%set_attribute(p, 'class', 'lead')
   call doc%append_child(doc%root(), p)
   call doc%append_child(p, t)
   copy = doc
   call copy%set_literal(t, 'changed')
   call copy%set_attribute(p, 'class', 'other')
   call copy%unlink(p)
   call copy%compact
   call check(copy%nodes_number() == 1_I4P, 'copy: the copy was not edited')
   call check(doc%nodes_number() == 3_I4P .and. doc%first_child(p) == t, 'copy: the original lost its nodes')
   call check_string(doc%literal(t)//'|'//doc%attribute(p, 'class'), 'original|lead', 'copy: the original changed')
   endsubroutine test_copy

   subroutine test_million()
   !< 10^6 nodes: a root, 999 paragraphs of 1000 texts each. The even paragraphs are unlinked whole, the odd ones lose
   !< their first, 500th and last text; the second text of each odd paragraph gets a new literal (the old one dies).
   !< After `compact`: the count, the map, the sibling order, every string and the pool size, exactly.
   integer(I4P), parameter              :: PARAGRAPHS = 999_I4P   !< Paragraphs.
   integer(I4P), parameter              :: TEXTS = 1000_I4P       !< Texts per paragraph.
   type(markdown_document)              :: doc                    !< Document.
   integer(I4P), allocatable            :: map(:)                 !< Old to new.
   integer(I4P), allocatable            :: par(:)                 !< Paragraph handles.
   integer(I4P)                         :: p, j, n, t, c          !< Counters and handles.
   integer(I4P)                         :: expected_live          !< Live nodes expected.
   integer(I4P)                         :: expected_bytes         !< Pool bytes expected.
   integer(I4P)                         :: stat                   !< Status.
   character(:), allocatable            :: errmsg                 !< Message.

   allocate(par(PARAGRAPHS))
   do p=1, PARAGRAPHS
      par(p) = doc%new_node(NODE_PARAGRAPH)
      call doc%append_child(doc%root(), par(p))
      call doc%set_attribute(par(p), 'id', 'p'//istr(p))
      do j=1, TEXTS
         t = doc%new_node(NODE_TEXT)
         call doc%set_literal(t, 't'//istr(p)//'.'//istr(j))
         call doc%append_child(par(p), t)
      enddo
   enddo
   call check(doc%nodes_number() == 1000000_I4P .and. doc%last_node() == 1000000_I4P, 'million: 10^6 nodes')
   expected_live = 1_I4P
   expected_bytes = 0_I4P
   do p=1, PARAGRAPHS
      if (mod(p, 2_I4P) == 0_I4P) then
         call doc%unlink(par(p), stat=stat)
         call check(stat == BARUCH_OK, 'million: unlink a paragraph')
         cycle
      endif
      call doc%unlink(doc%first_child(par(p)))                              ! text 1
      call doc%unlink(doc%last_child(par(p)))                               ! text 1000
      t = doc%first_child(par(p))                                           ! text 2
      call doc%set_literal(t, 'T'//istr(p)//'.2')
      do j=3, 500
         t = doc%next_sibling(t)
      enddo
      call doc%unlink(t)                                                    ! text 500
      expected_live = expected_live + 1_I4P + TEXTS - 3_I4P
      expected_bytes = expected_bytes + len('id'//'p'//istr(p), I4P)
      do j=2, TEXTS - 1_I4P
         if (j == 500_I4P) cycle
         expected_bytes = expected_bytes + len(text_of(p, j), I4P)
      enddo
   enddo
   call check(doc%nodes_number() == expected_live, 'million: live nodes before compact')
   print '(A,I0,A,I0,A)', 'baruch_test_document: ', 1000000_I4P - expected_live, ' of 1000000 nodes unlinked, ', &
                          expected_live, ' left'
   call doc%compact(map=map, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK, 'million: compact')
   call check(doc%nodes_number() == expected_live .and. doc%last_node() == expected_live, 'million: count after compact')
   call check(doc%pool_bytes() == expected_bytes, 'million: pool bytes after compact')
   ! the map: tombstones to 0, live nodes to 1, 2, 3... in their old order
   call check(size(map) == 1000000 .and. map(1) == 1_I4P, 'million: map size or root')
   n = 0_I4P
   do j=1, size(map, kind=I4P)
      if (map(j) == 0_I4P) cycle
      n = n + 1_I4P
      call check(map(j) == n, 'million: map is not order preserving')
   enddo
   call check(n == expected_live, 'million: map count')
   ! sibling order and strings, through the new handles
   call check(doc%children_number(doc%root()) == (PARAGRAPHS + 1_I4P) / 2_I4P, 'million: paragraphs left')
   c = doc%first_child(doc%root())
   do p=1, PARAGRAPHS, 2
      call check(c == map(par(p)), 'million: a paragraph handle is not the mapped one')
      call check_string(doc%attribute(c, 'id'), 'p'//istr(p), 'million: paragraph id')
      call check(doc%children_number(c) == TEXTS - 3_I4P, 'million: texts left in a paragraph')
      t = doc%first_child(c)
      do j=2, TEXTS - 1_I4P
         if (j == 500_I4P) cycle
         call check(doc%parent(t) == c, 'million: parent')
         call check_string(doc%literal(t), text_of(p, j), 'million: text order')
         t = doc%next_sibling(t)
      enddo
      call check(t == 0_I4P, 'million: a paragraph has more texts')
      c = doc%next_sibling(c)
   enddo
   call check(c == 0_I4P, 'million: more paragraphs')
   ! the document keeps working after compaction
   ! the document keeps working after compaction: the pool, sized to its live strings, grows again
   t = doc%new_node(NODE_TEXT)
   call check(t == expected_live + 1_I4P, 'million: new node after compact')
   call doc%set_literal(t, repeat('x', 1000))
   do j=1, 1000
      call doc%set_attribute(t, 'a'//istr(j), istr(j))
   enddo
   call check(doc%pool_bytes() > expected_bytes + 1000_I4P, 'million: the pool did not grow after compact')
   call check_string(doc%literal(t), repeat('x', 1000), 'million: a literal after compact')
   call check_string(doc%attribute(t, 'a1000'), '1000', 'million: an attribute after compact')
   call check_string(doc%literal(doc%first_child(doc%first_child(doc%root()))), text_of(1_I4P, 2_I4P), &
                     'million: the first text after the pool grew')
   endsubroutine test_million

   subroutine test_deep_chain()
   !< A 10^5-deep chain of block quotes: moved, refused a cycle, unlinked and compacted with no recursion.
   integer(I4P), parameter :: DEPTH = 100000_I4P !< Depth.
   type(markdown_document) :: doc               !< Document.
   integer(I4P)            :: q, pq, top, n, d  !< Handles.
   integer(I4P)            :: stat              !< Status.

   top = doc%new_node(NODE_BLOCK_QUOTE)
   call doc%append_child(doc%root(), top)
   q = top
   do d=2, DEPTH
      n = doc%new_node(NODE_BLOCK_QUOTE)
      call doc%append_child(q, n)
      q = n
   enddo
   call check(doc%nodes_number() == DEPTH + 1_I4P, 'deep: chain built')
   pq = doc%parent(q)
   n = doc%new_node(NODE_TEXT)
   call doc%append_child(q, n)
   call doc%append_child(top, q, stat=stat)  ! the deepest (with a child) moved up
   call check(stat == BARUCH_OK .and. doc%parent(q) == top, 'deep: move up')
   call doc%append_child(pq, q, stat=stat)   ! and back down: 10^5 ancestors walked, no cycle
   call check(stat == BARUCH_OK .and. doc%parent(q) == pq, 'deep: move down')
   call doc%append_child(pq, top, stat=stat) ! the top under its deepest descendant: a cycle, found 10^5 levels up
   call check(stat == BARUCH_ERR_LINK, 'deep: cycle not refused')
   call doc%unlink(top)
   call check(doc%nodes_number() == 1_I4P, 'deep: chain unlinked')
   call doc%compact
   call check(doc%last_node() == 1_I4P .and. doc%first_child(doc%root()) == 0_I4P, 'deep: compact')
   endsubroutine test_deep_chain

   pure function text_of(p, j) result(s)
   !< The literal of text j of paragraph p after the edits of `test_million`.
   integer(I4P), intent(in)  :: p, j !< Paragraph, text.
   character(:), allocatable :: s    !< Literal.

   if (j == 2_I4P) then
      s = 'T'//istr(p)//'.2'
   else
      s = 't'//istr(p)//'.'//istr(j)
   endif
   endfunction text_of

   pure function istr(i) result(s)
   !< An integer as text, no blanks.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(16)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr

   subroutine check(condition, what)
   !< Stop the test if a condition is false.
   logical,      intent(in) :: condition !< Condition.
   character(*), intent(in) :: what      !< What failed.

   if (.not. condition) then
      print '(A)', 'baruch_test_document: FAILED, '//what
      error stop 1
   endif
   endsubroutine check

   subroutine check_string(got, expected, what)
   !< Stop the test if two strings differ (length included).
   character(*), intent(in) :: got      !< String obtained.
   character(*), intent(in) :: expected !< String expected.
   character(*), intent(in) :: what     !< What is compared.

   if (len(got) /= len(expected) .or. got /= expected) then
      print '(A)', 'baruch_test_document: FAILED, '//what//': got "'//got//'", expected "'//expected//'"'
      error stop 1
   endif
   endsubroutine check_string
endprogram baruch_test_document
