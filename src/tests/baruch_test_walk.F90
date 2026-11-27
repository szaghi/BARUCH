!< BARUCH test: the walk, every node of every fixture visited once with balanced events, inline and extension events.
program baruch_test_walk
!< BARUCH test: the walk, every node of every fixture visited once with balanced events, inline and extension events.
!<
!< The gate of plan step 1.6 for the walk: each of the 652 CommonMark examples (`src/tests/spec/commonmark-0.31.2`,
!< read from the project's root, where `scripts/run_tests.sh` and `fpm test` run) is parsed and walked; every live node
!< must be entered (or be a text) exactly once, every enter must be closed by the leave of the same node in the same
!< direction (block or span), texts only for the inline leaves; the counts are printed. Then: the events of a tree
!< built by hand with every inline kind, an extension's `walk_event` answer and the default for an extension kind with
!< no hook, the walk of a subtree, the byte offsets, the misuses and a contract violation (status, never a stop), an
!< empty document, and a 10^5-deep nesting of block quotes and of lists walked with no recursion.
use baruch
use baruch_test_extensions, only : math_extension, KIND_MATH
use penf,                   only : I4P
implicit none
character, parameter :: LF = achar(10) !< Line feed.
integer              :: failures       !< Failed checks.

failures = 0
call test_fixtures
call test_inline_events
call test_subtree_and_offsets
call test_misuse
call test_deep
if (failures > 0) then
   print '(A,I0,A)', 'baruch_test_walk: ', failures, ' failure(s)'
   error stop 1
endif
print '(A)', 'baruch_test_walk: ok'

contains
   subroutine test_fixtures()
   !< Every example of the CommonMark fixtures: each node visited once, enters and leaves balanced.
   character(*), parameter   :: DIR = 'src/tests/spec/commonmark-0.31.2/' !< Fixtures, from the project's root.
   type(markdown_document)   :: doc      !< Document.
   type(markdown_options)    :: options  !< Options.
   integer(I4P)              :: stat     !< Status.
   character(:), allocatable :: errmsg   !< Message.
   character(4)              :: stem     !< Example number on four digits.
   integer                   :: examples !< Examples in the index.
   integer                   :: k        !< Counter.
   integer                   :: bad      !< Examples failing.
   integer(I4P)              :: totals(4) !< Nodes, block pairs, span pairs, texts.
   integer(I4P)              :: counts(4) !< The same, for one example.
   logical                   :: ok       !< One example passes.

   examples = index_lines(DIR//'index.tsv') - 1
   call check(examples == 652, 'fixtures: 652 examples in the index, found '//str(examples))
   totals = 0_I4P
   bad = 0
   do k=1, examples
      write(stem, '(I4.4)') k
      call doc%load_file(DIR//'examples/'//stem//'.md', stat=stat, errmsg=errmsg)
      if (stat == BARUCH_OK) call options%parse(doc, stat=stat, errmsg=errmsg)
      if (stat /= BARUCH_OK) then
         call check(.false., 'fixtures: example '//stem//': '//errmsg)
         bad = bad + 1
         cycle
      endif
      call walk_and_count(doc, options, counts, ok, 'example '//stem)
      if (.not. ok) bad = bad + 1
      totals = totals + counts
   enddo
   print '(A,I0,A,I0,A,I0,A,I0,A,I0,A)', 'walk: ', examples - bad, '/', examples, ' fixtures, ', totals(1),        &
         ' nodes each visited once, ', totals(2) + totals(3), ' enter/leave pairs balanced, ', totals(4), ' texts'
   call check(bad == 0, 'fixtures: '//str(bad)//' example(s) walked wrong')
   endsubroutine test_fixtures

   subroutine walk_and_count(doc, options, counts, ok, label)
   !< Walk a whole document: every live node visited once, the enters closed in order by the matching leaves; return
   !< the nodes, the block and span pairs, the texts.
   type(markdown_document), intent(in)  :: doc       !< Document, parsed.
   type(markdown_options),  intent(in)  :: options   !< Options.
   integer(I4P),            intent(out) :: counts(4) !< Nodes, block pairs, span pairs, texts.
   logical,                 intent(out) :: ok        !< Everything held.
   character(*),            intent(in)  :: label     !< What is walked, for the messages.
   integer(I4P), allocatable            :: visits(:) !< Visits of each node.
   integer(I4P), allocatable            :: stack(:)  !< Nodes entered and not left.
   integer(I4P), allocatable            :: kinds(:)  !< Event that entered each of them.
   integer(I4P)                         :: depth     !< Stack depth.
   integer(I4P)                         :: node      !< Current node.
   integer(I4P)                         :: event     !< Current event.
   integer(I4P)                         :: stat      !< Status.
   character(:), allocatable            :: errmsg    !< Message.

   ok = .true.
   counts = 0_I4P
   allocate(visits(max(1_I4P, doc%last_node())), source=0_I4P)
   allocate(stack(doc%last_node() + 1_I4P), kinds(doc%last_node() + 1_I4P))
   depth = 0_I4P
   node = 0_I4P
   event = EVENT_NONE
   do
      call options%walk(doc, node, event, stat=stat, errmsg=errmsg)
      if (stat /= BARUCH_OK) then
         call walk_failed(ok, label, 'stat '//str(stat)//' '//errmsg)
         exit
      endif
      if (node == 0_I4P) exit
      select case(event)
      case(EVENT_ENTER_BLOCK, EVENT_ENTER_SPAN, EVENT_TEXT)
         visits(node) = visits(node) + 1_I4P
         if (event == EVENT_TEXT) then
            counts(4) = counts(4) + 1_I4P
            if (.not. is_inline_leaf(doc%node_kind(node))) &
               call walk_failed(ok, label, 'a text event for '//doc%kind_name(doc%node_kind(node)))
         else
            depth = depth + 1_I4P
            stack(depth) = node
            kinds(depth) = event
         endif
      case(EVENT_LEAVE_BLOCK, EVENT_LEAVE_SPAN)
         if (depth == 0_I4P) then
            call walk_failed(ok, label, 'a leave with nothing entered')
            exit
         endif
         if (stack(depth) /= node .or. kinds(depth) + 1_I4P /= event) then
            call walk_failed(ok, label, 'leave of node '//str(node)//' does not match the enter of node '//str(stack(depth)))
            exit
         endif
         if (event == EVENT_LEAVE_BLOCK) counts(2) = counts(2) + 1_I4P
         if (event == EVENT_LEAVE_SPAN) counts(3) = counts(3) + 1_I4P
         depth = depth - 1_I4P
      case default
         call walk_failed(ok, label, 'event '//str(event))
         exit
      endselect
   enddo
   if (depth /= 0_I4P) call walk_failed(ok, label, str(depth)//' node(s) entered and never left')
   if (any(visits(1:doc%last_node()) /= 1_I4P)) call walk_failed(ok, label, 'a node visited '//str(minval(visits))//' to '// &
                                                          str(maxval(visits))//' times')
   counts(1) = count(visits == 1_I4P)
   if (counts(1) /= doc%nodes_number()) call walk_failed(ok, label, str(counts(1))//' nodes visited of '//str(doc%nodes_number()))
   endsubroutine walk_and_count

   subroutine walk_failed(ok, label, what)
   !< Record the first failure of a walk.
   logical,      intent(inout) :: ok    !< The walk passes so far.
   character(*), intent(in)    :: label !< What is walked.
   character(*), intent(in)    :: what  !< What went wrong.

   if (ok) call check(.false., 'walk: '//label//': '//what)
   ok = .false.
   endsubroutine walk_failed

   subroutine test_inline_events()
   !< A tree built by hand with every inline kind and an extension's inline node: the exact sequence of events, with and
   !< without the extension's `walk_event` answer.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: options !< Options with the math extension.
   type(markdown_options)    :: core    !< Options with no extension.
   type(math_extension)      :: math    !< Extension.
   integer(I4P)              :: counts(4) !< Counts of a walk.
   logical                   :: ok        !< A walk passed.

   call options%register(math)
   call build_inlines(doc, options)
   call check(events(doc, options) == '+document +paragraph =text <emph =text >emph <strong <link =code >link >strong '// &
              '=softbreak =linebreak =html_inline <image =text >image =math -paragraph -document', &
              'inline: events with the extension "'//events(doc, options)//'"')
   call check(events(doc, core) == '+document +paragraph =text <emph =text >emph <strong <link =code >link >strong '// &
              '=softbreak =linebreak =html_inline <image =text >image <math >math -paragraph -document', &
              'inline: an inline extension kind with no hook is a span "'//events(doc, core)//'"')
   call walk_and_count(doc, options, counts, ok, 'inlines')
   call check(ok .and. counts(1) == doc%nodes_number(), 'inline: every node once')
   endsubroutine test_inline_events

   subroutine build_inlines(doc, options)
   !< A paragraph of every inline kind: `a`, *b*, **[`c`](/u)**, soft break, hard break, `<i>`, ![d](/i), math.
   type(markdown_document), intent(inout) :: doc     !< Document.
   type(markdown_options),  intent(in)    :: options !< Options: the kinds of their extensions registered.
   integer(I4P)                           :: p, n, s !< Nodes.

   call doc%initialize()
   call options%register_kinds(doc)
   p = add(doc, doc%root(), NODE_PARAGRAPH)
   n = add(doc, p, NODE_TEXT, 'a')
   n = add(doc, p, NODE_EMPH)
   n = add(doc, n, NODE_TEXT, 'b')
   s = add(doc, p, NODE_STRONG)
   s = add(doc, s, NODE_LINK)
   call doc%set_destination(s, '/u')
   n = add(doc, s, NODE_CODE, 'c')
   n = add(doc, p, NODE_SOFTBREAK)
   n = add(doc, p, NODE_LINEBREAK)
   n = add(doc, p, NODE_HTML_INLINE, '<i>')
   s = add(doc, p, NODE_IMAGE)
   call doc%set_destination(s, '/i')
   n = add(doc, s, NODE_TEXT, 'd')
   n = add(doc, p, KIND_MATH, 'x^2')
   endsubroutine build_inlines

   function add(doc, parent, kind, literal) result(n)
   !< Append a new node of a kind to a parent, with a literal if given.
   type(markdown_document), intent(inout)        :: doc     !< Document.
   integer(I4P),            intent(in)           :: parent  !< Parent.
   integer(I4P),            intent(in)           :: kind    !< Kind.
   character(*),            intent(in), optional :: literal !< Literal.
   integer(I4P)                                  :: n       !< New node.

   n = doc%new_node(kind)
   call doc%append_child(parent, n)
   if (present(literal)) call doc%set_literal(n, literal)
   endfunction add

   function events(doc, options, top) result(trace)
   !< The events of a walk, one word each: `+name` enter block, `-name` leave block, `<name` enter span, `>name` leave
   !< span, `=name` text.
   type(markdown_document), intent(in)           :: doc     !< Document.
   type(markdown_options),  intent(in)           :: options !< Options.
   integer(I4P),            intent(in), optional :: top     !< Subtree.
   character(:), allocatable                     :: trace   !< Events.
   character(5), parameter                       :: MARK = '+-<>=' !< Mark of each event.
   integer(I4P)                                  :: node    !< Current node.
   integer(I4P)                                  :: event   !< Current event.

   trace = ''
   node = 0_I4P
   event = EVENT_NONE
   do
      call options%walk(doc, node, event, top=top)
      if (node == 0_I4P) exit
      if (len(trace) > 0) trace = trace//' '
      trace = trace//MARK(event:event)//doc%kind_name(doc%node_kind(node))
   enddo
   endfunction events

   subroutine test_subtree_and_offsets()
   !< A walk limited to a subtree; the byte span returned with each event.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: options !< Options.
   integer(I4P)              :: list    !< The list.
   integer(I4P)              :: node    !< Current node.
   integer(I4P)              :: event   !< Current event.
   integer(I4P)              :: first   !< First byte.
   integer(I4P)              :: last    !< Last byte.
   logical                   :: spans   !< Every span right.

   call doc%load('# T'//LF//LF//'- a'//LF//'- b'//LF//LF//'end'//LF)
   call options%parse(doc)
   list = doc%next_sibling(doc%first_child(doc%root()))
   call check(events(doc, options, top=list) == '+list +item +paragraph =text -paragraph -item +item +paragraph '// &
              '=text -paragraph -item -list', 'subtree: the list only "'//events(doc, options, top=list)//'"')
   spans = .true.
   node = 0_I4P
   event = EVENT_NONE
   do
      call options%walk(doc, node, event, first=first, last=last)
      if (node == 0_I4P) exit
      spans = spans .and. first == doc%span_first(node) .and. last == doc%span_last(node)
   enddo
   call check(spans .and. first == 0_I4P .and. last == 0_I4P, 'offsets: each event with its node''s span, 0 at the end')
   node = 0_I4P
   call options%walk(doc, node, event, top=list, first=first, last=last)
   call check(doc%source_slice(first, last) == '- a'//LF//'- b', 'offsets: the list "'//doc%source_slice(first, last)//'"')
   endsubroutine test_subtree_and_offsets

   subroutine test_misuse()
   !< Misuses and a contract violation end the walk with a status; an empty document has no event.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_document)   :: empty   !< Uninitialised document.
   type(markdown_options)    :: options !< Options.
   type(markdown_options)    :: bad     !< Options with a math extension breaking the contract.
   type(math_extension)      :: math    !< Extension.
   integer(I4P)              :: node    !< Current node.
   integer(I4P)              :: event   !< Current event.
   integer(I4P)              :: stat    !< Status.
   character(:), allocatable :: errmsg  !< Message.

   node = 0_I4P
   call options%walk(empty, node, event, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_OK .and. node == 0_I4P .and. event == EVENT_NONE, 'misuse: an empty document, no event')
   call doc%load('text'//LF)
   call options%parse(doc)
   node = 99_I4P
   event = EVENT_ENTER_BLOCK
   call options%walk(doc, node, event, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT .and. node == 0_I4P .and. event == EVENT_NONE, &
              'misuse: a current node that is not one "'//errmsg//'"')
   node = doc%root()
   event = 42_I4P
   call options%walk(doc, node, event, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_ARGUMENT .and. node == 0_I4P, 'misuse: an event that is not one "'//errmsg//'"')
   node = 0_I4P
   call options%walk(doc, node, event, top=7_I4P, stat=stat, errmsg=errmsg)
   call check(stat == BARUCH_ERR_NODE .and. node == 0_I4P, 'misuse: top not a node "'//errmsg//'"')
   math%mode = 1_I4P
   call bad%register(math)
   call build_inlines(doc, bad)
   node = 0_I4P
   do
      call bad%walk(doc, node, event, stat=stat, errmsg=errmsg)
      if (node == 0_I4P) exit
   enddo
   call check(stat == BARUCH_ERR_CONTRACT .and. index(errmsg, 'extension "math"') > 0, &
              'contract: an answer that is not an event "'//errmsg//'"')
   endsubroutine test_misuse

   subroutine test_deep()
   !< 10^5 nested block quotes, then 10^5 nested lists: walked whole, every node once, balanced, with no recursion.
   type(markdown_document)   :: doc     !< Document.
   type(markdown_options)    :: options !< Options.
   character(:), allocatable :: source  !< Markdown.
   integer(I4P)              :: counts(4) !< Counts of the walk.
   integer                   :: depth   !< Nesting.
   logical                   :: ok      !< The walk passed.

   ! a variable, not a named constant: nvfortran 26.9 fails (internal error) on a repeat() of a long constant length
   ! folded in a contained procedure (reproducer notes/nvfortran-ilm-long-constant-string.f90)
   depth = 100000
   source = repeat('>', depth)//' deep'//LF
   call doc%load(source)
   call options%parse(doc)
   call walk_and_count(doc, options, counts, ok, 'deep quotes')
   print '(A,I0,A,I0,A)', 'deep walk: ', depth, ' nested block quotes, ', counts(2), ' block pairs'
   call check(ok .and. counts(2) == depth + 2 .and. counts(4) == 1, 'deep: quotes, root and paragraph, one text')
   source = repeat('- ', depth)//'x'//LF
   call doc%load(source)
   call options%parse(doc)
   call walk_and_count(doc, options, counts, ok, 'deep lists')
   print '(A,I0,A,I0,A)', 'deep walk: ', depth, ' nested lists, ', counts(2), ' block pairs'
   call check(ok .and. counts(2) == 2 * depth + 2, 'deep: lists and items, root and paragraph')
   endsubroutine test_deep

   pure function is_inline_leaf(kind) result(is)
   !< True for the inline kinds the walk reports as texts (the math extension's too).
   integer(I4P), intent(in) :: kind !< Kind.
   logical                  :: is   !< Result.

   select case(kind)
   case(NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE, KIND_MATH)
      is = .true.
   case default
      is = .false.
   endselect
   endfunction is_inline_leaf

   function index_lines(path) result(lines)
   !< The number of lines of a text file (0 if it cannot be read).
   character(*), intent(in) :: path  !< File.
   integer                  :: lines !< Lines.
   character(1024)          :: record !< A line.
   integer                  :: u     !< Unit.
   integer                  :: ios   !< I/O status.

   lines = 0
   open(newunit=u, file=path, action='read', status='old', iostat=ios)
   if (ios /= 0) return
   do
      read(u, '(A)', iostat=ios) record
      if (ios /= 0) exit
      lines = lines + 1
   enddo
   close(u)
   endfunction index_lines

   pure function str(i) result(s)
   !< An integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction str

   subroutine check(condition, label)
   !< Count a failed check and print it.
   logical,      intent(in) :: condition !< What must hold.
   character(*), intent(in) :: label     !< What is checked.

   if (.not. condition) then
      failures = failures + 1
      print '(A)', 'FAIL '//label
   endif
   endsubroutine check
endprogram baruch_test_walk
