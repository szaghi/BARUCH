!< BARUCH, the walk: a document's tree as a sequence of events (enter and leave a block or a span, text), iteratively.
module baruch_walk
!< BARUCH, the walk: a document's tree as a sequence of events (enter and leave a block or a span, text), iteratively.
!<
!< The reader interface of settled design 5: the events of md4c's callbacks (`enter_block`, `leave_block`,
!< `enter_span`, `leave_span`, `text`), each with its node, whose kind and byte span the document gives. The walk is an
!< **iterator**, not a callback: the caller holds the state, two integers (the current node and the current event), and
!< each call of `walk_step` advances them to the next event. No procedure argument, no recursion, no state in the
!< library: a walk is reentrant, any number can run at once over one document, and a loop over the events is a plain
!< `do` with a `select case`, which is what Fortran reads best.
!<
!< The events of a node depend on its **class**:
!<
!< - a **block** (every core block kind; an extension kind registered as a block): `EVENT_ENTER_BLOCK`, its children,
!<   `EVENT_LEAVE_BLOCK`;
!< - a **span** (emphasis, strong emphasis, link, image; an extension kind registered as inline): `EVENT_ENTER_SPAN`,
!<   its children, `EVENT_LEAVE_SPAN`;
!< - a **text** (text, soft and hard line breaks, code span, inline HTML: the inline leaves, whose content is their
!<   literal): one `EVENT_TEXT`; its children, if any, are not visited.
!<
!< An extension changes the class of its own kinds (kind numbers from `NODE_EXTENSION_FIRST`) through its `walk_event`
!< hook: asked with `entering = .true.` it answers `EVENT_ENTER_BLOCK`, `EVENT_ENTER_SPAN` or `EVENT_TEXT`, asked with
!< `entering = .false.` (leaving a node it entered) the matching `EVENT_LEAVE_*`. The enabled extensions are asked in
!< registration order, the first that handles the node wins; any other answer is a contract violation
!< (`BARUCH_ERR_CONTRACT`). Core kinds are never offered to the hook.
!<
!< The walk follows the tree's links (parent, first child, next sibling): its cost is one event per block and span
!< boundary and one per text, whatever the depth. Editing the tree while walking it is the caller's risk: the walk reads
!< the links of the current node at each call (a current node unlinked ends the walk with `BARUCH_ERR_ARGUMENT`).
!< `markdown_options%walk` is the public face of `walk_step`.
use penf,               only : I4P
use baruch_document,    only : markdown_document
use baruch_extension,   only : extension_slot
use baruch_kinds_nodes, only : NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE, NODE_EMPH, &
                               NODE_STRONG, NODE_LINK, NODE_IMAGE, NODE_EXTENSION_FIRST, is_core_block_kind
use baruch_status
implicit none
private
public :: walk_step
public :: EVENT_NONE, EVENT_ENTER_BLOCK, EVENT_LEAVE_BLOCK, EVENT_ENTER_SPAN, EVENT_LEAVE_SPAN, EVENT_TEXT

integer(I4P), parameter :: EVENT_NONE        = 0_I4P !< No event: before the first call, after the last.
integer(I4P), parameter :: EVENT_ENTER_BLOCK = 1_I4P !< Entering a block: its children follow.
integer(I4P), parameter :: EVENT_LEAVE_BLOCK = 2_I4P !< Leaving a block: its children are done.
integer(I4P), parameter :: EVENT_ENTER_SPAN  = 3_I4P !< Entering a span (inline container): its children follow.
integer(I4P), parameter :: EVENT_LEAVE_SPAN  = 4_I4P !< Leaving a span.
integer(I4P), parameter :: EVENT_TEXT        = 5_I4P !< A text (inline leaf): one event, no leave.

contains
   subroutine walk_step(doc, slots, top, node, event, ierr, msg)
   !< Advance a walk of the subtree of `top` by one event: `node` and `event` hold the current event on entry (`node =
   !< 0` to start) and the next one on exit; `node = 0` and `event = EVENT_NONE` once the subtree is done. `ierr` (0 on
   !< success) and `msg` report a misuse (a current node that is not live, an unknown event, `top` not a live node) or
   !< an extension breaking the `walk_event` contract; the walk then ends (`node = 0`).
   type(markdown_document),   intent(in)    :: doc      !< Document.
   type(extension_slot),      intent(in)    :: slots(:) !< Extensions of the options.
   integer(I4P),              intent(in)    :: top      !< Root of the walked subtree.
   integer(I4P),              intent(inout) :: node     !< Current node (0: start), then the next one.
   integer(I4P),              intent(inout) :: event    !< Current event, then the next one.
   integer(I4P),              intent(out)   :: ierr     !< Status.
   character(:), allocatable, intent(inout) :: msg      !< Message, set with a nonzero `ierr`.
   integer(I4P)                             :: next     !< Next node.

   ierr = BARUCH_OK
   if (node == 0_I4P) then
      if (doc%root() == 0_I4P .and. top <= 1_I4P .and. top >= 0_I4P) then ! an uninitialised document: no event
         event = EVENT_NONE
         return
      endif
      if (.not. doc%is_node(top)) then
         call stop_walk(BARUCH_ERR_NODE, 'markdown_options%walk: top '//istr(top)//' is not a live node')
         return
      endif
      call go_to(top, .true.)
      return
   endif
   if (.not. doc%is_node(node)) then
      call stop_walk(BARUCH_ERR_ARGUMENT, 'markdown_options%walk: the current node '//istr(node)//' is not a live node')
      return
   endif
   select case(event)
   case(EVENT_ENTER_BLOCK, EVENT_ENTER_SPAN)
      next = doc%first_child(node)
      if (next /= 0_I4P) then
         call go_to(next, .true.)
      else
         event = event + 1_I4P ! the matching leave: ENTER_BLOCK + 1 = LEAVE_BLOCK, ENTER_SPAN + 1 = LEAVE_SPAN
      endif
   case(EVENT_LEAVE_BLOCK, EVENT_LEAVE_SPAN, EVENT_TEXT)
      if (node == top) then
         node = 0_I4P
         event = EVENT_NONE
         return
      endif
      next = doc%next_sibling(node)
      if (next /= 0_I4P) then
         call go_to(next, .true.)
         return
      endif
      next = doc%parent(node)
      if (next == 0_I4P) then ! `top` was not an ancestor: the walk ends at the root of the tree it was in
         node = 0_I4P
         event = EVENT_NONE
         return
      endif
      call go_to(next, .false.)
   case default
      call stop_walk(BARUCH_ERR_ARGUMENT, 'markdown_options%walk: the current event '//istr(event)//' is not an event')
   endselect

   contains
      subroutine go_to(n, entering)
      !< Make a node the current one, entering or leaving it: the event is its kind's class, or what its extension
      !< answers (checked).
      integer(I4P), value      :: n        !< Node.
      logical,      intent(in) :: entering !< Entering (else leaving).
      integer(I4P)             :: kind     !< Its kind.
      integer(I4P)             :: ev       !< Event.
      integer(I4P)             :: s        !< Counter.
      logical                  :: handled  !< An extension answered.

      kind = doc%node_kind(n)
      if (kind >= NODE_EXTENSION_FIRST) then
         do s=1, size(slots, kind=I4P)
            if (.not. slots(s)%enabled) cycle
            call slots(s)%ext%walk_event(doc, n, entering, ev, handled)
            if (.not. handled) cycle
            if ((entering .and. (ev == EVENT_ENTER_BLOCK .or. ev == EVENT_ENTER_SPAN .or. ev == EVENT_TEXT)) .or. &
                ((.not. entering) .and. (ev == EVENT_LEAVE_BLOCK .or. ev == EVENT_LEAVE_SPAN))) then
               node = n
               event = ev
            else
               call stop_walk(BARUCH_ERR_CONTRACT, 'markdown_options%walk: extension "'//slots(s)%ext%name()// &
                              '" answers event '//istr(ev)//' for node '//istr(n)//' (kind '//istr(kind)//') on '// &
                              trim(merge('entering', 'leaving ', entering)))
            endif
            return
         enddo
      endif
      ev = default_class(doc, kind)
      if (.not. entering) ev = ev + 1_I4P ! only blocks and spans are left: their leave is their enter + 1
      node = n
      event = ev
      endsubroutine go_to

      subroutine stop_walk(code, text)
      !< End the walk on an error.
      integer(I4P), intent(in) :: code !< Status.
      character(*), intent(in) :: text !< Message.

      ierr = code
      msg = text
      node = 0_I4P
      event = EVENT_NONE
      endsubroutine stop_walk
   endsubroutine walk_step

   ! private
   pure function default_class(doc, kind) result(ev)
   !< The entering event of a kind with no extension answering: block, span or text.
   type(markdown_document), intent(in) :: doc  !< Document.
   integer(I4P),            intent(in) :: kind !< Kind.
   integer(I4P)                        :: ev   !< `EVENT_ENTER_BLOCK`, `EVENT_ENTER_SPAN` or `EVENT_TEXT`.

   select case(kind)
   case(NODE_TEXT, NODE_SOFTBREAK, NODE_LINEBREAK, NODE_CODE, NODE_HTML_INLINE)
      ev = EVENT_TEXT
   case(NODE_EMPH, NODE_STRONG, NODE_LINK, NODE_IMAGE)
      ev = EVENT_ENTER_SPAN
   case default
      ev = EVENT_ENTER_BLOCK
      if (.not. is_core_block_kind(kind)) then
         if (doc%kind_is_inline(kind)) ev = EVENT_ENTER_SPAN
      endif
   endselect
   endfunction default_class

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr
endmodule baruch_walk
