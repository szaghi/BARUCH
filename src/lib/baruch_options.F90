!< BARUCH, the options: what a parse does, the extensions registered and enabled first of all.
module baruch_options
!< BARUCH, the options: what a parse does, the extensions registered and enabled first of all.
!<
!< `markdown_options` holds the extensions a client registers (a copy of each, polymorphic) and which of them are
!< enabled, by name; a parse uses the enabled ones, in the order they were registered. The registry lives in the
!< options object the caller owns and passes: BARUCH keeps no module state, so different threads can parse with
!< different options, or with one options object (the hooks receive the extensions as `intent(in)`).
!<
!< `parse` reads the document's source (loaded by `markdown_document%load` or `load_file`) into its tree: the tree is
!< reset to its root, the enabled extensions register their kinds in the document, the block parser runs
!< (`baruch_blocks`: the CommonMark block structure, the extensions' block hooks tried before the core's starts).
!<
!< The options also read a parsed tree back, because what an extension's node looks like from outside is its
!< extension's to say: `render_html` (`baruch_html`: the HTML, a fragment or a standalone page, into a `character`, a
!< StringiFor `string` or an open unit; `render_html_file` into a file) asks the enabled extensions' `write_html` hooks,
!< and `walk` (`baruch_walk`: one event per call, the caller holding the state) their `walk_event` hooks. The line
!< ending and the canonical output of the writer join the options in later steps.
use penf,             only : I4P
use stringifor,       only : string
use baruch_document,  only : markdown_document
use baruch_extension, only : markdown_extension, extension_slot
use baruch_blocks,    only : parse_blocks
use baruch_html,      only : render_html_document
use baruch_walk,      only : walk_step
use baruch_status
implicit none
private
public :: markdown_options

integer(I4P), parameter :: SLOTS_DEFAULT = 4_I4P !< Extension slots allocated by the first registration.

type :: markdown_options
   !< What a parse does: the extensions registered and enabled.
   private
   type(extension_slot), allocatable :: slots_(:)         !< Registered extensions.
   integer(I4P)                      :: slots_used = 0_I4P !< Slots in use.
   contains
      procedure, pass(self) :: register           !< Register an extension (a copy), enabled unless asked otherwise.
      procedure, pass(self) :: enable             !< Enable a registered extension by name.
      procedure, pass(self) :: disable            !< Disable a registered extension by name.
      procedure, pass(self) :: is_registered      !< Return true if an extension of a name is registered.
      procedure, pass(self) :: is_enabled         !< Return true if an extension of a name is registered and enabled.
      procedure, pass(self) :: extensions_number  !< Return the number of registered extensions.
      procedure, pass(self) :: extension_name     !< Return the name of the i-th registered extension.
      procedure, pass(self) :: register_kinds     !< Register the kinds of the enabled extensions in a document.
      procedure, pass(self) :: parse              !< Parse a document's source into its tree.
      generic               :: render_html => render_html_character, render_html_string, render_html_unit !< As HTML.
      procedure, pass(self) :: render_html_file   !< Render a document as HTML into a file.
      procedure, pass(self) :: walk               !< Advance a walk of a document's tree by one event.
      procedure, pass(self), private :: render_html_character !< Render as HTML into a `character`.
      procedure, pass(self), private :: render_html_string    !< Render as HTML into a `string`.
      procedure, pass(self), private :: render_html_unit      !< Render as HTML into an open unit.
      procedure, pass(self), private :: find      !< Return the slot of an extension by name, 0 if none.
      procedure, pass(self), private :: set_state !< Enable or disable an extension by name.
endtype markdown_options

contains
   subroutine register(self, extension, enabled, stat, errmsg)
   !< Register an extension: a copy of it joins the options, after the ones already registered (a parse offers them
   !< the positions in this order), enabled unless `enabled = .false.`. Its name must be non-empty and not taken.
   class(markdown_options),   intent(inout)         :: self      !< Options.
   class(markdown_extension), intent(in)            :: extension !< Extension.
   logical,                   intent(in),  optional :: enabled   !< Enable it (default true).
   integer(I4P),              intent(out), optional :: stat      !< Status.
   character(:), allocatable, intent(out), optional :: errmsg    !< Message.
   type(extension_slot),      allocatable           :: grown(:)  !< Grown slots.
   character(:),              allocatable           :: name      !< Its name.
   integer(I4P)                                     :: s         !< Counter.

   call status_ok(stat, errmsg)
   name = extension%name()
   if (len(name) == 0) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_options%register: an extension needs a name', stat, errmsg)
      return
   endif
   if (self%find(name) > 0_I4P) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_options%register: an extension named "'//name// &
                        '" is registered already', stat, errmsg)
      return
   endif
   if (.not. allocated(self%slots_)) then
      allocate(self%slots_(SLOTS_DEFAULT))
   elseif (self%slots_used == size(self%slots_, kind=I4P)) then
      allocate(grown(2_I4P * self%slots_used))
      do s=1, self%slots_used
         call move_alloc(self%slots_(s)%ext, grown(s)%ext)
         grown(s)%enabled = self%slots_(s)%enabled
      enddo
      call move_alloc(grown, self%slots_)
   endif
   self%slots_used = self%slots_used + 1_I4P
   allocate(self%slots_(self%slots_used)%ext, source=extension)
   self%slots_(self%slots_used)%enabled = .true.
   if (present(enabled)) self%slots_(self%slots_used)%enabled = enabled
   endsubroutine register

   subroutine enable(self, name, stat, errmsg)
   !< Enable a registered extension by name; an unknown name gives `BARUCH_ERR_ARGUMENT`.
   class(markdown_options),   intent(inout)         :: self   !< Options.
   character(*),              intent(in)            :: name   !< Name.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   call self%set_state(name, .true., 'enable', stat, errmsg)
   endsubroutine enable

   subroutine disable(self, name, stat, errmsg)
   !< Disable a registered extension by name (it stays registered); an unknown name gives `BARUCH_ERR_ARGUMENT`.
   class(markdown_options),   intent(inout)         :: self   !< Options.
   character(*),              intent(in)            :: name   !< Name.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   call self%set_state(name, .false., 'disable', stat, errmsg)
   endsubroutine disable

   function is_registered(self, name) result(is)
   !< Return true if an extension of a name is registered.
   class(markdown_options), intent(in) :: self !< Options.
   character(*),            intent(in) :: name !< Name.
   logical                             :: is   !< Result.

   is = self%find(name) > 0_I4P
   endfunction is_registered

   function is_enabled(self, name) result(is)
   !< Return true if an extension of a name is registered and enabled.
   class(markdown_options), intent(in) :: self !< Options.
   character(*),            intent(in) :: name !< Name.
   logical                             :: is   !< Result.
   integer(I4P)                        :: s    !< Slot.

   is = .false.
   s = self%find(name)
   if (s > 0_I4P) is = self%slots_(s)%enabled
   endfunction is_enabled

   pure function extensions_number(self) result(number)
   !< Return the number of registered extensions, enabled or not.
   class(markdown_options), intent(in) :: self   !< Options.
   integer(I4P)                        :: number !< Registered extensions.

   number = self%slots_used
   endfunction extensions_number

   function extension_name(self, i, stat, errmsg) result(name)
   !< Return the name of the i-th registered extension (in registration order); empty, with `BARUCH_ERR_ARGUMENT`,
   !< for no such extension.
   class(markdown_options),   intent(in)            :: self   !< Options.
   integer(I4P),              intent(in)            :: i      !< Index, 1 to `extensions_number()`.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   character(:), allocatable                        :: name   !< Name.

   name = ''
   if (i < 1_I4P .or. i > self%slots_used) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_options%extension_name: no extension '//istr(i)// &
                        ' (1 to '//istr(self%slots_used)//')', stat, errmsg)
      return
   endif
   call status_ok(stat, errmsg)
   name = self%slots_(i)%ext%name()
   endfunction extension_name

   subroutine register_kinds(self, doc, stat, errmsg)
   !< Register the kinds of the enabled extensions in a document (each extension's `register_kinds` hook), in
   !< registration order. `parse` does it; a client building a document by hand with extension nodes calls it first.
   !< A clash (two extensions with one kind number or name) gives the document's status, the extension named.
   class(markdown_options),   intent(in)            :: self   !< Options.
   type(markdown_document),   intent(inout)         :: doc    !< Document.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: s      !< Counter.
   integer(I4P)                                     :: ierr   !< Status of a hook.
   character(:), allocatable                        :: msg    !< Message of a hook.

   call status_ok(stat, errmsg)
   do s=1, self%slots_used
      if (.not. self%slots_(s)%enabled) cycle
      ierr = BARUCH_OK
      msg = ''
      call self%slots_(s)%ext%register_kinds(doc, ierr, msg)
      if (ierr /= BARUCH_OK) then
         call status_error(ierr, 'markdown_options%register_kinds: extension "'//self%slots_(s)%ext%name()//'": '// &
                           msg, stat, errmsg)
         return
      endif
   enddo
   endsubroutine register_kinds

   subroutine parse(self, doc, stat, errmsg)
   !< Parse a document's source into its tree, with the enabled extensions: the tree is reset to its root (the source
   !< and its lines kept), the extensions' kinds registered, the blocks parsed. Parsing never fails on the text: a
   !< nonzero `stat` comes from an extension (a hook failing, or breaking the hook contract: `BARUCH_ERR_CONTRACT`)
   !< or from memory, and the tree then holds what was parsed before it.
   class(markdown_options),   intent(in)            :: self   !< Options.
   type(markdown_document),   intent(inout)         :: doc    !< Document, its source loaded.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.
   integer(I4P)                                     :: ierr   !< Status.
   character(:), allocatable                        :: msg    !< Message.
   type(extension_slot)                             :: empty(0) !< No extension.

   call doc%initialize(stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call self%register_kinds(doc, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) then
      if (self%slots_used > 0_I4P) then
         call parse_blocks(doc, self%slots_(1:self%slots_used), stat=ierr, errmsg=msg)
      else
         call parse_blocks(doc, empty, stat=ierr, errmsg=msg)
      endif
   endif
   if (ierr /= BARUCH_OK) then
      call status_error(ierr, msg, stat, errmsg)
   else
      call status_ok(stat, errmsg)
   endif
   endsubroutine parse

   subroutine render_html_character(self, doc, html, standalone, title, stat, errmsg)
   !< Render a document as HTML, with the enabled extensions: an HTML fragment (default) or, with `standalone =
   !< .true.`, a whole HTML5 document titled `title` (default: the first heading's text). A nonzero `stat` comes from an
   !< extension (a `write_html` hook failing, a `walk_event` answer breaking the contract); `html` then holds what was
   !< rendered before it.
   class(markdown_options),   intent(in)            :: self       !< Options.
   type(markdown_document),   intent(in)            :: doc        !< Document.
   character(:), allocatable, intent(out)           :: html       !< HTML.
   logical,                   intent(in),  optional :: standalone !< A whole HTML document (default: a fragment).
   character(*),              intent(in),  optional :: title      !< Title of a standalone document.
   integer(I4P),              intent(out), optional :: stat       !< Status.
   character(:), allocatable, intent(out), optional :: errmsg     !< Message.
   integer(I4P)                                     :: ierr       !< Status.
   character(:), allocatable                        :: msg        !< Message.
   type(extension_slot)                             :: empty(0)   !< No extension.
   logical                                          :: alone      !< Standalone.
   character(:), allocatable                        :: heading    !< Title.

   alone = .false.
   if (present(standalone)) alone = standalone
   heading = ''
   if (present(title)) heading = title
   msg = ''
   if (self%slots_used > 0_I4P) then
      call render_html_document(doc, self%slots_(1:self%slots_used), html, alone, heading, ierr, msg)
   else
      call render_html_document(doc, empty, html, alone, heading, ierr, msg)
   endif
   if (ierr /= BARUCH_OK) then
      call status_error(ierr, msg, stat, errmsg)
   else
      call status_ok(stat, errmsg)
   endif
   endsubroutine render_html_character

   subroutine render_html_string(self, doc, html, standalone, title, stat, errmsg)
   !< Render a document as HTML into a StringiFor `string`; as the `character` form.
   class(markdown_options),   intent(in)            :: self       !< Options.
   type(markdown_document),   intent(in)            :: doc        !< Document.
   type(string),              intent(out)           :: html       !< HTML.
   logical,                   intent(in),  optional :: standalone !< A whole HTML document (default: a fragment).
   character(*),              intent(in),  optional :: title      !< Title of a standalone document.
   integer(I4P),              intent(out), optional :: stat       !< Status.
   character(:), allocatable, intent(out), optional :: errmsg     !< Message.
   character(:), allocatable                        :: text       !< HTML.

   call self%render_html_character(doc, text, standalone=standalone, title=title, stat=stat, errmsg=errmsg)
   html = text
   endsubroutine render_html_string

   subroutine render_html_unit(self, doc, unit, standalone, title, stat, errmsg)
   !< Render a document as HTML into a unit the caller opened for writing: a formatted unit gets one record per line
   !< (the last one left open when the HTML does not end with a line feed), an unformatted stream unit the bytes. A
   !< unit not open, open for reading only, or unformatted but not stream gives `BARUCH_ERR_IO`, a failed write too.
   class(markdown_options),   intent(in)            :: self       !< Options.
   type(markdown_document),   intent(in)            :: doc        !< Document.
   integer,                   intent(in)            :: unit       !< Unit, open for writing.
   logical,                   intent(in),  optional :: standalone !< A whole HTML document (default: a fragment).
   character(*),              intent(in),  optional :: title      !< Title of a standalone document.
   integer(I4P),              intent(out), optional :: stat       !< Status.
   character(:), allocatable, intent(out), optional :: errmsg     !< Message.
   character(:), allocatable                        :: html       !< HTML.
   integer(I4P)                                     :: ierr       !< Status.
   character(:), allocatable                        :: msg        !< Message.

   call self%render_html_character(doc, html, standalone=standalone, title=title, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) call write_unit(unit, html, 'markdown_options%render_html', ierr, msg)
   if (ierr /= BARUCH_OK) then
      call status_error(ierr, msg, stat, errmsg)
   else
      call status_ok(stat, errmsg)
   endif
   endsubroutine render_html_unit

   subroutine render_html_file(self, doc, path, standalone, title, stat, errmsg)
   !< Render a document as HTML into a file, created or replaced, byte for byte (stream access). A file that cannot be
   !< opened or written gives `BARUCH_ERR_IO` with the processor's message.
   class(markdown_options),   intent(in)            :: self       !< Options.
   type(markdown_document),   intent(in)            :: doc        !< Document.
   character(*),              intent(in)            :: path       !< File.
   logical,                   intent(in),  optional :: standalone !< A whole HTML document (default: a fragment).
   character(*),              intent(in),  optional :: title      !< Title of a standalone document.
   integer(I4P),              intent(out), optional :: stat       !< Status.
   character(:), allocatable, intent(out), optional :: errmsg     !< Message.
   character(:), allocatable                        :: html       !< HTML.
   integer(I4P)                                     :: ierr       !< Status.
   character(:), allocatable                        :: msg        !< Message.
   character(512)                                   :: iomsg      !< I/O message.
   integer                                          :: u          !< Unit.
   integer                                          :: ios        !< I/O status.

   call self%render_html_character(doc, html, standalone=standalone, title=title, stat=ierr, errmsg=msg)
   if (ierr == BARUCH_OK) then
      iomsg = ''
      open(newunit=u, file=path, access='stream', form='unformatted', action='write', status='replace', iostat=ios, &
           iomsg=iomsg)
      if (ios == 0) then
         write(u, iostat=ios, iomsg=iomsg) html
         close(u)
      endif
      if (ios /= 0) then
         ierr = BARUCH_ERR_IO
         msg = 'markdown_options%render_html_file: cannot write "'//path//'": '//trim(iomsg)
      endif
   endif
   if (ierr /= BARUCH_OK) then
      call status_error(ierr, msg, stat, errmsg)
   else
      call status_ok(stat, errmsg)
   endif
   endsubroutine render_html_file

   subroutine walk(self, doc, node, event, top, first, last, stat, errmsg)
   !< Advance a walk of a document's tree by one event. The caller holds the walk's state, `node` and `event`: it
   !< starts with `node = 0`, and each call replaces them with the next event (`EVENT_ENTER_BLOCK`, `EVENT_LEAVE_BLOCK`,
   !< `EVENT_ENTER_SPAN`, `EVENT_LEAVE_SPAN`, `EVENT_TEXT`) and its node, until `node = 0` (and `event = EVENT_NONE`):
   !< the walk is over. `top` walks the subtree of a node only (default: the root); `first` and `last` return the
   !< node's byte span in the source. An extension node's events are what its extension's `walk_event` hook answers.
   !< A misuse (a current node or event that is not one, `top` not a live node: `BARUCH_ERR_ARGUMENT`,
   !< `BARUCH_ERR_NODE`) or a hook answer that is not an event of the right direction (`BARUCH_ERR_CONTRACT`) ends the
   !< walk with `node = 0`.
   class(markdown_options),   intent(in)            :: self     !< Options.
   type(markdown_document),   intent(in)            :: doc      !< Document.
   integer(I4P),              intent(inout)         :: node     !< Current node (0: start), then the next one.
   integer(I4P),              intent(inout)         :: event    !< Current event, then the next one.
   integer(I4P),              intent(in),  optional :: top      !< Root of the walked subtree (default: the root).
   integer(I4P),              intent(out), optional :: first    !< First byte of the node's span (0 when done).
   integer(I4P),              intent(out), optional :: last     !< Last byte of the node's span (0 when done).
   integer(I4P),              intent(out), optional :: stat     !< Status.
   character(:), allocatable, intent(out), optional :: errmsg   !< Message.
   integer(I4P)                                     :: ierr     !< Status.
   character(:), allocatable                        :: msg      !< Message.
   type(extension_slot)                             :: empty(0) !< No extension.
   integer(I4P)                                     :: t        !< Top.

   t = 1_I4P
   if (present(top)) t = top
   msg = ''
   if (self%slots_used > 0_I4P) then
      call walk_step(doc, self%slots_(1:self%slots_used), t, node, event, ierr, msg)
   else
      call walk_step(doc, empty, t, node, event, ierr, msg)
   endif
   if (present(first)) first = 0_I4P
   if (present(last)) last = 0_I4P
   if (node /= 0_I4P) then
      if (present(first)) first = doc%span_first(node)
      if (present(last)) last = doc%span_last(node)
   endif
   if (ierr /= BARUCH_OK) then
      call status_error(ierr, msg, stat, errmsg)
   else
      call status_ok(stat, errmsg)
   endif
   endsubroutine walk

   ! private
   function find(self, name) result(s)
   !< Return the slot of an extension by name, 0 if none.
   class(markdown_options), intent(in) :: self !< Options.
   character(*),            intent(in) :: name !< Name.
   integer(I4P)                        :: s    !< Slot.

   do s=1, self%slots_used
      if (self%slots_(s)%ext%name() == name .and. len(self%slots_(s)%ext%name()) == len(name)) return
   enddo
   s = 0_I4P
   endfunction find

   subroutine set_state(self, name, enabled, proc, stat, errmsg)
   !< Enable or disable an extension by name.
   class(markdown_options),   intent(inout)         :: self    !< Options.
   character(*),              intent(in)            :: name    !< Name.
   logical,                   intent(in)            :: enabled !< New state.
   character(*),              intent(in)            :: proc    !< Name of the calling procedure.
   integer(I4P),              intent(out), optional :: stat    !< Status.
   character(:), allocatable, intent(out), optional :: errmsg  !< Message.
   integer(I4P)                                     :: s       !< Slot.

   s = self%find(name)
   if (s == 0_I4P) then
      call status_error(BARUCH_ERR_ARGUMENT, 'markdown_options%'//proc//': no extension named "'//name// &
                        '" is registered', stat, errmsg)
      return
   endif
   self%slots_(s)%enabled = enabled
   call status_ok(stat, errmsg)
   endsubroutine set_state

   subroutine write_unit(unit, text, proc, ierr, msg)
   !< Write a text to a unit open for writing: line by line to a formatted unit, the bytes to an unformatted stream.
   integer,                   intent(in)    :: unit  !< Unit.
   character(*),              intent(in)    :: text  !< Text.
   character(*),              intent(in)    :: proc  !< Name of the calling procedure.
   integer(I4P),              intent(out)   :: ierr  !< Status.
   character(:), allocatable, intent(inout) :: msg   !< Message.
   character(512)                           :: iomsg !< I/O message.
   character(16)                            :: form  !< Its form.
   character(16)                            :: acc   !< Its access.
   character(16)                            :: act   !< Its action.
   logical                                  :: open_ !< It is open.
   integer                                  :: ios   !< I/O status.
   integer                                  :: i, j  !< Bounds of a line.

   ierr = BARUCH_OK
   iomsg = ''
   inquire(unit=unit, opened=open_, form=form, access=acc, action=act, iostat=ios, iomsg=iomsg)
   if (ios /= 0) then
      call fail('cannot inquire unit '//istr(int(unit, I4P))//': '//trim(iomsg))
      return
   endif
   if (.not. open_) then
      call fail('unit '//istr(int(unit, I4P))//' is not open')
      return
   endif
   if (trim(act) == 'READ') then
      call fail('unit '//istr(int(unit, I4P))//' is open for reading only')
      return
   endif
   if (trim(form) == 'UNFORMATTED') then
      if (trim(acc) /= 'STREAM') then
         call fail('unit '//istr(int(unit, I4P))//' is unformatted but not stream: it cannot take text')
         return
      endif
      write(unit, iostat=ios, iomsg=iomsg) text
   else
      i = 1
      ios = 0
      do while (i <= len(text) .and. ios == 0)
         j = index(text(i:), achar(10))
         if (j > 0) then
            write(unit, '(A)', iostat=ios, iomsg=iomsg) text(i:i+j-2)
            i = i + j
         else
            write(unit, '(A)', advance='no', iostat=ios, iomsg=iomsg) text(i:)
            i = len(text) + 1
         endif
      enddo
   endif
   if (ios /= 0) call fail('cannot write to unit '//istr(int(unit, I4P))//': '//trim(iomsg))

   contains
      subroutine fail(what)
      !< Set the status to an I/O failure.
      character(*), intent(in) :: what !< What failed.

      ierr = BARUCH_ERR_IO
      msg = proc//': '//what
      endsubroutine fail
   endsubroutine write_unit

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr
endmodule baruch_options
