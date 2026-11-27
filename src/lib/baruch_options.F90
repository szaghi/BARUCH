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
!< The encoding, the line ending and the canonical output of the writer join the options in later steps.
use penf,             only : I4P
use baruch_document,  only : markdown_document
use baruch_extension, only : markdown_extension, extension_slot
use baruch_blocks,    only : parse_blocks
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

   pure function istr(i) result(s)
   !< Return an integer as text.
   integer(I4P), intent(in)  :: i   !< Integer.
   character(:), allocatable :: s   !< Text.
   character(24)             :: buf !< Buffer.

   write(buf, '(I0)') i
   s = trim(buf)
   endfunction istr
endmodule baruch_options
