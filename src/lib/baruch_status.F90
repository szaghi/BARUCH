!< BARUCH, the status policy: a code and a message, never a stop.
module baruch_status
!< BARUCH, the status policy: a code and a message, never a stop.
!<
!< A procedure that can fail has optional `stat` (`intent(out)`) and `errmsg` (`character(:), allocatable`,
!< `intent(out)`) arguments: `stat = BARUCH_OK` and an empty `errmsg` on success, one code per class of failure and a
!< sentence naming the procedure and the values involved otherwise. BARUCH never executes `stop` or `error stop`: a
!< caller that passes no `stat` gets the documented default result (a null node, an empty string, zero) and no
!< message. Parsing never fails (every byte sequence is a document): the codes are for API misuse, input and output,
!< and extensions breaking the hook contract.
use penf, only : I4P
implicit none
private
public :: status_ok, status_error
public :: BARUCH_OK
public :: BARUCH_ERR_ARGUMENT
public :: BARUCH_ERR_NODE
public :: BARUCH_ERR_KIND
public :: BARUCH_ERR_LINK
public :: BARUCH_ERR_ALLOCATION
public :: BARUCH_ERR_IO

integer(I4P), parameter :: BARUCH_OK             = 0_I4P !< Success.
integer(I4P), parameter :: BARUCH_ERR_ARGUMENT   = 1_I4P !< An argument is out of its range.
integer(I4P), parameter :: BARUCH_ERR_NODE       = 2_I4P !< A handle that is not a live node of the document.
integer(I4P), parameter :: BARUCH_ERR_KIND       = 3_I4P !< An accessor called on a node of the wrong kind.
integer(I4P), parameter :: BARUCH_ERR_LINK       = 4_I4P !< An edit that would break the tree (a cycle, the root moved).
integer(I4P), parameter :: BARUCH_ERR_ALLOCATION = 5_I4P !< Memory could not be allocated.
integer(I4P), parameter :: BARUCH_ERR_IO         = 6_I4P !< A file could not be opened, read or written.

contains
   pure subroutine status_ok(stat, errmsg)
   !< Set the status to success, when the caller asked for it.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (present(stat)) stat = BARUCH_OK
   if (present(errmsg)) errmsg = ''
   endsubroutine status_ok

   pure subroutine status_error(code, msg, stat, errmsg)
   !< Set the status to `code` and the message to `msg`, when the caller asked for them.
   integer(I4P),              intent(in)            :: code   !< Code.
   character(*),              intent(in)            :: msg    !< What failed, with its values.
   integer(I4P),              intent(out), optional :: stat   !< Status.
   character(:), allocatable, intent(out), optional :: errmsg !< Message.

   if (present(stat)) stat = code
   if (present(errmsg)) errmsg = msg
   endsubroutine status_error
endmodule baruch_status
