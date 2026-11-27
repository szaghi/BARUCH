!< BARUCH, the extension interface: an abstract type whose hooks the parser, the renderers and the writer call.
module baruch_extension
!< BARUCH, the extension interface: an abstract type whose hooks the parser, the renderers and the writer call.
!<
!< An extension is a type extending `markdown_extension`. Every facet of the syntax it adds is a hook, a type-bound
!< procedure with a "not mine" default, so an extension overrides only the hooks it needs:
!<
!< - kinds: `register_kinds` registers the extension's node kinds in the document before a parse;
!< - blocks: `block_start` (may open a block at the current position of a line, and sees the lines of the open
!<   paragraph), `block_continue` (does a line continue an open block of the extension), `block_close` (the block is
!<   finished, with its content lines);
!< - content rules: `block_content` (holds blocks, takes raw lines, or neither), `can_contain` (which children a
!<   container of the extension holds), `contains_inlines` (whether a block's content is inline-parsed);
!< - inlines: `inline_triggers` and `inline_start` (an inline construct opened by a byte), `delimiter_chars`,
!<   `delimiter_run` and `delimiter_match` (runs of a character taking part in the delimiter algorithm, as `~~` does);
!< - post-processing: `post_process`, once the whole document is parsed;
!< - output: `write_html`, `write_markdown` and `walk_event`.
!<
!< Two bindings have no default: `name` (the name the options enable and disable it by) and `write_markdown` (every
!< extension can be written back as Markdown: settled design 6). The hooks receive the extension as `intent(in)`: a
!< parse keeps its state in the document, never in the extension, so one options object can serve parses running at
!< the same time.
!<
!< The block hooks are called by the block parser (`baruch_blocks`), `write_html` by the HTML renderer (`baruch_html`)
!< and `walk_event` by the walk (`baruch_walk`); `contains_inlines` and the inline hooks by the inline parser
!< (`baruch_inlines`), `post_process` by `markdown_options%parse` after it; `write_markdown` by the writer (step 3.2).
!<
!< `extension_slot` (a polymorphic extension and its enabled flag) is how `markdown_options` holds the extensions; it is
!< not exported by `baruch`.
use penf,            only : I4P
use baruch_document, only : markdown_document
implicit none
private
public :: markdown_extension
public :: extension_slot
public :: BLOCK_UNMATCHED, BLOCK_MATCHED, BLOCK_FINISHED
public :: CONTENT_NONE, CONTENT_BLOCKS, CONTENT_LINES

! what block_continue says of a line
integer(I4P), parameter :: BLOCK_UNMATCHED = 0_I4P !< The line does not continue the block: it closes.
integer(I4P), parameter :: BLOCK_MATCHED   = 1_I4P !< The line continues the block.
integer(I4P), parameter :: BLOCK_FINISHED  = 2_I4P !< The line is the block's last (a closing fence): the block takes the
                                                   !< rest of it and closes.
! what a block holds (block_content)
integer(I4P), parameter :: CONTENT_NONE   = 0_I4P !< Nothing after its first line: a one-line leaf (a thematic break).
integer(I4P), parameter :: CONTENT_BLOCKS = 1_I4P !< Blocks: a container (a block quote); `can_contain` refines it.
integer(I4P), parameter :: CONTENT_LINES  = 2_I4P !< Lines: a leaf taking the rest of each line (a code block).

type, abstract :: markdown_extension
   !< The extension interface: every hook has a "not mine" default, but `name` and `write_markdown`.
   contains
      ! identity, kinds
      procedure(name_interface),           pass(self), deferred :: name             !< Return the extension's name.
      procedure,                           pass(self)           :: register_kinds   !< Register its kinds.
      ! blocks
      procedure,                           pass(self)           :: block_start      !< Open a block, or not mine.
      procedure,                           pass(self)           :: block_continue   !< Continue an open block.
      procedure,                           pass(self)           :: block_close      !< Finish a block.
      ! content rules
      procedure,                           pass(self)           :: block_content    !< What a block of a kind holds.
      procedure,                           pass(self)           :: can_contain      !< Can a container hold a child.
      procedure,                           pass(self)           :: contains_inlines !< Is a block's content inline.
      ! inlines
      procedure,                           pass(self)           :: inline_triggers  !< Characters opening inlines.
      procedure,                           pass(self)           :: inline_start     !< Parse an inline, or not mine.
      procedure,                           pass(self)           :: delimiter_chars  !< Its delimiter characters.
      procedure,                           pass(self)           :: delimiter_run    !< Can a run open, close.
      procedure,                           pass(self)           :: delimiter_match  !< Match two delimiter runs.
      ! post-processing
      procedure,                           pass(self)           :: post_process     !< Edit the parsed document.
      ! output
      procedure,                           pass(self)           :: write_html       !< A node as HTML, or not mine.
      procedure(write_markdown_interface), pass(self), deferred :: write_markdown   !< A node as Markdown.
      procedure,                           pass(self)           :: walk_event       !< A node's walk event.
endtype markdown_extension

type :: extension_slot
   !< An extension held by the options: the extension itself (a copy, polymorphic) and whether a parse uses it.
   class(markdown_extension), allocatable :: ext             !< Extension.
   logical                                :: enabled = .false. !< Used by a parse.
endtype extension_slot

abstract interface
   function name_interface(self) result(name)
   !< Return the extension's name: unique among the extensions of an options object, what `enable`/`disable` take.
   import :: markdown_extension
   class(markdown_extension), intent(in) :: self !< Extension.
   character(:), allocatable             :: name !< Name.
   endfunction name_interface

   subroutine write_markdown_interface(self, doc, node, entering, markdown, handled, stat, errmsg)
   !< Write a node as Markdown, on entering it and on leaving it (after its children). A node of another extension or
   !< of the core: `handled = .false.`. Mandatory: whatever an extension parses, it can write back.
   import :: markdown_extension, markdown_document, I4P
   class(markdown_extension), intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering the node (else leaving it).
   character(:), allocatable, intent(out)   :: markdown !< Markdown written for this event.
   logical,                   intent(out)   :: handled  !< True if the node is the extension's.
   integer(I4P),              intent(inout) :: stat     !< Status: left 0 (`BARUCH_OK`) unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg   !< Message, set with a nonzero `stat`.
   endsubroutine write_markdown_interface
endinterface

contains
   ! kinds
   subroutine register_kinds(self, doc, stat, errmsg)
   !< Register the extension's kinds in a document (`doc%register_kind`), called by the options before every parse.
   !< Default: no kinds.
   class(markdown_extension), intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status: left 0 unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg !< Message, set with a nonzero `stat`.

   ! not mine: the arguments are referenced only so that no compiler warns about them
   associate(unused => [doc%root(), stat, merge(1_I4P, 0_I4P, allocated(errmsg))])
   endassociate
   endsubroutine register_kinds

   ! blocks
   subroutine block_start(self, doc, line, line_number, offset, column, indent, container, paragraph, &
                          paragraph_first, paragraph_last, node, consumed, paragraph_taken, stat, errmsg)
   !< Open a block at the current position of a line, or leave `node = 0` (not mine).
   !<
   !< The line is `line` (its ending excluded), line `line_number` of the document's source; `line(offset:)` is not
   !< consumed yet (the containers' markers before it are), and starts at column `column` (0-based, tab stops of 4,
   !< CommonMark 2.2); `indent` columns of white space follow it (4 or more: an indented code block, for the core). The
   !< new block opens in `container`. If a paragraph is open there, `paragraph` is it and `paragraph_first(i)`,
   !< `paragraph_last(i)` are the bytes of its lines' content in `doc%source()` (empty arrays and 0 otherwise); a block
   !< that turns the paragraph's last lines into something else (a table header, a setext heading) reads them and sets
   !< `paragraph_taken` to how many it takes: they leave the paragraph (a paragraph left with none is removed).
   !<
   !< To claim the position: create a detached block node (`doc%new_node`, a kind of the extension), set
   !< `node` to it and `consumed` to the bytes of `line(offset:)` its marker takes. The parser links the node, closing
   !< what it must (`can_contain`), sets its span, and calls `block_continue` on the next lines.
   !<
   !< Contract (the parser sets `stat = BARUCH_ERR_CONTRACT` and stops, never loops): `node` is a new detached block
   !< node; `0 <= consumed <= len(line) - offset + 1`; `consumed > 0` unless the block takes lines (`CONTENT_LINES`:
   !< the rest of the line is its first line); `0 <= paragraph_taken <= size(paragraph_first)`.
   class(markdown_extension), intent(in)    :: self               !< Extension.
   type(markdown_document),   intent(inout) :: doc                !< Document.
   character(*),              intent(in)    :: line               !< The line, its ending excluded.
   integer(I4P),              intent(in)    :: line_number        !< Its number in the source.
   integer(I4P),              intent(in)    :: offset             !< First byte of `line` not consumed.
   integer(I4P),              intent(in)    :: column             !< Column of `line(offset:)`.
   integer(I4P),              intent(in)    :: indent             !< Columns of white space from `offset`.
   integer(I4P),              intent(in)    :: container          !< Block the new block opens in.
   integer(I4P),              intent(in)    :: paragraph          !< Open paragraph in it, 0 if none.
   integer(I4P),              intent(in)    :: paragraph_first(:) !< First byte of each paragraph line's content.
   integer(I4P),              intent(in)    :: paragraph_last(:)  !< Last byte of each.
   integer(I4P),              intent(out)   :: node               !< New block, 0: not mine.
   integer(I4P),              intent(out)   :: consumed           !< Bytes of `line(offset:)` its marker takes.
   integer(I4P),              intent(out)   :: paragraph_taken    !< Last lines of the paragraph it takes.
   integer(I4P),              intent(inout) :: stat               !< Status: left 0 unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg             !< Message, set with a nonzero `stat`.

   node = 0_I4P
   consumed = 0_I4P
   paragraph_taken = 0_I4P
   associate(unused => [doc%root(), len(line, kind=I4P), line_number, offset, column, indent, container, paragraph, &
                        size(paragraph_first, kind=I4P), size(paragraph_last, kind=I4P), stat,                     &
                        merge(1_I4P, 0_I4P, allocated(errmsg))])
   endassociate
   endsubroutine block_start

   subroutine block_continue(self, doc, node, line, line_number, offset, column, indent, match, consumed, stat, errmsg)
   !< Say whether a line continues an open block of the extension: `match` is `BLOCK_MATCHED` (and `consumed` the
   !< bytes of `line(offset:)` its continuation marker takes), `BLOCK_UNMATCHED` (the block closes, with what it
   !< holds) or `BLOCK_FINISHED` (the line is the block's last, a closing fence: the block takes all of it and
   !< closes). The arguments are `block_start`'s. Called only for the extension's own open blocks, outermost first.
   !< Default: `BLOCK_UNMATCHED`.
   !<
   !< Contract: `0 <= consumed <= len(line) - offset + 1`, else `stat = BARUCH_ERR_CONTRACT`.
   class(markdown_extension), intent(in)    :: self        !< Extension.
   type(markdown_document),   intent(inout) :: doc         !< Document.
   integer(I4P),              intent(in)    :: node        !< The open block.
   character(*),              intent(in)    :: line        !< The line, its ending excluded.
   integer(I4P),              intent(in)    :: line_number !< Its number in the source.
   integer(I4P),              intent(in)    :: offset      !< First byte of `line` not consumed.
   integer(I4P),              intent(in)    :: column      !< Column of `line(offset:)`.
   integer(I4P),              intent(in)    :: indent      !< Columns of white space from `offset`.
   integer(I4P),              intent(out)   :: match       !< `BLOCK_UNMATCHED`, `BLOCK_MATCHED`, `BLOCK_FINISHED`.
   integer(I4P),              intent(out)   :: consumed    !< Bytes of `line(offset:)` its marker takes.
   integer(I4P),              intent(inout) :: stat        !< Status: left 0 unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg      !< Message, set with a nonzero `stat`.

   match = BLOCK_UNMATCHED
   consumed = 0_I4P
   associate(unused => [doc%root(), node, len(line, kind=I4P), line_number, offset, column, indent, stat, &
                        merge(1_I4P, 0_I4P, allocated(errmsg))])
   endassociate
   endsubroutine block_continue

   subroutine block_close(self, doc, node, lines_first, lines_last, stat, errmsg)
   !< Finish a block of the extension: it closes, its span is set. A block taking lines (`CONTENT_LINES`) gets them,
   !< the bytes `lines_first(i)` to `lines_last(i)` of `doc%source()` (what is left of each line after the markers of
   !< its containers and its own); other blocks get empty arrays. Default: nothing.
   class(markdown_extension), intent(in)    :: self           !< Extension.
   type(markdown_document),   intent(inout) :: doc            !< Document.
   integer(I4P),              intent(in)    :: node           !< The block.
   integer(I4P),              intent(in)    :: lines_first(:) !< First byte of each content line.
   integer(I4P),              intent(in)    :: lines_last(:)  !< Last byte of each.
   integer(I4P),              intent(inout) :: stat           !< Status: left 0 unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg         !< Message, set with a nonzero `stat`.

   associate(unused => [doc%root(), node, size(lines_first, kind=I4P), size(lines_last, kind=I4P), stat, &
                        merge(1_I4P, 0_I4P, allocated(errmsg))])
   endassociate
   endsubroutine block_close

   ! content rules
   function block_content(self, kind) result(content)
   !< Return what a block of one of the extension's kinds holds: `CONTENT_BLOCKS` (a container), `CONTENT_LINES` (a
   !< leaf taking the rest of each line) or `CONTENT_NONE` (a one-line leaf). Default: `CONTENT_NONE`.
   class(markdown_extension), intent(in) :: self    !< Extension.
   integer(I4P),              intent(in) :: kind    !< One of the extension's kinds.
   integer(I4P)                          :: content !< `CONTENT_NONE`, `CONTENT_BLOCKS`, `CONTENT_LINES`.

   content = CONTENT_NONE
   associate(unused => kind)
   endassociate
   endfunction block_content

   function can_contain(self, kind, child_kind) result(can)
   !< Return true if a container of one of the extension's kinds (`CONTENT_BLOCKS`) can hold a block of `child_kind`
   !< (core or registered); when it cannot, the parser closes the container and opens the child in its parent.
   !< Default: any block.
   class(markdown_extension), intent(in) :: self       !< Extension.
   integer(I4P),              intent(in) :: kind       !< One of the extension's kinds.
   integer(I4P),              intent(in) :: child_kind !< Kind of the block to open in it.
   logical                               :: can        !< Result.

   can = .true.
   associate(unused => [kind, child_kind])
   endassociate
   endfunction can_contain

   function contains_inlines(self, kind) result(inlines)
   !< Return true if the content of a block of one of the extension's kinds is inline content: the inline parser then
   !< reads the block as it reads a paragraph, when the block holds one text node and nothing else (`block_close` makes
   !< it: `STRING_LITERAL` the content, lines joined by LF; its span starting at the content's first source byte, each
   !< later line ending where its source line ends). Default: false.
   class(markdown_extension), intent(in) :: self    !< Extension.
   integer(I4P),              intent(in) :: kind    !< One of the extension's kinds.
   logical                               :: inlines !< Result.

   inlines = .false.
   associate(unused => kind)
   endassociate
   endfunction contains_inlines

   ! inlines
   function inline_triggers(self) result(chars)
   !< Return the bytes that hand an inline position to the extension: the inline parser calls `inline_start` at each
   !< of them, in the subject of every block it parses. Each byte must be a graphic ASCII character (`!` to `~`) that
   !< is not a core trigger (`` ` `` `\` `&` `<` `*` `_` `[` `]` `!`: the core's stay the core's); another byte is a
   !< contract violation, reported when the inline phase starts. Default: none.
   class(markdown_extension), intent(in) :: self  !< Extension.
   character(:), allocatable             :: chars !< Trigger bytes.

   chars = ''
   endfunction inline_triggers

   subroutine inline_start(self, doc, text, offset, node, consumed, stat, errmsg)
   !< Parse an inline construct of the extension at `text(offset:)`, where one of its trigger bytes stands, or leave
   !< `node = 0` (not mine: the byte is offered to the next extension, then read as text).
   !<
   !< `text` is the subject the inline parser reads, the content of the block being parsed: its lines without their
   !< leading white space, joined by LF; `text(:offset-1)` is already parsed (the hook may look at it, as a construct
   !< allowed only at a word start does). To claim the position the hook creates an inline node of one of its kinds
   !< (`doc%new_node`), detached, and sets `consumed` to the bytes of `text(offset:)` it takes. The parser links the
   !< node after what precedes it, sets its span to the source bytes of `text(offset:offset+consumed-1)` and goes on
   !< after them. The node may have children the hook built (their spans are the hook's: the subject's offsets map to
   !< the source only through the parser); a leaf with its content as `STRING_LITERAL` is the usual case.
   !<
   !< Contract (`stat = BARUCH_ERR_CONTRACT`, the parse stops): `node` is a node created by this call, detached, of an
   !< inline kind of an extension; `1 <= consumed <= len(text) - offset + 1` (every claim makes progress).
   class(markdown_extension), intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(inout) :: doc      !< Document.
   character(*),              intent(in)    :: text     !< The subject: the block's inline content.
   integer(I4P),              intent(in)    :: offset   !< Position of the trigger byte.
   integer(I4P),              intent(out)   :: node     !< New inline node, 0: not mine.
   integer(I4P),              intent(out)   :: consumed !< Bytes of `text(offset:)` it takes.
   integer(I4P),              intent(inout) :: stat     !< Status: left 0 unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg   !< Message, set with a nonzero `stat`.

   node = 0_I4P
   consumed = 0_I4P
   associate(unused => [doc%root(), len(text, kind=I4P), offset, stat, merge(1_I4P, 0_I4P, allocated(errmsg))])
   endassociate
   endsubroutine inline_start

   function delimiter_chars(self) result(chars)
   !< Return the characters whose runs take part in the delimiter algorithm on the extension's behalf (`~` for
   !< strikethrough): a run of one is read as emphasis runs are (its flanking computed), `delimiter_run` says whether
   !< it can open or close, and the core's `process_emphasis` asks `delimiter_match` to pair its closers with openers.
   !< The rules are `inline_triggers`'s (graphic ASCII, no core trigger); one character belongs to one extension (two
   !< enabled extensions declaring it: `BARUCH_ERR_ARGUMENT`). At a byte that is both, `inline_start` is tried first.
   !< Default: none.
   class(markdown_extension), intent(in) :: self  !< Extension.
   character(:), allocatable             :: chars !< Delimiter characters.

   chars = ''
   endfunction delimiter_chars

   subroutine delimiter_run(self, doc, text, first, length, left_flanking, right_flanking, before_punct, after_punct, &
                            can_open, can_close)
   !< Say whether a run of one of the extension's delimiter characters, `text(first:first+length-1)`, can open and
   !< whether it can close; a run that can do neither is text. The parser gives the run's flanking (CommonMark 6.2:
   !< Unicode White_Space and punctuation around it, the start and the end of the subject as white space) and whether
   !< punctuation stands before and after it. Default: the rule of `*`, `can_open = left_flanking`, `can_close =
   !< right_flanking`.
   class(markdown_extension), intent(in)  :: self           !< Extension.
   type(markdown_document),   intent(in)  :: doc            !< Document.
   character(*),              intent(in)  :: text           !< The subject.
   integer(I4P),              intent(in)  :: first          !< First byte of the run.
   integer(I4P),              intent(in)  :: length         !< Its characters.
   logical,                   intent(in)  :: left_flanking  !< The run is left-flanking.
   logical,                   intent(in)  :: right_flanking !< The run is right-flanking.
   logical,                   intent(in)  :: before_punct   !< Punctuation before it.
   logical,                   intent(in)  :: after_punct    !< Punctuation after it.
   logical,                   intent(out) :: can_open       !< It can open.
   logical,                   intent(out) :: can_close      !< It can close.

   can_open = left_flanking
   can_close = right_flanking
   associate(unused => [doc%root(), len(text, kind=I4P), first, length, merge(1_I4P, 0_I4P, before_punct), &
                        merge(1_I4P, 0_I4P, after_punct)])
   endassociate
   endsubroutine delimiter_run

   subroutine delimiter_match(self, doc, delimiter, opener_length, closer_length, opener_original, closer_original, &
                              opener_can_close, closer_can_open, used, node, stat, errmsg)
   !< Pair a closer run of one of the extension's delimiter characters with an opener run of the same character, the
   !< nearest one below it not refused yet: set `used` to the characters taken from each run (0: no match, the search
   !< goes on below) and, on a match, `node` to a new detached inline node of one of the extension's kinds. The parser
   !< moves the nodes between the two runs into it, sets its span (the used characters included), links it, drops the
   !< delimiters between, and tries the closer again while characters are left in it, as for emphasis. The hook sees
   !< what the core's emphasis rules see: the characters left in each run, their lengths as read, and whether the
   !< opener can also close and the closer also open. A closer that finds no opener bounds the search of the later
   !< closers of the same character and the same `closer_can_open` (the matching stays linear).
   !<
   !< Contract (`stat = BARUCH_ERR_CONTRACT`): `0 <= used <= min(opener_length, closer_length)`; with `used > 0`,
   !< `node` is a node created by this call, detached, childless, of an inline kind of an extension; with `used = 0`,
   !< `node = 0`. Default: no match.
   class(markdown_extension), intent(in)    :: self             !< Extension.
   type(markdown_document),   intent(inout) :: doc              !< Document.
   character,                 intent(in)    :: delimiter        !< Delimiter character.
   integer(I4P),              intent(in)    :: opener_length    !< Characters left in the opener run.
   integer(I4P),              intent(in)    :: closer_length    !< Characters left in the closer run.
   integer(I4P),              intent(in)    :: opener_original  !< Characters of the opener run as read.
   integer(I4P),              intent(in)    :: closer_original  !< Characters of the closer run as read.
   logical,                   intent(in)    :: opener_can_close !< The opener run can also close.
   logical,                   intent(in)    :: closer_can_open  !< The closer run can also open.
   integer(I4P),              intent(out)   :: used             !< Characters taken from each run, 0: no match.
   integer(I4P),              intent(out)   :: node             !< New inline node wrapping the content, 0 if none.
   integer(I4P),              intent(inout) :: stat             !< Status: left 0 unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg           !< Message, set with a nonzero `stat`.

   used = 0_I4P
   node = 0_I4P
   associate(unused => [doc%root(), iachar(delimiter, kind=I4P), opener_length, closer_length, opener_original, &
                        closer_original, merge(1_I4P, 0_I4P, opener_can_close), merge(1_I4P, 0_I4P, closer_can_open), &
                        stat, merge(1_I4P, 0_I4P, allocated(errmsg))])
   endassociate
   endsubroutine delimiter_match

   ! post-processing
   subroutine post_process(self, doc, stat, errmsg)
   !< Edit the document once it is parsed, blocks and inlines (a task list item's checkbox, footnote numbering, an
   !< autolink found in text). Called by `markdown_options%parse` after the inline phase, once per parse, the enabled
   !< extensions in registration order; the tree is compacted after them when they unlinked nodes. Default: nothing.
   class(markdown_extension), intent(in)    :: self   !< Extension.
   type(markdown_document),   intent(inout) :: doc    !< Document.
   integer(I4P),              intent(inout) :: stat   !< Status: left 0 unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg !< Message, set with a nonzero `stat`.

   associate(unused => [doc%root(), stat, merge(1_I4P, 0_I4P, allocated(errmsg))])
   endassociate
   endsubroutine post_process

   ! output (write_markdown: called from plan step 3.2)
   subroutine write_html(self, doc, node, entering, html, handled, stat, errmsg)
   !< Write a node as HTML, on entering it (or reaching it, for a text) and on leaving it (after its children); a node
   !< the extension does not write: `handled = .false.`. The renderer offers every event of every node, core kinds
   !< too, to the enabled extensions in registration order; the first that handles it writes `html` (its own line
   !< feeds included) and the core writes nothing for that event. A nonzero `stat` stops the render. Default: not
   !< mine (the renderer writes the node its own way; an extension node no extension writes is rendered
   !< transparently: its children, or its literal escaped).
   class(markdown_extension), intent(in)    :: self     !< Extension.
   type(markdown_document),   intent(in)    :: doc      !< Document.
   integer(I4P),              intent(in)    :: node     !< Node.
   logical,                   intent(in)    :: entering !< Entering the node (else leaving it).
   character(:), allocatable, intent(out)   :: html     !< HTML written for this event.
   logical,                   intent(out)   :: handled  !< True if the node is the extension's.
   integer(I4P),              intent(inout) :: stat     !< Status: left 0 unless the hook fails.
   character(:), allocatable, intent(inout) :: errmsg   !< Message, set with a nonzero `stat`.

   html = ''
   handled = .false.
   associate(unused => [doc%root(), node, merge(1_I4P, 0_I4P, entering), stat, merge(1_I4P, 0_I4P, allocated(errmsg))])
   endassociate
   endsubroutine write_html

   subroutine walk_event(self, doc, node, entering, event, handled)
   !< Say which walk event (`EVENT_*` of `baruch_walk`) a node of the extension is reported as: entering it,
   !< `EVENT_ENTER_BLOCK`, `EVENT_ENTER_SPAN` or `EVENT_TEXT` (a text has no children visited and is never left);
   !< leaving it, the matching `EVENT_LEAVE_BLOCK` or `EVENT_LEAVE_SPAN`. Any other answer is a contract violation
   !< (`BARUCH_ERR_CONTRACT`). Only extension kinds are offered; a node that is not the extension's:
   !< `handled = .false.`. Default: not mine (the walk reports a block or a span, as the kind was registered).
   class(markdown_extension), intent(in)  :: self     !< Extension.
   type(markdown_document),   intent(in)  :: doc      !< Document.
   integer(I4P),              intent(in)  :: node     !< Node.
   logical,                   intent(in)  :: entering !< Entering the node (else leaving it).
   integer(I4P),              intent(out) :: event    !< Event reported.
   logical,                   intent(out) :: handled  !< True if the node is the extension's.

   event = 0_I4P
   handled = .false.
   associate(unused => [doc%root(), node, merge(1_I4P, 0_I4P, entering)])
   endassociate
   endsubroutine walk_event
endmodule baruch_extension
