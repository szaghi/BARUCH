type, extends(markdown_extension) :: percent_extension
   !< `%%` lines: one block taking the rest of each line.
   contains
      procedure, pass(self) :: name           => percent_name
      procedure, pass(self) :: register_kinds => percent_register_kinds
      procedure, pass(self) :: block_start    => percent_block_start
      procedure, pass(self) :: block_continue => percent_block_continue
      procedure, pass(self) :: block_content  => percent_block_content
      procedure, pass(self) :: block_close    => percent_block_close
      procedure, pass(self) :: write_markdown => percent_write_markdown
      ! inline: `%name%`
      procedure, pass(self) :: inline_triggers => percent_inline_triggers
      procedure, pass(self) :: inline_start    => percent_inline_start
endtype percent_extension
