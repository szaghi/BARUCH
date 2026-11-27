type, extends(markdown_extension) :: mark_extension
   !< `==text==` or `=text=`: runs of `=` taking part in the delimiter algorithm.
   contains
      procedure, pass(self) :: name            => mark_name
      procedure, pass(self) :: register_kinds  => mark_register_kinds
      procedure, pass(self) :: delimiter_chars => mark_delimiter_chars
      procedure, pass(self) :: delimiter_run   => mark_delimiter_run
      procedure, pass(self) :: delimiter_match => mark_delimiter_match
      procedure, pass(self) :: write_html      => mark_write_html
      procedure, pass(self) :: write_markdown  => mark_write_markdown
endtype mark_extension
