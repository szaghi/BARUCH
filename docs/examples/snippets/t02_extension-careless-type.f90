type, extends(percent_extension) :: careless_extension
   !< The same, but its block holds blocks and its start consumes nothing: it breaks the contract.
   contains
      procedure, pass(self) :: name          => careless_name
      procedure, pass(self) :: block_start   => careless_block_start
      procedure, pass(self) :: block_content => careless_block_content
endtype careless_extension
