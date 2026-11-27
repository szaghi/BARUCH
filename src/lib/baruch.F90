!< BARUCH, Bidirectional AST Reader and Unparser for CommonMark, Hookable: the library's entry point.
module baruch
!< BARUCH, Bidirectional AST Reader and Unparser for CommonMark, Hookable: the library's entry point.
!<
!< Seed of the repository: the module exports nothing yet. The parser (CommonMark blocks and inlines into a document
!< tree), the emitter (the tree back to Markdown, semantic round trip), the streaming writer and the opt-in MyST and
!< VitePress extensions are added behind this module, so a client needs `use baruch` only.
!<
!< Kinds come from PENF and text from StringiFor, used here so that the library archive holds them; neither is
!< re-exported, a client uses them directly.
use penf, only : I4P
use stringifor, only : string
implicit none
private
endmodule baruch
