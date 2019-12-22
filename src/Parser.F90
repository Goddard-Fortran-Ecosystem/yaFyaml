!!! The Parser imports a sequence of tokens and constructs a
!!! configuration object.  I naively expect this to be rather simple
!!! compared to the Lexer, but reading suggests that it should be the
!!! opposite.  The difference may in part be that this package restricts
!!! keys to be simple strings.  

module fy_Parser
  use Lexer
  implicit none
  private

  public :: Parser

  type :: Parser
     private
     type (Lexer) :: lexer
   contains
     procedure parse
  end type Parser


  interface Parser
     module procedure new_Parser_stream
     module procedure new_Parser_filename
  end interface Parser

contains

  function new_Parser(stream) result(p)
    class(TextStream), intent(inout) :: stream
    type(Parser) :: p

    p%lexr = Lexer(stream)

  end function new_Parser

  function new_Parser_filename(filename) result(p)
    type(Parser) :: p
    character(*), intent(in) :: filename

    p%lexr = Lexer(FileStream(filename))

  end function new_Parser_filename
  
end module fy_Parser
