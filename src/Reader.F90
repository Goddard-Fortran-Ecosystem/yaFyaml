module fy_Reader
  use fy_AbstractStream
  use fy_KeywordEnforcer
  implicit none
  private

  public :: Reader

  type :: Reader
     private
     class(AbstractStream), allocatable :: stream
   contains
     procedure(peek), deferred :: peek
     procedure(forward), deferred :: forward
     procedure(scan_line_break), deferred :: scan_line_break
  end type Reader

contains
  
  function peek(this, unused, n) result(c)
    character(1) :: c
    class(ReaderAbstractStream), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    integer, optional, intent(in) :: n
  end function peek

  subroutine forward(this, unused, n)
    class(Reader), intent(inout) :: this
    class(KeywordEnforcer), optional, intent(in) :: unused
    integer, optional, intent(in) :: n
  end subroutine forward

end module fy_Reader
