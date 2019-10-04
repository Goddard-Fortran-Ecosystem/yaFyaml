module fy_AbstractStream
  private

  public :: AbstractStream

  type :: AbstractStream
     private
     character(:), allocatable :: buffer
   contains
     procedure :: read
  end type AbstractStream


  abstract interface

     function read(this, n_characters) result(buffer)
       import AbstractStream
       character(:), allocatable :: buffer
       class(AbstractStream), intent(inout) :: this
       integer, optional, intent(in) :: n_characters
     end subroutine read

  end interface

end module fy_AbstractStream
