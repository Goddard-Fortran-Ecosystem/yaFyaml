module fy_AbstractTextStream
  private

  public :: AbstractTextStream

  type, abstract :: AbstractTextStream
     private
     character(:), allocatable :: buffer
   contains
     procedure(read), deferred :: read
  end type AbstractTextStream


  abstract interface

     function read(this, n_characters) result(buffer)
       import AbstractTextStream
       character(:), allocatable :: buffer
       class(AbstractTextStream), intent(inout) :: this
       integer, intent(in) :: n_characters
     end function read

  end interface

end module fy_AbstractTextStream
