module fy_AbstractTextStream
  use, intrinsic :: iso_c_binding, only: C_NEW_LINE, C_CARRIAGE_RETURN
  implicit none
  private

  public :: AbstractTextStream

  type, abstract :: AbstractTextStream
     private
   contains
     procedure(read), deferred :: read
  end type AbstractTextStream


  abstract interface

     ! An empty read indicated EOF to clients
     function read(this, n_characters) result(buffer)
       import AbstractTextStream
       character(:), allocatable :: buffer
       class(AbstractTextStream), intent(inout) :: this
       integer, intent(in) :: n_characters
     end function read

  end interface


end module fy_AbstractTextStream
