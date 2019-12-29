module fy_AbstractSchema
  implicit none
  private

  public :: AbstractSchema

  type, abstract :: AbstractSchema
     private
   contains
     procedure(matches), deferred, nopass :: matches_null
     procedure(matches), deferred, nopass :: matches_logical
     procedure(matches), deferred, nopass :: matches_integer
     procedure(matches), deferred, nopass :: matches_real
  end type AbstractSchema

  abstract interface

     logical function matches(text)
       import AbstractSchema
       character(*), intent(in) :: text
     end function matches

  end interface

end module fy_AbstractSchema
