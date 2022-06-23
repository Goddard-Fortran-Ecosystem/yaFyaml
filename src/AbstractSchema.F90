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

     procedure(to_logical), deferred, nopass :: to_logical
     procedure(to_integer), deferred, nopass :: to_integer
     procedure(to_real), deferred, nopass :: to_real
  end type AbstractSchema

  abstract interface

     logical function matches(text)
       import AbstractSchema
       character(*), intent(in) :: text
     end function matches


     logical function to_logical(text,rc)
       import AbstractSchema
       character(*), intent(in) :: text
       integer, optional, intent(out) :: rc
     end function to_logical

     integer function to_integer(text,rc)
       import AbstractSchema
       character(*), intent(in) :: text
       integer, optional, intent(out) :: rc
     end function to_integer

     real function to_real(text,rc)
       import AbstractSchema
       character(*), intent(in) :: text
       integer, optional, intent(out) :: rc
     end function to_real

  end interface

end module fy_AbstractSchema
