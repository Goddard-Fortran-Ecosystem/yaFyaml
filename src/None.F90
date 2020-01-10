module fy_None
  implicit none
  private

  public :: None
  
  type :: NoneType
     private
     ! Intel compiler treats the singleton None object below as
     ! ininitialized ("common") unless we put something in the
     ! type. This prevents linking.
     integer :: placeholder=0
  end type NoneType
  
  type (NoneType), save, target :: None

end module fy_None
