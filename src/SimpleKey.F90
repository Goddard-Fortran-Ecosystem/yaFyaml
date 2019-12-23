! A simple key is a key that is not explicitly denoted by the '?' indicator.
!
! Examples
!   ---
!   block simple key: value
!   ? not a simple key:
!   : { flow simple key: value }
!
module fy_SimpleKey
  implicit none
  private

  public :: SimpleKey


  type :: SimpleKey
     integer :: column
     integer :: line
     integer :: index
     integer :: token_number
     logical :: required
  end type SimpleKey

end module fy_SimpleKey
