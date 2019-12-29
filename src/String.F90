! This module is a workaround for limitations in gfortran 9.2
! for when character variables are stored in unlimited polymorphic
! entities.   Wrapping strings in derived types appears to bypass
! the issue, but does entail various bits of tedious coding
! throughout this package.

module fy_String

  public :: String

  type :: String
     character(:), allocatable :: s
  end type String

end module fy_String

