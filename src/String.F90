! This module is a workaround for limitations in gfortran 9.2
! for when character variables are stored in unlimited polymorphic
! entities.   Wrapping strings in derived types appears to bypass
! the issue, but does entail various bits of tedious coding
! throughout this package.

module fy_String

   implicit none
   private

   public :: String
   public :: toString
   public :: len

   type :: String
      character(len=:), allocatable :: s
   contains
      procedure :: toString => toString_self ! override toString() from Object
      procedure :: get
      generic :: assignment(=) => copyFromString, copyToString !, copySelf
      generic :: operator(==) => equal_str_string, equal_string_str, equal_string_string
      generic :: operator(/=) => notEqual_str_string, notEqual_string_str, notEqual_string_string

      procedure, pass(a) :: copySelf
      procedure, pass(a) :: copyFromString
      procedure, pass(b) :: copyToString
      procedure :: equal_string_string
      procedure, pass(b) :: equal_str_string
      procedure :: equal_string_str
      procedure :: notEqual_string_string
      procedure, pass(b) :: notEqual_str_string
      procedure :: notEqual_string_str
   end type String


   interface String
      module procedure newString_empty
      module procedure newString_str
    end interface

   interface toString
      module procedure toString_self
   end interface toString


   interface len
      module procedure len_string
   end interface len


contains


   function newString_empty()
      type (String) :: newString_empty
      newString_empty = String('')
   end function newString_empty


   function newString_str(str)
      type (String) :: newString_str
      character(len=*), intent(in) :: str

      newString_str%s = str

   end function newString_str


   pure function toString_self(this) result(str)
      character(len=:), allocatable :: str
      class (String), intent(in) :: this
      str = this%s
   end function toString_self

   
   function get(this) result(str)
      character(len=:), pointer :: str
      class (String), target, intent(in) :: this
      str => this%s
   end function get

   
   subroutine copySelf(a, b)
      class (String), intent(inout) :: a
      class (String), intent(in) :: b

      a = b%toString()

   end subroutine copySelf


   subroutine copyFromString(a, b)
      class (String), intent(inout) :: a
      character(len=*), intent(in) :: b

      a%s = trim(b)

   end subroutine copyFromString


   subroutine copyToString(a, b)
      character(len=:), allocatable, intent(out) :: a
      class (String), intent(in) :: b

      a = b%s

   end subroutine copyToString
   

   logical function equal_string_string(a, b) result(areEqual)
      class (String), intent(in) :: a
      class (String), intent(in) :: b

      areEqual = (a%s == b%s)
      
   end function equal_string_string


   logical function equal_str_string(a, b)  result(areEqual)
      character(len=*), intent(in) :: a
      class (String), intent(in) :: b

      areEqual = (a == b%s)
      
   end function equal_str_string

   logical function equal_string_str(a, b)  result(areEqual)
      class (String), intent(in) :: a
      character(len=*), intent(in) :: b

      areEqual = (a%s == b)
      
   end function equal_string_str


   logical function notEqual_string_string(a, b) result(notEqual)
      class (String), intent(in) :: a
      class (String), intent(in) :: b

      notEqual = (.not. (a == b))
      
   end function notEqual_string_string


   logical function notEqual_str_string(a, b)  result(notEqual)
      character(len=*), intent(in) :: a
      class (String), intent(in) :: b

      notEqual = (.not. (a == b))
      
   end function notEqual_str_string


   logical function notEqual_string_str(a, b)  result(notEqual)
      class (String), intent(in) :: a
      character(len=*), intent(in) :: b

      notEqual = (.not. (a == b))
      
   end function notEqual_string_str


   integer function len_string(str)
      type (String), intent(in) :: str
      len_string = len(str%s)
   end function len_string


!!$  public :: String
!!$
!!$  type :: String
!!$     character(:), allocatable :: s
!!$  end type String
!!$
end module fy_String

