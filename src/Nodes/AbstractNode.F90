#include "string_handling.h"
module fy_AbstractNode
   use fy_KeywordEnforcer
   implicit none
   private

   public :: AbstractNode

   type, abstract :: AbstractNode
      integer :: ID = 0
   contains

      ! accessors to sub-nodes
      procedure(I_at_multi_selector), deferred :: at_multi_selector
      generic :: at => at_multi_selector
      procedure(I_of_multi_selector), deferred :: of_multi_selector
      generic :: of => of_multi_selector

      procedure(I_get_logical),   deferred :: get_logical
      procedure(I_get_string),    deferred :: get_string
      procedure(I_get_integer32), deferred :: get_integer32
      procedure(I_get_integer64), deferred :: get_integer64
      procedure(I_get_real32),    deferred :: get_real32
      procedure(I_get_real64),    deferred :: get_real64
      generic :: get => get_logical
      generic :: get => get_string
      generic :: get => get_integer32
      generic :: get => get_integer64
      generic :: get => get_real32
      generic :: get => get_real64

      procedure(I_assign_to_logical),   pass(this), deferred :: assign_to_logical
      procedure(I_assign_to_string),    pass(this), deferred :: assign_to_string
      procedure(I_assign_to_integer32), pass(this), deferred :: assign_to_integer32
      procedure(I_assign_to_integer64), pass(this), deferred :: assign_to_integer64
      procedure(I_assign_to_real32),    pass(this), deferred :: assign_to_real32
      procedure(I_assign_to_real64),    pass(this), deferred :: assign_to_real64
      generic :: assignment(=) => assign_to_logical
      generic :: assignment(=) => assign_to_string
      generic :: assignment(=) => assign_to_integer32
      generic :: assignment(=) => assign_to_integer64
      generic :: assignment(=) => assign_to_real32
      generic :: assignment(=) => assign_to_real64

      ! using YAML terminology
      procedure(I_is), deferred, nopass :: is_sequence
      procedure(I_is), deferred, nopass :: is_mapping
      procedure(I_is), deferred, nopass :: is_scalar
      procedure(I_is), deferred, nopass :: is_bool
      procedure(I_is), deferred, nopass :: is_string
      procedure(I_is), deferred, nopass :: is_int
      procedure(I_is), deferred, nopass :: is_float

!!$      procedure(I_write_formatted) :: write_formatted
!!$      generic :: write(formatted) => write_formatted

      ! "<"
      ! Necessary to support map container with *Node keys.
      procedure(I_less_than), deferred :: less_than
      generic :: operator(<) => less_than

      procedure :: analysis
   end type AbstractNode

#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
   abstract interface

      
      function I_of_multi_selector(this, SELECTORS) result(node_ptr)
         import AbstractNode
         implicit none
         class(AbstractNode), pointer :: node_ptr
         class(AbstractNode), target, intent(in) :: this
         class(*), optional, intent(in) :: OPT_SELECTORS
      end function I_of_multi_selector


      function I_at_multi_selector(this, SELECTORS, &
           & unusable, found, err_msg, rc) result(node_ptr)
         use fy_KeywordEnforcer
         import AbstractNode
         implicit none
         class(AbstractNode), pointer :: node_ptr
         class(AbstractNode), target, intent(in) :: this
         class(*), optional, intent(in) :: OPT_SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function I_at_multi_selector


      subroutine I_get_logical(this, value, SELECTORS, unusable, found, err_msg, rc)
         use fy_KeywordEnforcer
         import AbstractNode
         implicit none
         class(AbstractNode), target, intent(in) :: this
         logical, intent(out) :: value
         class(*), optional, intent(in) :: OPT_SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_logical


      subroutine I_get_string(this, value, SELECTORS, unusable, found, err_msg, rc)
         use fy_KeywordEnforcer
         import AbstractNode
         implicit none
         class(AbstractNode), target, intent(in) :: this
         character(:), allocatable, intent(out) :: value
         class(*), optional, intent(in) :: OPT_SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_string


      subroutine I_get_integer32(this, value, SELECTORS, unusable, found, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         import AbstractNode
         implicit none
         class(AbstractNode), target, intent(in) :: this
         integer(kind=INT32), intent(out) :: value
         class(*), optional, intent(in) :: OPT_SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_integer32


      subroutine I_get_integer64(this, value, SELECTORS, unusable, found, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         import AbstractNode
         implicit none
         class(AbstractNode), target, intent(in) :: this
         integer(kind=INT64), intent(out) :: value
         class(*), optional, intent(in) :: OPT_SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_integer64


      subroutine I_get_real32(this, value, SELECTORS, unusable, found, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         import AbstractNode
         implicit none
         class(AbstractNode), target, intent(in) :: this
         real(kind=REAL32), intent(out) :: value
         class(*), optional, intent(in) :: OPT_SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_real32


      subroutine I_get_real64(this, value, SELECTORS, unusable, found, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         import AbstractNode
         implicit none
         class(AbstractNode), target, intent(in) :: this
         real(kind=REAL64), intent(out) :: value
         class(*), optional, intent(in) :: OPT_SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine I_get_real64


      subroutine I_assign_to_logical(flag, this)
         import AbstractNode
         implicit none
         logical, intent(out) :: flag
         class(AbstractNode), intent(in) :: this
      end subroutine I_assign_to_logical


      subroutine I_assign_to_string(string, this)
         import AbstractNode
         implicit none
         character(:), allocatable, intent(out) :: string
         class(AbstractNode), intent(in) :: this
      end subroutine I_assign_to_string

      subroutine I_assign_to_integer32(i32, this)
         use, intrinsic :: iso_fortran_env, only: INT32
         import AbstractNode
         implicit none
         integer(kind=INT32), intent(out) :: i32
         class(AbstractNode), intent(in) :: this
      end subroutine I_assign_to_integer32

      subroutine I_assign_to_integer64(i64, this)
         use, intrinsic :: iso_fortran_env, only: INT64
         import AbstractNode
         implicit none
         integer(kind=INT64), intent(out) :: i64
         class(AbstractNode), intent(in) :: this
      end subroutine I_assign_to_integer64

      subroutine I_assign_to_real32(r32, this)
         use, intrinsic :: iso_fortran_env, only: REAL32
         import AbstractNode
         implicit none
         real(kind=REAL32), intent(out) :: r32
         class(AbstractNode), intent(in) :: this
      end subroutine I_assign_to_real32

      subroutine I_assign_to_real64(r64, this)
         use, intrinsic :: iso_fortran_env, only: REAL64
         import AbstractNode
         implicit none
         real(kind=REAL64), intent(out) :: r64
         class(AbstractNode), intent(in) :: this
      end subroutine I_assign_to_real64

      pure logical function I_is() result(is)
         import AbstractNode
      end function I_is

      logical function I_less_than(a, b)
         import AbstractNode
         implicit none
         class(AbstractNode), intent(in) :: a
         class(AbstractNode), intent(in) :: b
      end function I_less_than
   end interface


contains

   function analysis(this, prefix)result(str)
      character(:), allocatable :: str
      class(AbstractNode), target, intent(in) :: this
      character(*), intent(in) :: prefix

      str = "I am an AbstractNode - I don't know anything more"
   end function analysis

   
end module fy_AbstractNode
