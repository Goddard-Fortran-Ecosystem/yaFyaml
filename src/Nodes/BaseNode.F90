#include "error_handling.h"
#include "string_handling.h"
module fy_BaseNode
   use fy_AbstractNode
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: BaseNode

   type, abstract, extends(AbstractNode) :: BaseNode
      private
   contains

      procedure, NON_OVERRIDABLE :: at_multi_selector
      procedure, NON_OVERRIDABLE :: of_multi_selector
      procedure :: get_logical
      procedure :: get_string
      procedure :: get_integer32
      procedure :: get_integer64
      procedure :: get_real32
      procedure :: get_real64


      procedure, pass(this) :: assign_to_logical
      procedure, pass(this) :: assign_to_string
      procedure, pass(this) :: assign_to_integer32
      procedure, pass(this) :: assign_to_integer64
      procedure, pass(this) :: assign_to_real32
      procedure, pass(this) :: assign_to_real64

      procedure, nopass :: is_sequence
      procedure, nopass :: is_mapping
      procedure, nopass :: is_scalar
      procedure, nopass :: is_bool
      procedure, nopass :: is_string
      procedure, nopass :: is_int
      procedure, nopass :: is_float

   end type BaseNode

#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s2, s3, s4, s5, s6, s7, s8, s9
   interface
      module function at_multi_selector(this, SELECTORS, &
           & unusable, found, err_msg, rc) result(node_ptr)
         use fy_KeywordEnforcer
         implicit none
         class(AbstractNode), pointer :: node_ptr
         class(BaseNode), target, intent(in) :: this
         class(*), intent(in) :: s1
         class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function at_multi_selector


      module subroutine get_logical(this, value, SELECTORS, unusable, found, default, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         logical, intent(out) :: value
         class(*), intent(in) :: s1
         class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         logical, optional, intent(in) :: default
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_logical


      module subroutine get_string(this, value, SELECTORS, unusable, found, default, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         character(:), allocatable, intent(out) :: value
         class(*), intent(in) :: s1
         class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         character(*), optional, intent(in) :: default
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_string


      module subroutine get_integer32(this, value, SELECTORS, unusable, found, default, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         integer(kind=INT32), intent(out) :: value
         class(*), intent(in) :: s1
         class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         integer(kind=INT32), optional, intent(in) :: default
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer32


      module subroutine get_integer64(this, value, SELECTORS, unusable, found, default, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         integer(kind=INT64), intent(out) :: value
         class(*), intent(in) :: s1
         class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         integer(kind=INT64), optional, intent(in) :: default
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer64


      module subroutine get_real32(this, value, SELECTORS, unusable, found, default, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         real(kind=REAL32), intent(out) :: value
         class(*), intent(in) :: s1
         class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         real(kind=REAL32), optional, intent(in) :: default
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real32

      module subroutine get_real64(this, value, SELECTORS, unusable, found, default, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         real(kind=REAL64), intent(out) :: value
         class(*), intent(in) :: s1
         class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         real(kind=REAL64), optional, intent(in) :: default
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real64

   end interface

contains


   function of_multi_selector(this, SELECTORS) result(ptr)
      class(AbstractNode), pointer :: ptr
      class(BaseNode), target, intent(in) :: this
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      ptr => this%at(SELECTORS)
   end function of_multi_selector

   subroutine assign_to_logical(flag, this)
      logical, intent(out) :: flag
      class(BaseNode), intent(in) :: this
      flag = .false. ! a terrible default, but the best of the two options.
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_logical
    

   subroutine assign_to_string(string, this)
      character(:), allocatable, intent(out) :: string
      class(BaseNode), intent(in) :: this
      string = ''
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_string
    

   subroutine assign_to_integer32(i32, this)
      integer(kind=INT32), intent(out) :: i32
      class(BaseNode), intent(in) :: this
      i32 = -huge(1_INT32)
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_integer32
    
   
   subroutine assign_to_integer64(i64, this)
      integer(kind=INT64), intent(out) :: i64
      class(BaseNode), intent(in) :: this
      i64 = -huge(1_INT64)
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_integer64
    
   
   subroutine assign_to_real32(r32, this)
      use, intrinsic :: ieee_arithmetic, only: IEEE_QUIET_NAN, ieee_value
      real(kind=REAL32), intent(out) :: r32
      class(BaseNode), intent(in) :: this
      r32 = ieee_value(r32,  IEEE_QUIET_NAN)
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_real32
    
   
   subroutine assign_to_real64(r64, this)
      use, intrinsic :: ieee_arithmetic, only: IEEE_QUIET_NAN, ieee_value
      real(kind=REAL64), intent(out) :: r64
      class(BaseNode), intent(in) :: this
      r64 = ieee_value(r64,  IEEE_QUIET_NAN)
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_real64

   pure logical function is_bool() result(is)
      is = .false.
   end function is_bool

   pure logical function is_int() result(is)
      is = .false.
   end function is_int

   pure logical function is_string() result(is)
      is = .false.
   end function is_string

   pure logical function is_float() result(is)
      is = .false.
   end function is_float

   pure logical function is_scalar() result(is)
      is = .false.
   end function is_scalar

   pure logical function is_sequence() result(is)
      is = .false.
   end function is_sequence

   pure logical function is_mapping() result(is)
      is = .false.
   end function is_mapping

end module fy_BaseNode
