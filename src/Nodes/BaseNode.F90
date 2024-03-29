#include "error_handling.h"
#include "string_handling.h"
module fy_BaseNode
   use fy_YAML_Node
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: BaseNode
!!$#ifdef __GFORTRAN__
!!$   public :: assignment(=)
!!$#endif

   type, abstract, extends(YAML_Node) :: BaseNode
      private
   contains

      procedure :: size
      procedure :: at_multi_selector
      procedure :: of_multi_selector
      procedure :: has => has_selector
      procedure :: get_logical, get_logical_1d
      procedure :: get_string
      procedure :: get_integer32, get_integer32_1d
      procedure :: get_integer64, get_integer64_1d
      procedure :: get_real32, get_real32_1d
      procedure :: get_real64, get_real64_1d

      procedure :: set_logical
      procedure :: set_string
      procedure :: set_integer32
      procedure :: set_integer64
      procedure :: set_real32
      procedure :: set_real64
      procedure :: set_logical_1d
      procedure :: set_integer32_1d
      procedure :: set_integer64_1d
      procedure :: set_real32_1d
      procedure :: set_real64_1d
      procedure :: set_node

      procedure, nopass :: is_sequence
      procedure, nopass :: is_mapping
      procedure, nopass :: is_scalar
      procedure, nopass :: is_bool
      procedure, nopass :: is_string
      procedure, nopass :: is_int
      procedure, nopass :: is_float

      procedure :: write_formatted
   end type BaseNode

#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s2, s3, s4, s5, s6, s7, s8, s9

   interface

      module function at_multi_selector(this, SELECTORS, &
           & unusable, found, err_msg, rc) result(node_ptr)
         use fy_KeywordEnforcer
         implicit none
         class(YAML_Node), pointer :: node_ptr
         class(BaseNode), target, intent(in) :: this
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(out) :: found
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function at_multi_selector


      module function has_selector(this, SELECTORS) result(has)
         implicit none
         logical :: has
         class(BaseNode), intent(in) :: this
         class(*), intent(in) :: s1 ! at least one selector required here
         class(*), optional, intent(in) :: OPT_SELECTORS
      end function has_selector

      module subroutine get_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         logical, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_logical

      module subroutine get_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         logical, allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_logical_1d


      module subroutine get_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         character(:), allocatable, intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_string


      module subroutine get_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         integer(kind=INT32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer32

      module subroutine get_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         integer(kind=INT32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer32_1d


      module subroutine get_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         integer(kind=INT64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer64


      module subroutine get_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         integer(kind=INT64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_integer64_1d


      module subroutine get_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         real(kind=REAL32), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real32

      module subroutine get_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         real(kind=REAL32), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real32_1d

      module subroutine get_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         real(kind=REAL64), intent(inout) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real64

      module subroutine get_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(in) :: this
         real(kind=REAL64), allocatable, intent(inout) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine get_real64_1d

      module subroutine set_logical(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         implicit none
         class(BaseNode), target, intent(inout) :: this
         logical, intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_logical

      module subroutine set_string(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         implicit none
         class(BaseNode), target, intent(inout) :: this
         character(*), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine  set_string

      module subroutine set_integer32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         implicit none
         class(BaseNode), target, intent(inout) :: this
         integer(kind=INT32), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_integer32

      module subroutine set_integer64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         implicit none
         class(BaseNode), target, intent(inout) :: this
         integer(kind=INT64), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_integer64


      module subroutine set_real32(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         implicit none
         class(BaseNode), target, intent(inout) :: this
         real(kind=REAL32), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_real32

      module subroutine set_real64(this, value, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         implicit none
         class(BaseNode), target, intent(inout) :: this
         real(kind=REAL64), intent(in) :: value
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_real64

      module subroutine set_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         implicit none
         class(BaseNode), target, intent(inout) :: this
         logical, intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_logical_1d

      module subroutine set_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT32
         implicit none
         class(BaseNode), target, intent(inout) :: this
         integer(kind=INT32), intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_integer32_1d

      module subroutine set_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: INT64
         implicit none
         class(BaseNode), target, intent(inout) :: this
         integer(kind=INT64), intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_integer64_1d

      module subroutine set_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL32
         implicit none
         class(BaseNode), target, intent(inout) :: this
         real(kind=REAL32), intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_real32_1d

      module subroutine set_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         use, intrinsic :: iso_fortran_env, only: REAL64
         implicit none
         class(BaseNode), target, intent(inout) :: this
         real(kind=REAL64), intent(in) :: values(:)
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_real64_1d

      module subroutine set_node(this, node, SELECTORS, unusable, err_msg, rc)
         use fy_KeywordEnforcer
         class(BaseNode), target, intent(inout) :: this
         class(YAML_Node), intent(in) :: node
         class(*), optional, intent(in) :: SELECTORS
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end subroutine set_node

   end interface


contains


   function of_multi_selector(this, SELECTORS) result(ptr)
      class(YAML_Node), pointer :: ptr
      class(BaseNode), target, intent(in) :: this
      class(*), optional, intent(in) :: SELECTORS
      ptr => this%at(SELECTORS)
   end function of_multi_selector

   subroutine assign_to_logical(flag, this)
      logical, intent(inout) :: flag
      class(BaseNode), intent(in) :: this
      __UNUSED_DUMMY__(flag)
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_logical
    

   subroutine assign_to_string(string, this)
      character(:), allocatable, intent(inout) :: string
      class(BaseNode), intent(in) :: this
      __UNUSED_DUMMY__(string)
   end subroutine assign_to_string
    

   subroutine assign_to_integer32(i32, this)
      integer(kind=INT32), intent(inout) :: i32
      class(BaseNode), intent(in) :: this
      __UNUSED_DUMMY__(i32)
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_integer32
    
   
   subroutine assign_to_integer64(i64, this)
      integer(kind=INT64), intent(inout) :: i64
      class(BaseNode), intent(in) :: this
      __UNUSED_DUMMY__(i64)
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_integer64
    
   
   subroutine assign_to_real32(r32, this)
      use, intrinsic :: ieee_arithmetic, only: IEEE_QUIET_NAN, ieee_value
      real(kind=REAL32), intent(inout) :: r32
      class(BaseNode), intent(in) :: this
      __UNUSED_DUMMY__(r32)
      __UNUSED_DUMMY__(this)
   end subroutine assign_to_real32
    
   
   subroutine assign_to_real64(r64, this)
      use, intrinsic :: ieee_arithmetic, only: IEEE_QUIET_NAN, ieee_value
      real(kind=REAL64), intent(inout) :: r64
      class(BaseNode), intent(in) :: this
      __UNUSED_DUMMY__(r64)
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

   integer function size(this)
      class(BaseNode), intent(in) :: this
      size = 1 ! overridden in SequenceNode and MappingNode
   end function size


   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(BaseNode), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      call this%write_node_formatted(unit, iotype, v_list, iostat, iomsg)
      
   end subroutine write_formatted

end module fy_BaseNode
