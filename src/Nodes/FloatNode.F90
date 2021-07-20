#include "error_handling.h"
#include "string_handling.h"
module fy_FloatNode
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use fy_AbstractNode
   use fy_BaseNode
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   implicit none
   private

   public :: FloatNode
   public :: to_float

   type, extends(BaseNode) :: FloatNode
      private
      real(kind=REAL64) :: value = -huge(1._REAL64)
   contains
      procedure, nopass :: is_float
      procedure, pass(this) :: assign_to_real32
      procedure, pass(this) :: assign_to_real64
      procedure :: less_than
   end type FloatNode

   ! Better woud be NaN but cannot do that as an initialiations expr
   ! This also must be something that can be assigned to a defaut real
   ! without causing a floating exception.
   real(kind=REAL32), target :: DEFAULT_REAL32 = nearest(-huge(1._REAL32),1._REAL32)
   real(kind=REAL64), target :: DEFAULT_REAL64 = nearest(-huge(1._REAL64),1._REAL64)

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(FloatNode), intent(in) :: a
         class(AbstractNode), intent(in) :: b
      end function less_than
   end interface

   interface FloatNode
      module procedure new_FloatNode_r32
      module procedure new_FloatNode_r64
   end interface FloatNode

contains

   pure logical function is_float() result(is)
      is = .true.
   end function is_float

   function new_FloatNode_r32(r32) result(node)
      type(FloatNode) :: node
      real(kind=REAL32), intent(in) :: r32
      node%value = r32
   end function new_FloatNode_r32

   function new_FloatNode_r64(r64) result(node)
      type(FloatNode) :: node
      real(kind=REAL64), intent(in) :: r64
      node%value = r64
   end function new_FloatNode_r64


   subroutine assign_to_real32(r32, this)
      use, intrinsic :: ieee_arithmetic, only: ieee_value, &
           & IEEE_POSITIVE_INF, IEEE_NEGATIVE_INF, IEEE_QUIET_NAN
      real(kind=REAL32), intent(out) :: r32
      class(FloatNode), intent(in) :: this

      if (abs(this%value) <= huge(1._REAL32)) then
         r32 = this%value
      elseif (this%value > huge(1._REAL32)) then
         r32 = ieee_value(r32,  IEEE_POSITIVE_INF)
      elseif (this%value < -huge(1._REAL32)) then
         r32 = ieee_value(r32,  IEEE_NEGATIVE_INF)
      else ! must be IEEE 64bit NaN
         r32 = ieee_value(r32,  IEEE_QUIET_NAN)
      end if

   end subroutine assign_to_real32
      

   subroutine assign_to_real64(r64, this)
      real(kind=REAL64), intent(out) :: r64
      class(FloatNode), intent(in) :: this

      r64 = this%value

   end subroutine assign_to_real64


   function to_float(this, unusable, err_msg, rc) result(ptr)
      real(kind=REAL64), pointer :: ptr
      class(AbstractNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      select type (this)
      type is (FloatNode)
         ptr => this%value
      class default
         ptr => DEFAULT_REAL64
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function to_float

end module fy_FloatNode
