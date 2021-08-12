#include "error_handling.h"
#include "string_handling.h"
module fy_BoolNode
   use fy_AbstractNode
   use fy_BaseNode
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   implicit none
   private

   public :: BoolNode
   public :: to_bool

   type, extends(BaseNode) :: BoolNode
      private
      logical :: value = .false.
   contains
      procedure, nopass :: is_bool
      procedure, pass(this) :: assign_to_logical
      procedure :: less_than
   end type BoolNode

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(BoolNode), intent(in) :: a
         class(AbstractNode), intent(in) :: b
      end function less_than
   end interface

   interface BoolNode
      module procedure new_BoolNode
   end interface BoolNode


   
contains


   pure logical function is_bool() result(is)
      is = .true.
   end function is_bool


   function new_BoolNode(flag) result(node)
      type(BoolNode) :: node
      logical, intent(in) :: flag
      node%value = flag
   end function new_BoolNode

   subroutine assign_to_logical(flag, this)
      logical, intent(out) :: flag
      class(BoolNode), intent(in) :: this

      flag = this%value
   end subroutine assign_to_logical
      


   function to_bool(this, unusable, err_msg, rc) result(ptr)
      logical, pointer :: ptr
      class(AbstractNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      select type (this)
      type is (BoolNode)
         ptr => this%value
      class default
         ptr => DEFAULT_LOGICAL
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function to_bool

end module fy_BoolNode
