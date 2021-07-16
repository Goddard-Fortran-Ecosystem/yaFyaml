#include "error_handling.h"
#include "string_handling.h"
module fy_SequenceNode
   use fy_AbstractNode
   use fy_BaseNode
   use fy_Sequence
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   implicit none
   private

   public :: SequenceNode
   public :: to_sequence
   
   type, extends(BaseNode) :: SequenceNode
      private
      type(Sequence) :: value
   contains
      procedure, pass(this) :: assign_to_sequence
      procedure, nopass :: is_sequence
      procedure :: less_than
   end type SequenceNode

   type(Sequence), target :: DEFAULT_SEQUENCE

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(SequenceNode), intent(in) :: a
         class(AbstractNode), intent(in) :: b
      end function less_than
   end interface

   interface SequenceNode
      module procedure new_SequenceNode
   end interface SequenceNode

contains


   function new_SequenceNode() result(node)
      type(SequenceNode) :: node
      node%value = Sequence()
   end function new_SequenceNode

   subroutine assign_to_sequence(s, this)
      type(Sequence), intent(out) :: s
      class(SequenceNode), intent(in) :: this
      
      s = this%value
      
   end subroutine assign_to_sequence


   function to_sequence(this, unusable, err_msg, rc) result(ptr)
      type(Sequence), pointer :: ptr
      class(AbstractNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      select type (this)
      type is (SequenceNode)
         ptr => this%value
      class default
         ptr => DEFAULT_SEQUENCE
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function to_sequence

   pure logical function is_sequence() result(is)
      is = .true.
   end function is_sequence

end module fy_SequenceNode
