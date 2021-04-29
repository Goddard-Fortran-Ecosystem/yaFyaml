#include "error_handling.h"
#include "string_handling.h"
module fy_SequenceNode
   use fy_AbstractNode
   use fy_BaseNode
   use fy_NodeVector
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   implicit none
   private

   public :: SequenceNode
   public :: to_sequence
   
   type, extends(BaseNode) :: SequenceNode
      private
      type(NodeVector) :: value
   contains
      procedure, pass(this) :: assign_to_sequence
      procedure :: less_than
   end type SequenceNode

   type(NodeVector), target :: DEFAULT_SEQUENCE

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(SequenceNode), intent(in) :: a
         class(AbstractNode), intent(in) :: b
      end function less_than
   end interface
contains

   subroutine assign_to_sequence(sequence, this)
      type(NodeVector), intent(out) :: sequence
      class(SequenceNode), intent(in) :: this
      
      sequence = this%value
      
   end subroutine assign_to_sequence


   function to_sequence(this, unusable, err_msg, rc) result(ptr)
      type(NodeVector), pointer :: ptr
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
   

end module fy_SequenceNode
