#include "error_handling.h"
#include "string_handling.h"

module fy_NullIterator
   use fy_AbstractNode, only: AbstractNode
   use fy_AbstractNode, only: NodeIterator
   use fy_KeywordEnforcer
   use fy_ErrorHandling
   use fy_ErrorCodes
   implicit none
   private

   public :: NullIterator

   type, extends(NodeIterator) :: NullIterator
   contains
      procedure :: is_valid => false
      procedure :: is_sequence_iterator => false
      procedure :: is_mapping_iterator => false

      procedure :: next

      procedure :: equal_to
      procedure :: not_equal_to

      procedure :: at
      procedure :: first => at
      procedure :: second => at
      
   end type NullIterator

contains

   logical function false(this)
      class(NullIterator), intent(in) :: this
      false = .false.
      __UNUSED_DUMMY__(this)
   end function false

   subroutine next(this)
      class(NullIterator), intent(inout) :: this
      ! noop
      __UNUSED_DUMMY__(this)
   end subroutine next

   function at(this, unusable, err_msg, rc) result(ptr)
      class(AbstractNode), pointer :: ptr
      class(NullIterator), intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      ptr => null()
      __UNUSED_DUMMY__(this)
      __UNUSED_DUMMY__(unusable)
      __FAIL2__(YAFYAML_INVALID_ITERATOR)
   end function at

   logical function equal_to(a,b)
      class(NullIterator), intent(in) :: a
      class(NodeIterator), intent(in) :: b
      equal_to = .false.
   end function equal_to

   ! Always return false - force loops to terminate
   logical function not_equal_to(a,b)
      class(NullIterator), intent(in) :: a
      class(NodeIterator), intent(in) :: b
      not_equal_to = .false.
   end function not_equal_to

end module fy_NullIterator
