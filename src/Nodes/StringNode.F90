#include "error_handling.h"
#include "string_handling.h"

module fy_StringNode
   use fy_AbstractNode
   use fy_BaseNode
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   implicit none
   private

   public :: StringNode
   public :: to_string

   type, extends(BaseNode) :: StringNode
      private
      character(:), allocatable :: value
   contains
      procedure, nopass :: is_string
      procedure, pass(this) :: assign_to_string
      procedure :: less_than
   end type StringNode

   character(len=0), target :: DEFAULT_STRING = ''

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(StringNode), intent(in) :: a
         class(AbstractNode), intent(in) :: b
      end function less_than
   end interface

   interface StringNode
      module procedure new_StringNode
   end interface StringNode

contains


   pure logical function is_string() result(is)
      is = .true.
   end function is_string


   function new_StringNode(str) result(node)
      type(StringNode) :: node
      character(*), intent(in) :: str
      node%value = str
   end function new_StringNode


   subroutine assign_to_string(string, this)
      character(:), allocatable, intent(out) :: string
      class(StringNode), intent(in) :: this

      string = this%value

   end subroutine assign_to_string
      
   function to_string(this, unusable, err_msg, rc) result(ptr)
      character(:), pointer :: ptr
      class(AbstractNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      select type (this)
      type is (StringNode)
         ptr => this%value
      class default
         ptr => DEFAULT_STRING
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function to_string

end module fy_StringNode
