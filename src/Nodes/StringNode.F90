#include "error_handling.h"
#include "string_handling.h"

module fy_StringNode
   use fy_YAML_Node
   use fy_BaseNode
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   use fy_NullIterator
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: StringNode
   public :: to_string

   type, extends(BaseNode) :: StringNode
      private
      character(:), allocatable :: value
   contains
      procedure, nopass :: is_string
      procedure, nopass :: is_scalar
      procedure :: less_than
      procedure :: write_node_formatted

      procedure :: clear

      procedure :: begin
      procedure :: end

      procedure :: verify => verify_string
      procedure :: clone

   end type StringNode

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(StringNode), intent(in) :: a
         class(YAML_Node), intent(in) :: b
      end function less_than
   end interface

   interface StringNode
      module procedure new_StringNode
   end interface StringNode

contains


   pure logical function is_string() result(is)
      is = .true.
   end function is_string

   pure logical function is_scalar() result(is)
      is = .true.
   end function is_scalar

   function new_StringNode(str) result(node)
      type(StringNode) :: node
      character(*), intent(in) :: str
      node%value = str
!!$      node%id = -1
   end function new_StringNode


      
   function to_string(this, unusable, err_msg, rc) result(ptr)
      character(:), pointer :: ptr
      class(YAML_Node), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      select type (this)
      type is (StringNode)
         ptr => this%value
      class default
         ptr => null()
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function to_string

   subroutine write_node_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(StringNode), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
      
      write(unit,'(a1,a,a1)',iostat=iostat) "'",this%value,"'"
      
   end subroutine write_node_formatted

   subroutine clear(this)
      class(StringNode), intent(inout) :: this
      if (allocated(this%value)) deallocate(this%value)
   end subroutine clear

   function begin(this, unusable, rc) result(iter)
      class(NodeIterator), allocatable :: iter
      class(StringNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      iter = NullIterator()
      
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function begin

   function end(this, unusable, rc) result(iter)
      class(NodeIterator), allocatable :: iter
      class(StringNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      iter = NullIterator()
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function end
   
   logical function verify_string(this) result(verify)
      class(StringNode), target, intent(in) :: this
      verify = .true.
   end function verify_string

   subroutine clone(to, from, unusable, rc)
      class(StringNode), intent(out) :: to
      class(YAML_Node), intent(in)  :: from
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      select type(from)
      type is (StringNode)
         to%value = from%value
      class default
         __FAIL__(YAFYAML_TYPE_MISMATCH)
      end select
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

   end subroutine clone

end module fy_StringNode
