#include "error_handling.h"
#include "string_handling.h"
module fy_IntNode
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

   public :: IntNode
   public :: to_int

   type, extends(BaseNode) :: IntNode
      private
      integer(kind=INT64) :: value = -huge(1_INT64)
   contains
      procedure, nopass :: is_int
      procedure, nopass :: is_scalar
      procedure :: less_than
      procedure :: write_node_formatted

      procedure :: clear

      procedure :: begin
      procedure :: end
   end type IntNode

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(IntNode), intent(in) :: a
         class(YAML_Node), intent(in) :: b
      end function less_than
   end interface

   interface IntNode
      module procedure new_IntNode_i32
      module procedure new_IntNode_i64
   end interface IntNode

contains

   pure logical function is_int() result(is)
      is = .true.
   end function is_int

   pure logical function is_scalar() result(is)
      is = .true.
   end function is_scalar

   function new_IntNode_i32(i32) result(node)
      type(IntNode) :: node
      integer(kind=INT32), intent(in) :: i32
      node%value = i32
   end function new_IntNode_i32

   function new_IntNode_i64(i64) result(node)
      type(IntNode) :: node
      integer(kind=INT64), intent(in) :: i64
      node%value = i64
   end function new_IntNode_i64


   function to_int(this, unusable, err_msg, rc) result(ptr)
      integer(kind=INT64), pointer :: ptr
      class(YAML_Node), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      select type (this)
      type is (IntNode)
         ptr => this%value
      class default
         ptr => null()
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function to_int

   subroutine write_node_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(IntNode), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
      
      write(unit,'(i0)',iostat=iostat) this%value
      
   end subroutine write_node_formatted

   subroutine clear(this)
      class(IntNode), intent(inout) :: this
      __UNUSED_DUMMY__(this)
   end subroutine clear

   function begin(this, unusable, rc) result(iter)
      class(NodeIterator), allocatable :: iter
      class(IntNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      iter = NullIterator()
      
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function begin

   function end(this, unusable, rc) result(iter)
      class(NodeIterator), allocatable :: iter
      class(IntNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      iter = NullIterator()
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function end
   
end module fy_IntNode
