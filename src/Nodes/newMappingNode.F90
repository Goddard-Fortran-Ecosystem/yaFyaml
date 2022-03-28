#include "error_handling.h"
#include "string_handling.h"
module fy_newMappingNode
   use fy_AbstractNode
   use fy_BaseNode
   use fy_newMapping
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: newMappingNode
   public :: to_newmapping
   public :: clone

   type, extends(BaseNode) :: newMappingNode
      ! TODO undo private debugging
      private
      type(newMapping) :: value
   contains
      procedure :: size
      procedure, nopass :: is_mapping
      procedure, pass(this) :: assign_to_mapping
      procedure :: less_than
      procedure :: write_node_formatted
!!$      final :: clear

      procedure :: clear
   end type newMappingNode


   type(newMappingNode) :: mmm

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(newMappingNode), intent(in) :: a
         class(AbstractNode), intent(in) :: b
      end function less_than
   end interface


   type(newMapping), target :: DEFAULT_MAPPING

   interface newMappingNode
      module procedure new_newMappingNode_empty
      module procedure new_newMappingNode
   end interface newMappingNode


   ! Workaround for compilers that break during deep copies
                                                                                                                      
   interface clone
      module procedure clone_mapping_node
      module procedure clone_mapping
   end interface clone

   interface
      recursive module subroutine clone_mapping_node(from, to)
         type(newMappingNode), target, intent(in) :: from
         class(AbstractNode), target, intent(out) :: to
      end subroutine clone_mapping_node
      recursive module subroutine clone_mapping(from, to)
         type(newMapping), target, intent(in) :: from
         type(newMapping), target, intent(out) :: to
      end subroutine clone_mapping
   end interface


contains

   pure logical function is_mapping() result(is)
      is = .true.
   end function is_mapping

   function new_newMappingNode_empty() result(node)
      type(newMappingNode) :: node

      node%value = newMapping()

   end function new_newMappingNode_empty

   function new_newMappingNode(m) result(node)
      type(newMapping), intent(in) :: m
      type(newMappingNode) :: node

      ! Direct assignment is legal, but some compilers show signs of
      ! corrupting memory on deep, nested semi-recursive data
      ! structures.
      call clone(m, node%value)

   end function new_newMappingNode

   subroutine assign_to_mapping(m, this)
      type(newMapping), intent(out) :: m
      class(newMappingNode), intent(in) :: this
      
      m = this%value
      
   end subroutine assign_to_mapping
   

   function to_newmapping(this, unusable, err_msg, rc) result(ptr)
      type(newMapping), pointer :: ptr
      class(AbstractNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      select type (this)
      type is (newMappingNode)
         ptr => this%value
      class default
         ptr => DEFAULT_MAPPING
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function to_newmapping
   

   recursive subroutine write_node_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(newMappingNode), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      type(newMappingIterator) :: iter
      integer :: depth
      character(32) :: fmt
      class(AbstractNode), pointer :: key, value

      iostat = 0
      write(unit,'("{")', iostat=iostat)
      if (iostat /= 0) return
      
      associate (m => this%value)
        iter = m%begin()
        do while (iter /= m%end())

           key => iter%first()
           call key%write_node_formatted(unit, iotype, v_list, iostat, iomsg)
           if (iostat /= 0) return
           write(unit,'(": ")', iostat=iostat)
           if (iostat /= 0) return

           value => iter%second()
           call value%write_node_formatted(unit, iotype, v_list, iostat, iomsg)
           if (iostat /= 0) return

           call iter%next()
           if (iter /= m%end()) then
              write(unit,'(", ")', iostat=iostat)
              if (iostat /= 0) return
           end if
        end do
      end associate
      write(unit,'(" }")', iostat=iostat)
      if (iostat /= 0) return
   end subroutine write_node_formatted


   integer function size(this)
      class(newMappingNode), intent(in) :: this
      size = this%value%size()
   end function size


!!$   recursive subroutine clear(this)
!!$      type(newMappingNode), intent(inout) :: this
!!$      call this%value%clear()
!!$   end subroutine clear

   
   recursive subroutine clear(this)
      class(newMappingNode), intent(inout) :: this

      type(newMappingIterator) :: iter, t_iter
      class(AbstractNode), pointer :: key, value

      call this%value%clear()

   end subroutine clear


end module fy_newMappingNode
