#include "error_handling.h"
#include "string_handling.h"
module fy_MappingNode
   use fy_AbstractNode
   use fy_BaseNode
   use fy_Mapping
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: MappingNode
   public :: to_mapping
   public :: clone

   type, extends(BaseNode) :: MappingNode
      ! TODO undo private debugging
      private
      type(Mapping) :: value
   contains
      procedure :: size
      procedure, nopass :: is_mapping
      procedure, pass(this) :: assign_to_mapping
      procedure :: less_than
      procedure :: write_node_formatted
      final :: clear_final

      procedure :: clear
   end type MappingNode


   type(MappingNode) :: mmm

   interface
      module function less_than(a,b)
         implicit none
         logical :: less_than
         class(MappingNode), intent(in) :: a
         class(AbstractNode), intent(in) :: b
      end function less_than
   end interface


   type(Mapping), target :: DEFAULT_MAPPING

   interface MappingNode
      module procedure new_MappingNode_empty
      module procedure new_MappingNode
   end interface MappingNode


   ! Workaround for compilers that break during deep copies
                                                                                                                      
   interface clone
      module procedure clone_mapping_node
      module procedure clone_mapping
   end interface clone

   interface
      recursive module subroutine clone_mapping_node(from, to)
         type(MappingNode), target, intent(in) :: from
         class(AbstractNode), target, intent(out) :: to
      end subroutine clone_mapping_node
      recursive module subroutine clone_mapping(from, to)
         type(Mapping), target, intent(in) :: from
         type(Mapping), target, intent(out) :: to
      end subroutine clone_mapping
   end interface


contains

   pure logical function is_mapping() result(is)
      is = .true.
   end function is_mapping

   function new_MappingNode_empty() result(node)
      type(MappingNode) :: node

      node%value = Mapping()

   end function new_MappingNode_empty

   function new_MappingNode(m) result(node)
      type(Mapping), intent(in) :: m
      type(MappingNode) :: node

      ! Direct assignment is legal, but some compilers show signs of
      ! corrupting memory on deep, nested semi-recursive data
      ! structures.
      call clone(m, node%value)

   end function new_MappingNode

   subroutine assign_to_mapping(m, this)
      type(Mapping), intent(out) :: m
      class(MappingNode), intent(in) :: this
      
      m = this%value
      
   end subroutine assign_to_mapping
   

   function to_mapping(this, unusable, err_msg, rc) result(ptr)
      type(Mapping), pointer :: ptr
      class(AbstractNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      select type (this)
      type is (MappingNode)
         ptr => this%value
      class default
         ptr => DEFAULT_MAPPING
         __FAIL2__(YAFYAML_TYPE_MISMATCH)
      end select

      __RETURN__(YAFYAML_SUCCESS)
   end function to_mapping
   

   recursive subroutine write_node_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(MappingNode), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      type(MappingIterator) :: iter
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
      class(MappingNode), intent(in) :: this
      size = this%value%size()
   end function size


!!$   recursive subroutine clear(this)
!!$      type(MappingNode), intent(inout) :: this
!!$      call this%value%clear()
!!$   end subroutine clear

   
   recursive subroutine clear_final(this)
      type(MappingNode), intent(inout) :: this
      call this%clear()
   end subroutine clear_final

   recursive subroutine clear(this)
      class(MappingNode), intent(inout) :: this

      type(MappingIterator) :: iter, t_iter
      class(AbstractNode), pointer :: key, value

      call this%value%clear()

   end subroutine clear


end module fy_MappingNode
