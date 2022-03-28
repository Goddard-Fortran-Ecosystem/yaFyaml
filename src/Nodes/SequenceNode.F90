#include "error_handling.h"
#include "string_handling.h"
module fy_SequenceNode
   use fy_AbstractNode
   use fy_BaseNode
   use fy_Sequence
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   private

   public :: SequenceNode
   public :: to_sequence
   public :: clone
   
   type, extends(BaseNode) :: SequenceNode
      private
      type(Sequence) :: value
   contains
      procedure :: size
      procedure, pass(this) :: assign_to_sequence
      procedure, nopass :: is_sequence
      procedure :: less_than
      procedure :: write_node_formatted

      procedure :: clear
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
      module procedure new_SequenceNode_empty
      module procedure new_SequenceNode
   end interface SequenceNode
   
   interface clone                                                                                                                    
      module procedure clone_sequence_node                                                                                            
      module procedure clone_sequence                                                                                                 
   end interface clone                                                                                                                

   interface
      recursive module subroutine clone_sequence_node(from, to)
         type(SequenceNode), target, intent(in) :: from
         class(AbstractNode), target, intent(out) :: to
      end subroutine clone_sequence_node
      recursive module subroutine clone_sequence(from, to)
         type(Sequence), target, intent(in) :: from
         type(Sequence), target, intent(out) :: to
      end subroutine clone_sequence
   end interface

contains


   function new_SequenceNode_empty() result(node)
      type(SequenceNode) :: node
      node%value = Sequence()
   end function new_SequenceNode_empty

   function new_SequenceNode(s) result(node)
      type(SequenceNode) :: node
      type(sequence), intent(in) :: s

      call clone(s, node%value)

   end function new_SequenceNode



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

   recursive subroutine write_node_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(SequenceNode), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      integer :: i
      class(AbstractNode), pointer :: element
      
      write(unit,'("[ ")', iostat=iostat)
      if (iostat /= 0) return
        do i = 1, this%value%size()
           element => this%value%of(i)
           call element%write_node_formatted(unit, iotype, v_list, iostat, iomsg)
           if (iostat /= 0) return
           if (i < this%value%size()) then
              write(unit,'(", ")', iostat=iostat)
              if (iostat /= 0) return
           end if
        end do
      write(unit,'(" ]")', iostat=iostat)
      
   end subroutine write_node_formatted


   integer function size(this)
      class(SequenceNode), intent(in) :: this
      size = this%value%size()
   end function size


   subroutine assign_to_sequence(s, this)
      type(Sequence), intent(out) :: s
      class(SequenceNode), intent(in) :: this
      
      s = this%value
      
   end subroutine assign_to_sequence


   recursive subroutine clear(this)
      class(SequenceNode), intent(inout) :: this

      type(SequenceIterator) :: iter
      class(AbstractNode), pointer :: item

        associate (b => this%value%begin(), e=> this%value%end())
          iter = b
          do while (iter /= e)
             item => iter%of()
             call item%clear()
             call iter%next()
          end do
        end associate
        call this%value%clear()

   end subroutine clear

end module fy_SequenceNode

