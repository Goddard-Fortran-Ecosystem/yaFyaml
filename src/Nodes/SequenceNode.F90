#include "error_handling.h"
#include "string_handling.h"
module fy_SequenceNode
   use fy_YAML_Node
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
   public :: clone_sequence
   public :: SequenceNodeIterator
   public :: operator(==)
   public :: operator(/=)

   type, extends(BaseNode) :: SequenceNode
      private
      type(sequence) :: value
   contains
      procedure :: size
      procedure, pass(this) :: assign_to_sequence
      procedure, nopass :: is_sequence
      procedure :: less_than
      procedure :: write_node_formatted

#ifdef __GFORTRAN__
      final :: clear_final
#endif
      procedure :: clear

      procedure :: begin => begin_sequence
      procedure :: end   => end_sequence

      procedure :: verify => verify_sequence
      procedure :: clone
   end type SequenceNode


   type, extends(NodeIterator) :: SequenceNodeIterator
      private
      type(SequenceIterator) :: seq_iter
   contains
      procedure :: is_valid => true
      procedure :: is_sequence_iterator => true
      procedure :: is_mapping_iterator => false

      procedure :: next => next_sequence
      procedure :: equal_to
      procedure :: not_equal_to

      procedure :: at
      procedure :: first
      procedure :: second => first

   end type SequenceNodeIterator


   type(Sequence), target :: DEFAULT_SEQUENCE

   interface
      recursive module function less_than(a,b)
         implicit none
         logical :: less_than
         class(SequenceNode), intent(in) :: a
         class(YAML_Node), intent(in) :: b
      end function less_than
   end interface

   interface SequenceNode
      module procedure new_SequenceNode_empty
      module procedure new_SequenceNode
   end interface SequenceNode
   
   interface
 
      module function at(this, unusable, err_msg, rc) result(ptr)
         class(YAML_Node), pointer :: ptr
         class(SequenceNodeIterator), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function at

      recursive module subroutine clone_sequence(to, from)
         type(sequence), target, intent(out) :: to
         type(sequence), target, intent(in) :: from
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

      call clone_sequence(to=node%value, from=s)
   end function new_SequenceNode

   function to_sequence(this, unusable, err_msg, rc) result(ptr)
      type(Sequence), pointer :: ptr
      class(YAML_Node), target, intent(in) :: this
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
      class(YAML_Node), pointer :: element
      
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


   recursive subroutine clear_final(this)
      type(SequenceNode), intent(inout) :: this
      call this%clear()
   end subroutine clear_final

   recursive subroutine clear(this)
      class(SequenceNode), intent(inout) :: this

      type(SequenceIterator) :: iter
      class(YAML_Node), pointer :: item
      integer, save :: depth = 0

      depth = depth + 1
      call this%value%clear()

        depth = depth - 1

   end subroutine clear

   function begin_sequence(this, unusable, rc) result(iter)
      class(NodeIterator), allocatable :: iter
      class(SequenceNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      iter = SequenceNodeIterator(this%value%begin())
      
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function begin_sequence

   function end_sequence(this, unusable, rc) result(iter)
      class(NodeIterator), allocatable :: iter
      class(SequenceNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      iter = SequenceNodeIterator(this%value%end())
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function end_sequence

   subroutine next_sequence(this)
      class(SequenceNodeIterator), intent(inout) :: this
      call this%seq_iter%next()
   end subroutine next_sequence



   logical function false(this)
      class(SequenceNodeIterator), intent(in) :: this
      false = .false.
      __UNUSED_DUMMY__(this)
   end function false

   logical function true(this)
      class(SequenceNodeIterator), intent(in) :: this
      true = .true.
      __UNUSED_DUMMY__(this)
   end function true

   logical function equal_to(a,b)
      class(SequenceNodeIterator), intent(in) :: a
      class(NodeIterator), intent(in) :: b

      select type (b)
      type is (SequenceNodeIterator)
         equal_to = a%seq_iter == b%seq_iter
      class default
         equal_to = .false.
      end select
   end function equal_to

   logical function not_equal_to(a,b)
      class(SequenceNodeIterator), intent(in) :: a
      class(NodeIterator), intent(in) :: b

      not_equal_to = .not. (a == b)
   end function not_equal_to
   
   function first(this, unusable, err_msg, rc) result(ptr)
      class(YAML_Node), pointer :: ptr
      class(SequenceNodeIterator), intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      ptr => null()
      __FAIL__(YAFYAML_INVALID_ITERATOR)

      __UNUSED_DUMMY__(this)
      __UNUSED_DUMMY__(unusable)
   end function first

   logical function verify_sequence(this) result(verify)
      class(SequenceNode), target, intent(in) :: this
      verify = .true.
   end function verify_sequence


   ! Node methods
   recursive subroutine clone(to, from, unusable, rc)
      class(SequenceNode), intent(out) :: to
      class(YAML_Node), intent(in) :: from
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer, save :: depth = 0

      depth = depth + 1
      select type (from)
      type is (SequenceNode)
         call clone_sequence(from=from%value, to=to%value)
      class default
         __FAIL__(YAFYAML_NONSPECIFIC_ERROR)
      end select

      depth = depth - 1
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine clone

end module fy_SequenceNode

