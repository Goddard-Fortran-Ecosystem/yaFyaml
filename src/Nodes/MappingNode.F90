#include "error_handling.h"
#include "string_handling.h"
module fy_MappingNode
   use fy_YAML_Node
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
   public :: clone_mapping
   public :: MappingIterator
   public :: MappingNodeIterator

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
#ifdef __GFORTRAN__
      final :: clear_final
#endif

      procedure :: clear

      procedure :: begin => begin_mapping
      procedure :: end   => end_mapping

      procedure :: verify => verify_mapping
      procedure :: clone

   end type MappingNode

   type, extends(NodeIterator) :: MappingNodeIterator
      private
      type(MappingIterator) :: map_iter
   contains
      procedure :: is_valid => true_
      procedure :: is_sequence_iterator => false_
      procedure :: is_mapping_iterator => true_

      procedure :: next => next_mapping

      procedure :: equal_to
      procedure :: not_equal_to
      
      procedure :: at
      procedure :: first
      procedure :: second

   end type MappingNodeIterator

   interface
      recursive module function less_than(a,b)
         implicit none
         logical :: less_than
         class(MappingNode), intent(in) :: a
         class(YAML_Node), intent(in) :: b
      end function less_than
   end interface


   type(Mapping), target :: DEFAULT_MAPPING

   interface MappingNode
      module procedure new_MappingNode_empty
      module procedure new_MappingNode
   end interface MappingNode


   ! Workaround for compilers that break during deep copies
                                                                                                                      
   interface
      ! Node methods
      recursive module subroutine clone_mapping(to, from, rc)
         type(Mapping), target, intent(out) :: to
         type(Mapping), target, intent(in) :: from
         integer, optional, intent(out) :: rc

      end subroutine clone_mapping

      ! Iterator methods
      module function first(this, unusable, err_msg, rc) result(ptr)
         class(YAML_Node), pointer :: ptr
         class(MappingNodeIterator), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function first

      module function second(this, unusable, err_msg, rc) result(ptr)
         class(YAML_Node), pointer :: ptr
         class(MappingNodeIterator), intent(in) :: this
         class(KeywordEnforcer), optional, intent(in) :: unusable
         STRING_DUMMY, optional, intent(inout) :: err_msg
         integer, optional, intent(out) :: rc
      end function second
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
      call clone_mapping(to=node%value, from=m)

   end function new_MappingNode

   subroutine assign_to_mapping(m, this)
      type(Mapping), intent(out) :: m
      class(MappingNode), intent(in) :: this
      
      m = this%value
      
   end subroutine assign_to_mapping
   

   function to_mapping(this, unusable, err_msg, rc) result(ptr)
      type(Mapping), pointer :: ptr
      class(YAML_Node), target, intent(in) :: this
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
      integer, save :: depth = 0
      character(32) :: fmt
      class(YAML_Node), pointer :: key, value
      integer :: counter

      depth = depth + 1
      iostat = 0
      write(unit,'("{")', iostat=iostat)
      if (iostat /= 0) return
      
      associate (m => this%value)
        associate ( b => m%begin(), e => m%end() )
          iter = b
          counter = 0
          do while (iter /= e)
             counter = counter + 1
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
      end associate
      write(unit,'(" }")', iostat=iostat)
      if (iostat /= 0) return
      depth = depth - 1
   end subroutine write_node_formatted


   integer function size(this)
      class(MappingNode), intent(in) :: this
      size = this%value%size()
   end function size

   
   recursive subroutine clear_final(this)
      type(MappingNode), intent(inout) :: this
      call this%clear()
   end subroutine clear_final

   recursive subroutine clear(this)
      class(MappingNode), intent(inout) :: this

      call this%value%clear()

   end subroutine clear


   function begin_mapping(this, unusable, rc) result(iter)
      class(NodeIterator), allocatable :: iter
      class(MappingNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      iter = MappingNodeIterator(this%value%begin())
      
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function begin_mapping

   function end_mapping(this, unusable, rc) result(iter)
      class(NodeIterator), allocatable :: iter
      class(MappingNode), target, intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      iter = MappingNodeIterator(this%value%end())

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function end_mapping
   
   logical function false_(this)
      class(MappingNodeIterator), intent(in) :: this
      false_ = .false.
      __UNUSED_DUMMY__(this)
   end function false_

   logical function true_(this)
      class(MappingNodeIterator), intent(in) :: this
      true_ = .true.
      __UNUSED_DUMMY__(this)
   end function true_

   subroutine next_mapping(this)
      class(MappingNodeIterator), intent(inout) :: this

      call this%map_iter%next()
   end subroutine next_mapping

   function at(this, unusable, err_msg, rc) result(ptr)
      class(YAML_Node), pointer :: ptr
      class(MappingNodeIterator), intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: node_ptr

      ptr => null()
      __FAIL__(YAFYAML_INVALID_ITERATOR)

      __UNUSED_DUMMY__(this)
      __UNUSED_DUMMY__(unusable)
   end function at

   logical function equal_to(a,b)
      class(MappingNodeIterator), intent(in) :: a
      class(NodeIterator), intent(in) :: b

      select type (b)
      type is (MappingNodeIterator)
         equal_to = a%map_iter == b%map_iter
      class default
         equal_to = .false.
      end select
   end function equal_to

   logical function not_equal_to(a,b)
      class(MappingNodeIterator), intent(in) :: a
      class(NodeIterator), intent(in) :: b
      not_equal_to = .not. (a == b)
   end function not_equal_to

   logical function verify_mapping(this) result(verify)
      class(MappingNode), target, intent(in) :: this
      verify = this%value%verify()
   end function verify_mapping

   ! Node methods
   recursive subroutine clone(to, from, unusable, rc)
      class(MappingNode), intent(inout) :: to
      class(YAML_Node), intent(in) :: from
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      select type (from)
      type is (MappingNode)
         call clone_mapping(from=from%value, to=to%value)
      class default
         __FAIL__(YAFYAML_TYPE_MISMATCH)
      end select
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine clone

end module fy_MappingNode
