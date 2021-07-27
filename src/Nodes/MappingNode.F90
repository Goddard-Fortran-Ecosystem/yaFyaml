#include "error_handling.h"
#include "string_handling.h"
module fy_MappingNode
   use fy_AbstractNode
   use fy_BaseNode
   use fy_Mapping
   use fy_ErrorCodes
   use fy_ErrorHandling
   use fy_keywordEnforcer
   implicit none
   private

   public :: MappingNode
   public :: to_mapping

   type, extends(BaseNode) :: MappingNode
!!$      private
      type(Mapping) :: value
   contains
      procedure, nopass :: is_mapping
      procedure, pass(this) :: assign_to_mapping
      procedure :: less_than
      procedure :: analysis
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
      module procedure new_MappingNode
   end interface MappingNode

contains

   pure logical function is_mapping() result(is)
      is = .true.
   end function is_mapping

   function new_MappingNode() result(node)
      type(MappingNode) :: node

      node%value = Mapping()

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
   

   function analysis(this, prefix)result(str)
      character(:), allocatable :: str
      class(MappingNode), target, intent(in) :: this
      character(*), intent(in) :: prefix

      integer :: i
      character(100) :: buffer
      type(MappingIterator) :: iter
      class(AbstractNode), pointer :: p_node
      type(Mapping), pointer :: m

      write(buffer,'(i0)') this%value%size()
      str = new_line('a') // prefix // "MappingNode: size " // trim(buffer)
      m => to_mapping(this)
      iter = m%begin()
      do i = 1, this%value%size()
         str = str // new_line('a')
         p_node => iter%first()
         str = str // p_node%analysis(prefix // '       key:     ') // new_line('a')
         p_node => iter%second()
         str = str // p_node%analysis(prefix // '       value:   ') // new_line('a')
         call iter%next()
      end do
   end function analysis

end module fy_MappingNode