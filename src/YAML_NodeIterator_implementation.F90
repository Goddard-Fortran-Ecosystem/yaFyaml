#include "error_handling.h"
#include "string_handling.h"
#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s2, s3, s4, s5, s6, s7, s8, s9

submodule (fy_YAML_Node)  YAML_NodeIterator_implementation
   use, intrinsic :: iso_fortran_env, only: INT32, INT64, REAL32, REAL64
   implicit none

contains
   
   module subroutine next_iter(this)
      class(YAML_NodeIterator), intent(inout) :: this
      call this%mapping_iterator%next()
   end subroutine next_iter

   module subroutine get_key_logical(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      logical, intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node

      node => this%mapping_iterator%first()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_logical


   module subroutine get_key_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      logical, allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_logical_1d
   

   module subroutine get_key_string(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      character(:), allocatable, intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_string


   module subroutine get_key_integer32(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      integer(kind=INT32), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_integer32

   module subroutine get_key_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      integer(kind=INT32), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_integer32_1d

   module subroutine get_key_integer64(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      integer(kind=INT64), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_integer64

   module subroutine get_key_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      integer(kind=INT64), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_integer64_1d


   module subroutine get_key_real32(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      real(kind=REAL32), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_real32

   module subroutine get_key_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      real(kind=REAL32), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_real32_1d

   module subroutine get_key_real64(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      real(kind=REAL64), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_real64

   module subroutine get_key_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      real(kind=REAL64), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%first()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_real64_1d

   module subroutine get_key_subconfig(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      type(YAML_Node), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node, subnode
      integer :: status

      ! To be consistent with other getters, we only update value _if_
      ! accessor is successful.
      node => this%mapping_iterator%first()
      subnode => node%at(SELECTORS, err_msg=err_msg, rc=rc)
      if (status == YAFYAML_SUCCESS) call value%initialize(subnode)
      if (present(rc)) rc = status

      __UNUSED_DUMMY__(unusable)
   end subroutine get_key_subconfig

   module subroutine get_value_logical(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      logical, intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node

      node => this%mapping_iterator%second()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_logical


   module subroutine get_value_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      logical, allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_logical_1d
   

   module subroutine get_value_string(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      character(:), allocatable, intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_string


   module subroutine get_value_integer32(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      integer(kind=INT32), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_integer32

   module subroutine get_value_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      integer(kind=INT32), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_integer32_1d

   module subroutine get_value_integer64(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      integer(kind=INT64), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_integer64

   module subroutine get_value_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      integer(kind=INT64), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_integer64_1d


   module subroutine get_value_real32(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      real(kind=REAL32), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_real32

   module subroutine get_value_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      real(kind=REAL32), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_real32_1d

   module subroutine get_value_real64(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      real(kind=REAL64), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_real64

   module subroutine get_value_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      real(kind=REAL64), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node
      
      node => this%mapping_iterator%second()
      call node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_real64_1d

   module subroutine get_value_subconfig(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(YAML_NodeIterator), target, intent(in) :: this
      type(YAML_Node), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: node, subnode
      integer :: status

      ! To be consistent with other getters, we only update value _if_
      ! accessor is successful.
      node => this%mapping_iterator%second()
      subnode => node%at(SELECTORS, err_msg=err_msg, rc=status)
      if (status == YAFYAML_SUCCESS) call value%initialize(subnode)
      if (present(rc)) rc = status

      __UNUSED_DUMMY__(unusable)
   end subroutine get_value_subconfig


   module function equal_iter(a, b) result(equal)
      logical :: equal
      type(YAML_NodeIterator), intent(in) :: a
      type(YAML_NodeIterator), intent(in) :: b

      equal = a%mapping_iterator == b%mapping_iterator
   end function equal_iter

   module function not_equal_iter(a, b) result(not_equal)
      logical :: not_equal
      type(YAML_NodeIterator), intent(in) :: a
      type(YAML_NodeIterator), intent(in) :: b

      not_equal = a%mapping_iterator /= b%mapping_iterator
   end function not_equal_iter

   
end submodule YAML_NodeIterator_implementation
