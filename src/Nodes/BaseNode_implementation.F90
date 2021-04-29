#include "error_handling.h"
#include "string_handling.h"
submodule (fy_BaseNode) BaseNode_implementation
   use fy_KeywordEnforcer
   use fy_SequenceNode
   use fy_MappingNode
   use fy_IntNode
   use fy_StringNode
   use fy_FloatNode
   use fy_BoolNode
   use fy_NodeVector
   use fy_NodeNodeOrderedMap
   use gFTL2_UnlimitedVector

   use fy_String
   use fy_ErrorCodes
   use fy_ErrorHandling
   implicit none


contains

#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s2, s3, s4, s5, s6, s7, s8, s9
   module function at_multi_selector(this, SELECTORS, unusable, is_present, err_msg, rc) result(ptr)
      class(AbstractNode), pointer :: ptr
      class(BaseNode), target, intent(in) :: this
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_present
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc


      class(AbstractNode), pointer :: config, next_config
      class(*), pointer :: selector
      
      integer :: status
      type(UnlimitedVector), target :: v
      type(UnlimitedVectorIterator) :: iter

      __UNUSED_DUMMY__(unusable)
      
      call selectors_to_vector(v)

      config => this
      rc = YAFYAML_SUCCESS ! unless
      
      associate(b => v%begin(), e => v%end())
        iter = b 
        do while (iter /= e)

           select type (config)
           type is (SequenceNode)
              call get_sequence_item(to_sequence(config), iter%of())
              __VERIFY2__(rc)
           type is (MappingNode)
              call get_mapping_item(to_mapping(config), iter%of())
              __VERIFY2__(rc)
           class default
              __FAIL2__(YAFYAML_NOT_A_COLLECTION)
           end select

           config => next_config

           call iter%next()

        end do

      end associate

      ptr => config

      __RETURN__(YAFYAML_SUCCESS)

   contains

      subroutine selectors_to_vector(v)
         type (UnlimitedVector), intent(out) :: v

         
         call save_one(v, s1) ! 1st selector is mandatory
         if (present(s2)) call save_one(v, s2)
         if (present(s3)) call save_one(v, s3)
         if (present(s4)) call save_one(v, s4)
         if (present(s5)) call save_one(v, s5)
         if (present(s6)) call save_one(v, s6)
         if (present(s7)) call save_one(v, s7)
         if (present(s8)) call save_one(v, s8)
         if (present(s9)) call save_one(v, s9)
      end subroutine selectors_to_vector

      subroutine save_one(v, arg)
         type(UnlimitedVector), intent(inout) :: v
         class(*), intent(in) :: arg
         select type (arg)
         type is (character(*))
            call v%push_back(String(arg))
         class default
            call v%push_back(arg)
         end select
      end subroutine save_one
    

      ! selector for sequence must be some kind of integer
      subroutine get_sequence_item(sequence, selector)
         type(NodeVector), target, intent(in) :: sequence
         class(*), intent(in) :: selector

         select type (i => selector)
         type is (integer(kind=INT32))
            if (i < 0 .or. i > sequence%size()) then
               __FAIL2__(YAFYAML_SEQUENCE_INDEX_OUT_OF_BOUNDS)
            else
               next_config => sequence%of(i)
            end if
         type is (integer(kind=INT64))
            if (i < 0 .or. i > sequence%size()) then
               __FAIL2__(YAFYAML_SEQUENCE_INDEX_OUT_OF_BOUNDS)
               next_config => sequence%of(i)
            end if
         class default
            __FAIL2__(YAFYAML_INVALID_SEQUENCE_INDEX)
         end select
      end subroutine get_sequence_item
      
      ! While a mapping may have keys that are any subclass of AbstractNode.
      subroutine get_mapping_item(mapping, selector)
         type(NodeNodeOrderedMap), target, intent(in) :: mapping
         class(*), intent(in) :: selector

         class(AbstractNode), allocatable :: node

         select type (s => selector)
         type is (logical)
            allocate(node, source=BoolNode(s))
         type is (integer(kind=INT32))
            allocate(node, source=IntNode(s))
         type is (integer(kind=INT64))
            allocate(node, source=IntNode(s))
         type is (real(kind=REAL32))
            allocate(node, source=FloatNode(s))
         type is (real(kind=REAL64))
            allocate(node, source=FloatNode(s))
         type is (character(*))
            allocate(node, source=StringNode(s))
         type is (String)
            allocate(node, source=StringNode(s%s))
         class default
            __FAIL2__(YAFYAML_INVALID_MAPPING_KEY)
         end select
         
         if (mapping%count(node) == 0) then
            __FAIL2__(YAFYAML_MAPPING_KEY_NOT_FOUND)
         end if
         next_config => mapping%at(node,rc=status)

            
      end subroutine get_mapping_item

   end function at_multi_selector


   module subroutine get_logical(this, value, SELECTORS, unusable, is_present, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      logical, intent(out) :: value
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_present
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      ptr => this%at(SELECTORS, is_present=is_present, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'is_present' is used.   Code returns
      ! and value remains undefined.
      if (present(is_present)) then
         if (.not. is_present) return
      else
         value = to_bool(ptr, err_msg=err_msg, __RC__)
      end if

      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical


   module subroutine get_string(this, value, SELECTORS, unusable, is_present, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      character(:), allocatable, intent(out) :: value
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_present
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      ptr => this%at(SELECTORS, is_present=is_present, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'is_present' is used.   Code returns
      ! and value remains undefined.
      if (present(is_present)) then
         if (.not. is_present) return
      else
         value = to_string(ptr, err_msg=err_msg, __RC__)
      end if

      __UNUSED_DUMMY__(unusable)
   end subroutine get_string


   module subroutine get_integer32(this, value, SELECTORS, unusable, is_present, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT32), intent(out) :: value
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_present
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      ptr => this%at(SELECTORS, is_present=is_present, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'is_present' is used.   Code returns
      ! and value remains undefined.
      if (present(is_present)) then
         if (.not. is_present) return
      else
         value = to_int(ptr, err_msg=err_msg, __RC__)
      end if

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32


   module subroutine get_integer64(this, value, SELECTORS, unusable, is_present, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT64), intent(out) :: value
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_present
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      ptr => this%at(SELECTORS, is_present=is_present, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'is_present' is used.   Code returns
      ! and value remains undefined.
      if (present(is_present)) then
         if (.not. is_present) return
      else
         value = to_int(ptr, err_msg=err_msg, __RC__)
      end if

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64


   module subroutine get_real32(this, value, SELECTORS, unusable, is_present, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL32), intent(out) :: value
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_present
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      ptr => this%at(SELECTORS, is_present=is_present, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'is_present' is used.   Code returns
      ! and value remains undefined.
      if (present(is_present)) then
         if (.not. is_present) return
      else
         value = to_float(ptr, err_msg=err_msg, __RC__)
      end if

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32


   module subroutine get_real64(this, value, SELECTORS, unusable, is_present, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL64), intent(out) :: value
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: is_present
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      ptr => this%at(SELECTORS, is_present=is_present, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'is_present' is used.   Code returns
      ! and value remains undefined.
      if (present(is_present)) then
         if (.not. is_present) return
      else
         value = to_float(ptr, err_msg=err_msg, __RC__)
      end if

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64

end submodule BaseNode_implementation
