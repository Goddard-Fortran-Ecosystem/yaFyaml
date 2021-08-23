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
   use fy_Sequence
   use fy_Mapping
   use gFTL2_UnlimitedVector

   use fy_String
   use fy_ErrorCodes
   use fy_ErrorHandling
   implicit none

contains

#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9

   module function at_multi_selector(this, SELECTORS, unusable, found, err_msg, rc) result(ptr)
      class(AbstractNode), pointer :: ptr
      class(BaseNode), target, intent(in) :: this
      class(*), optional, intent(in) :: OPT_SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc


      class(AbstractNode), pointer :: config, next_config
      class(*), pointer :: selector
      
      integer :: status
      type(UnlimitedVector), target :: v
      type(UnlimitedVectorIterator) :: iter
      logical :: found_

      call selectors_to_vector(v)

      config => this
      if (present(rc)) rc = YAFYAML_SUCCESS ! unless ...
      if (present(found)) found=.true. ! unless proven otherwise
      
      associate(b => v%begin(), e => v%end())
        iter = b 
        do while (iter /= e)

           found_ = .false. ! unless
           
           select type (config)
           type is (SequenceNode)
              call get_sequence_item(to_sequence(config), iter%of(), found_, rc=status)
              if (status /= YAFYAML_SUCCESS) then
                 if (present(found)) then
                    found = found_
                    __RETURN__(YAFYAML_SUCCESS)
                 else
                    __FAIL2__(status)
                 end if
              endif
           type is (MappingNode)
              call get_mapping_item(to_mapping(config), iter%of(), found_, rc=status)
              if (status /= YAFYAML_SUCCESS) then
                 if (present(found)) then
                    found = found_
                    __RETURN__(YAFYAML_SUCCESS)
                 else
                    __FAIL2__(status)
                 end if
              endif
           class default
                 if (present(found)) then
                    found = found_
                    __RETURN__(YAFYAML_SUCCESS)
                 else
                    __FAIL2__(YAFYAML_NOT_A_COLLECTION)
                 end if

           end select

           config => next_config

           call iter%next()

        end do

      end associate

      ptr => config

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
      
   contains

      subroutine selectors_to_vector(v)
         type (UnlimitedVector), intent(out) :: v

         if (present(s1)) call save_one(v, s1)
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
      subroutine get_sequence_item(s, selector, found, rc)
         type(Sequence), target, intent(in) :: s
         class(*), intent(in) :: selector
         logical, intent(out) :: found
         integer, intent(out) :: rc

         select type (i => selector)
         type is (integer(kind=INT32))
            if (i <= 0 .or. i > s%size()) then
               found = .false.
               next_config => null()
               rc = YAFYAML_SEQUENCE_INDEX_OUT_OF_BOUNDS
               return
            else
               found = .true.
               next_config => s%of(i)
               rc = YAFYAML_SUCCESS
               return
            end if
         type is (integer(kind=INT64))
            if (i <= 0 .or. i > s%size()) then
               found = .false.
               next_config => null()
               rc = YAFYAML_SEQUENCE_INDEX_OUT_OF_BOUNDS
               return
            else
               found = .true.
               next_config => s%of(i)
               rc = YAFYAML_SUCCESS
               return
            end if
         class default
            found = .false.
            next_config => null()
            rc = YAFYAML_INVALID_SEQUENCE_INDEX
         end select
      end subroutine get_sequence_item
      
      ! While a mapping may have keys that are any subclass of AbstractNode.
      subroutine get_mapping_item(m, selector, found, rc)
         type(Mapping), target, intent(in) :: m
         class(*), intent(in) :: selector
         logical, intent(out) :: found
         integer, intent(out) :: rc

         class(AbstractNode), allocatable :: node
         integer :: status
         
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
            found = .false.
            rc = YAFYAML_INVALID_MAPPING_KEY
            next_config => null()
         end select

         if (m%count(node) == 0) then
            found = .false.
            rc = YAFYAML_SELECTOR_NOT_FOUND
            next_config => null()
            return
         end if

         next_config => m%at(node,rc=status)
         if (status == 0) then
            found = .true.
            rc = YAFYAML_SUCCESS
         else  ! should not be possible
            found = .false.
            rc = YAFYAML_SELECTOR_NOT_FOUND
         end if
            
      end subroutine get_mapping_item

   end function at_multi_selector

   ! Error conditions
   ! 1. Selected item is not found _and_ no default value is provided _and_ found is not used
   ! 2. Selected item does exist but is of the wrong type

   module subroutine get_logical(this, value, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      logical, intent(out) :: value
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status
      logical :: found_

      value = DEFAULT_LOGICAL ! ensure that return value is defined

      ptr => this%at(SELECTORS, found=found_, err_msg=err_msg, __RC__)
      if (present(found)) found=found_
         

      if (.not. found_) then
         value = DEFAULT_LOGICAL
         if (.not. present(found)) then
            __FAIL2__(YAFYAML_SELECTOR_NOT_FOUND)
         end if
      else ! found but possible type-mismatch
         value = to_bool(ptr, err_msg=err_msg, __RC__)
         __VERIFY2__(err_msg,status)
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical

   module subroutine get_logical_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      logical, allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status
      logical :: found_
      logical :: tmp
      integer :: i

      allocate(values(0)) ! unsure that return value is defined

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=found_, err_msg=err_msg, __RC__)
      if (present(found)) found=found_

      if (.not. found_) then
         if (.not. present(found)) then
            __FAIL2__(YAFYAML_SELECTOR_NOT_FOUND)
         end if
      else ! found but possible type-mismatch
         __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)
         ! check type of each entry ...
         do i = 1, ptr%size()
            call ptr%get(tmp, i, err_msg=err_msg, rc=status)
            __VERIFY2__(err_msg, status)
         end do

         deallocate(values)
         allocate(values(ptr%size()))
         do i = 1, ptr%size()
            call ptr%get(values(i), i)
         end do

      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical_1d


   module subroutine get_string(this, value, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      character(:), allocatable, intent(out) :: value
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      value = DEFAULT_STRING
      ptr => this%at(SELECTORS, found=found, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'found' is used.   Code returns
      ! and value remains undefined.
      if (present(found)) then
         if (.not. found) return
      else
         value = to_string(ptr, err_msg=err_msg, __RC__)
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_string


   module subroutine get_integer32(this, value, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT32), intent(out) :: value
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status
      integer(kind=INT64) :: value64
      

      value = DEFAULT_INT32
      ptr => this%at(SELECTORS, found=found, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'found' is used.   Code returns
      ! and value remains undefined.
      if (present(found)) then
         if (.not. found) return
      else
         ! Must assign to 64-bit first to protect against overflow
         value64 = to_int(ptr, err_msg=err_msg, __RC__)
         if (-huge(1_INT32) <= value64 .and. value64 <= huge(1_INT64)) then
            value = value64  ! else keep default value
         end if
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32

   module subroutine get_integer32_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT32), allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status
      logical :: found_
      integer(kind=INT32) :: tmp
      integer :: i

      allocate(values(0)) ! unsure that return value is defined

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=found_, err_msg=err_msg, __RC__)
      if (present(found)) found=found_

      if (.not. found_) then
         if (.not. present(found)) then
            __FAIL2__(YAFYAML_SELECTOR_NOT_FOUND)
         end if
      else ! found but possible type-mismatch
         __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)
         ! check type of each entry ...
         do i = 1, ptr%size()
            call ptr%get(tmp, i, err_msg=err_msg, rc=status)
            __VERIFY2__(err_msg, status)
         end do

         deallocate(values)
         allocate(values(ptr%size()))
         do i = 1, ptr%size()
            call ptr%get(values(i), i)
         end do

      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32_1d


   module subroutine get_integer64(this, value, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT64), intent(out) :: value
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      value = DEFAULT_INT64
      ptr => this%at(SELECTORS, found=found, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'found' is used.   Code returns
      ! and value remains undefined.
      if (present(found)) then
         if (.not. found) return
      else
         value = to_int(ptr, err_msg=err_msg, __RC__)
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64


   module subroutine get_integer64_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT64), allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status
      logical :: found_
      integer(kind=INT64) :: tmp
      integer :: i

      allocate(values(0)) ! unsure that return value is defined

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=found_, err_msg=err_msg, __RC__)
      if (present(found)) found=found_

      if (.not. found_) then
         if (.not. present(found)) then
            __FAIL2__(YAFYAML_SELECTOR_NOT_FOUND)
         end if
      else ! found but possible type-mismatch
         __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)
         ! check type of each entry ...
         do i = 1, ptr%size()
            call ptr%get(tmp, i, err_msg=err_msg, rc=status)
            __VERIFY2__(err_msg, status)
         end do

         deallocate(values)
         allocate(values(ptr%size()))
         do i = 1, ptr%size()
            call ptr%get(values(i), i)
         end do

      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64_1d


   module subroutine get_real32(this, value, SELECTORS, unusable, found, err_msg, rc)
      use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL32), intent(out) :: value
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status
      real(kind=REAL64) :: safe_value

      value = DEFAULT_REAL32
      ptr => this%at(SELECTORS, found=found, err_msg=err_msg, __RC__)
      ! Not an error if selector not found when 'found' is used.   Code returns
      ! and value remains undefined.
      if (present(found)) then
         if (.not. found) return
      else
         ! unless it is a float in the acceptable range ...
         safe_value = to_float(ptr, err_msg=err_msg, __RC__)
         ! if we have not returned yet, then conversion
         ! is possible
         value = ptr
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32

   module subroutine get_real32_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL32), allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status
      logical :: found_
      real(kind=REAL32) :: tmp
      integer :: i

      allocate(values(0)) ! unsure that return value is defined

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=found_, err_msg=err_msg, __RC__)
      if (present(found)) found=found_

      if (.not. found_) then
         if (.not. present(found)) then
            __FAIL2__(YAFYAML_SELECTOR_NOT_FOUND)
         end if
      else ! found but possible type-mismatch
         __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)
         ! check type of each entry ...
         do i = 1, ptr%size()
            call ptr%get(tmp, i, err_msg=err_msg, rc=status)
            __VERIFY2__(err_msg, status)
         end do

         deallocate(values)
         allocate(values(ptr%size()))
         do i = 1, ptr%size()
            call ptr%get(values(i), i)
         end do

      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32_1d


   module subroutine get_real64(this, value, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL64), intent(out) :: value
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status

      value = DEFAULT_REAL64
      ptr => this%at(SELECTORS, found=found, err_msg=err_msg, __RC__)

      ! Not an error if selector not found when 'found' is used.   Code returns
      ! and value remains undefined.
      if (present(found)) then
         if (.not. found) return
      else
         value = to_float(ptr, err_msg=err_msg, __RC__)
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64


   module subroutine get_real64_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL64), allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: OPT_SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(AbstractNode), pointer :: ptr
      integer :: status
      logical :: found_
      real(kind=REAL64) :: tmp
      integer :: i

      allocate(values(0)) ! unsure that return value is defined

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=found_, err_msg=err_msg, __RC__)
      if (present(found)) found=found_

      if (.not. found_) then
         if (.not. present(found)) then
            __FAIL2__(YAFYAML_SELECTOR_NOT_FOUND)
         end if
      else ! found but possible type-mismatch
         __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)
         ! check type of each entry ...
         do i = 1, ptr%size()
            call ptr%get(tmp, i, err_msg=err_msg, rc=status)
            __VERIFY2__(err_msg, status)
         end do

         deallocate(values)
         allocate(values(ptr%size()))
         do i = 1, ptr%size()
            call ptr%get(values(i), i)
         end do

      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64_1d


end submodule BaseNode_implementation
