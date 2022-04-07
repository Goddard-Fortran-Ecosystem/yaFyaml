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
#define OPT_SELECTORS s2, s3, s4, s5, s6, s7, s8, s9

   module function at_multi_selector(this, SELECTORS, unusable, found, err_msg, rc) result(ptr)
      class(YAML_Node), pointer :: ptr
      class(BaseNode), target, intent(in) :: this
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      integer :: status
      type(Sequence), target :: v

      v = selectors(SELECTORS, err_msg=err_msg,rc=status)
      __VERIFY2__(err_msg, status)

      ptr => at_vector_selector(this, v, unusable, found=found, err_msg=err_msg, rc=status)
      ! We don't raise a new exception here - makes testing difficult.
      ! Should have a new macro.
      if (status /= YAFYAML_SUCCESS) then
         if (present(rc)) then
            rc = status
            return
         end if
      end if


      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

   end function

   function at_vector_selector(this, v, unusable, found, err_msg, rc) result(ptr)
      class(YAML_Node), pointer :: ptr
      class(BaseNode), target, intent(in) :: this
      type(Sequence), intent(in) :: v
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc


      class(YAML_Node), pointer :: config, next_config
      class(*), pointer :: selector
      
      integer :: status
      type(SequenceIterator) :: iter
      logical :: found_

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

      ! selector for sequence must be some kind of integer
      subroutine get_sequence_item(s, selector, found, rc)
         type(Sequence), target, intent(in) :: s
         class(YAML_Node), intent(in) :: selector
         logical, intent(out) :: found
         integer, intent(out) :: rc

         integer(kind=INT64), pointer :: i
         integer :: status

         select type (idx => selector)
         type is (IntNode)
            i => to_int(idx, rc=rc)
            if (rc /= YAFYAML_SUCCESS) then
               found = .false.
               return
            end if

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
      
      ! While a mapping may have keys that are any subclass of YAML_Node.
      subroutine get_mapping_item(m, selector, found, rc)
         type(Mapping), target, intent(in) :: m
         class(YAML_Node), intent(in) :: selector
         logical, intent(out) :: found
         integer, intent(out) :: rc

         integer :: status

         if (m%count(selector) == 0) then
            found = .false.
            rc = YAFYAML_SELECTOR_NOT_FOUND
            next_config => null()
            return
         end if

         next_config => m%at(selector, rc=status)

         if (status == 0) then
            found = .true.
            rc = YAFYAML_SUCCESS
         else  ! should not be possible
            found = .false.
            rc = YAFYAML_SELECTOR_NOT_FOUND
         end if
            
      end subroutine get_mapping_item

   end function at_vector_selector

   ! Error conditions
   ! 1. Selected item is not found _and_ found is not used
   ! 2. Selected item does exist but is of the wrong type

   module subroutine get_logical(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      logical, intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      logical, pointer :: bool_ptr
      logical :: was_found

      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)
      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)

      bool_ptr => to_bool(ptr, err_msg=err_msg, __RC__)
      __ASSERT2__(associated(bool_ptr), YAFYAML_TYPE_MISMATCH)

      value = bool_ptr

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical

   module subroutine get_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      logical, allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      logical :: tmp
      integer :: i
      logical :: was_found

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)
      __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)

      ! check type of each entry ...
      do i = 1, ptr%size()
         call ptr%get(tmp, i, err_msg=err_msg, rc=status)
         __VERIFY2__(err_msg, status)
      end do
      
      if (allocated(values)) deallocate(values)
      allocate(values(ptr%size()))
      do i = 1, ptr%size()
         call ptr%get(values(i), i)
      end do

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical_1d


   module subroutine get_string(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      character(:), allocatable, intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      logical :: was_found

      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)

      value = to_string(ptr, err_msg=err_msg, __RC__)

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_string


   module subroutine get_integer32(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT32), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      integer(kind=INT64), pointer :: safe_value
      logical :: was_found
      logical :: is_safe

      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)

      ! Must assign to 64-bit first to protect against overflow

      safe_value => to_int(ptr, err_msg=err_msg, __RC__)
      __ASSERT2__(associated(safe_value),YAFYAML_TYPE_MISMATCH)
      
      is_safe = (safe_value >= -huge(1_INT32) .and. safe_value <= huge(1_INT32))
      __ASSERT2__(is_safe, YAFYAML_INT32_OVERLFLOW)

      ! conversion is safe
      value = safe_value 
   
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32

   module subroutine get_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT32), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      integer(kind=INT32) :: tmp
      integer :: i
      logical :: was_found

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)
      __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)

      ! check type (and conversion) of each entry ...
      do i = 1, ptr%size()
         call ptr%get(tmp, i, err_msg=err_msg, rc=status)
         __VERIFY2__(err_msg, status)
      end do

      if (allocated(values)) deallocate(values)
      allocate(values(ptr%size()))
      do i = 1, ptr%size()
         call ptr%get(values(i), i)
      end do

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32_1d


   module subroutine get_integer64(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT64), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      integer(kind=INT64), pointer :: safe_value
      logical :: was_found

      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)

      safe_value => to_int(ptr, err_msg=err_msg, __RC__)
      __ASSERT2__(associated(safe_value),YAFYAML_TYPE_MISMATCH)

      value = safe_value
      
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64


   module subroutine get_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      integer(kind=INT64), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      integer(kind=INT64) :: tmp
      integer :: i
      logical :: was_found

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)
      __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)

      ! check type of each entry ...
      do i = 1, ptr%size()
         tmp = 0
         call ptr%get(tmp, i, err_msg=err_msg, rc=status)
         __VERIFY2__(err_msg, status)
      end do
      
      if (allocated(values)) deallocate(values)
      allocate(values(ptr%size()))
      do i = 1, ptr%size()
         call ptr%get(values(i), i)
      end do
         
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64_1d


   module subroutine get_real32(this, value, SELECTORS, unusable, err_msg, rc)
      use, intrinsic :: ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
      use, intrinsic :: ieee_arithmetic, only: IEEE_POSITIVE_INF, IEEE_NEGATIVE_INF
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL32), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      real(kind=REAL64), pointer :: safe_value
      real(kind=REAL32) :: pos_inf, neg_inf
      logical :: was_found

      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)

      ! unless it is a float in the acceptable range ...
      safe_value => to_float(ptr, err_msg=err_msg, __RC__)
      __ASSERT2__(associated(safe_value),YAFYAML_TYPE_MISMATCH)

      if (safe_value /= safe_value) then ! nan
         value = ieee_value(value, IEEE_QUIET_NAN)
      else
         ! conversion is always possible because of +/- Inf
         neg_inf = ieee_value(value, IEEE_NEGATIVE_INF)
         pos_inf = ieee_value(value, IEEE_POSITIVE_INF)
         ! if not inf and out of range then is an error
         ! and do not update value.
         if (safe_value <= -huge(1._REAL32)) then
            value = neg_inf
         else if (safe_value >= huge(1._REAL32)) then
            value = pos_inf
         else
            value = safe_value
         end if
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32

   module subroutine get_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL32), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      real(kind=REAL32) :: tmp
      integer :: i
      logical :: was_found

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)
      __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)

      ! check type of each entry ...
      do i = 1, ptr%size()
         call ptr%get(tmp, i, err_msg=err_msg, rc=status)
         __VERIFY2__(err_msg, status)
      end do

      if (allocated(values)) deallocate(values)
      allocate(values(ptr%size()))
      do i = 1, ptr%size()
         call ptr%get(values(i), i)
      end do

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32_1d


   module subroutine get_real64(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL64), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      real(kind=REAL64), pointer :: safe_value
      logical :: was_found

      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)

      safe_value => to_float(ptr, err_msg=err_msg, __RC__)
      __ASSERT2__(associated(safe_value), YAFYAML_TYPE_MISMATCH)
      value = safe_value


      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64


   module subroutine get_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(BaseNode), target, intent(in) :: this
      real(kind=REAL64), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS ! s2 - s9
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      real(kind=REAL64) :: tmp
      integer :: i
      logical :: was_found

      ! Is the selector list valid?
      ptr => this%at(SELECTORS, found=was_found, err_msg=err_msg, __RC__)

      __ASSERT2__(was_found, YAFYAML_SELECTOR_NOT_FOUND)
      __ASSERT2__(ptr%is_sequence(),YAFYAML_TYPE_MISMATCH)

      ! check type of each entry ...
      do i = 1, ptr%size()
         call ptr%get(tmp, i, err_msg=err_msg, rc=status)
         __VERIFY2__(err_msg, status)
      end do
      
      if (allocated(values)) deallocate(values)
      allocate(values(ptr%size()))
      do i = 1, ptr%size()
         call ptr%get(values(i), i)
      end do

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64_1d

   ! Useful helper function
   function selectors(SELECTORS, unusable, err_msg, rc) result(v)
      type(sequence) :: v
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      integer :: status
      
      call save_one(v, s1)
      __VERIFY2__(err_msg, status)
      call save_one(v, s2)
      __VERIFY2__(err_msg, status)
      call save_one(v, s3)
      __VERIFY2__(err_msg, status)
      call save_one(v, s4)
      __VERIFY2__(err_msg, status)
      call save_one(v, s5)
      __VERIFY2__(err_msg, status)
      call save_one(v, s6)
      __VERIFY2__(err_msg, status)
      call save_one(v, s7)
      __VERIFY2__(err_msg, status)
      call save_one(v, s8)
      __VERIFY2__(err_msg, status)
      call save_one(v, s9)
      __VERIFY2__(err_msg, status)

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   contains
      
      subroutine save_one(v, arg)
         use fy_String
         type(sequence), intent(inout) :: v
         class(*), optional, intent(in) :: arg

         status = YAFYAML_SUCCESS ! unless
         if (present(arg)) then
            select type (arg)
            type is (String)
               call v%push_back(StringNode(arg%s))
            type is (character(*))
               call v%push_back(StringNode(arg))
            type is (logical)
               call v%push_back(BoolNode(arg))
            type is (integer(kind=INT32))
               call v%push_back(IntNode(arg))
            type is (integer(kind=INT64))
               call v%push_back(IntNode(arg))
            type is (real(kind=REAL32))
               call v%push_back(FloatNode(arg))
            type is (real(kind=REAL64))
               call v%push_back(FloatNode(arg))
            class default
               __FAIL2__(YAFYAML_TYPE_MISMATCH)
            end select
         end if

         __RETURN__(YAFYAML_SUCCESS)
      end subroutine save_one
  end function selectors
  
  module subroutine set_logical(this, value, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_BoolNode
     class(BaseNode), target, intent(inout) :: this
     logical, intent(in) :: value
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc

     call this%set(BoolNode(value), SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine set_logical


  module subroutine set_string(this, value, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_StringNode
     class(BaseNode), target, intent(inout) :: this
     character(*), intent(in) :: value
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc
     
     call this%set(StringNode(value), SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine set_string


  module subroutine set_integer32(this, value, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_IntNode
     class(BaseNode), target, intent(inout) :: this
     integer(kind=INT32), intent(in) :: value
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc
     
     call this%set(IntNode(value), SELECTORS, err_msg=err_msg, rc=rc)

     __UNUSED_DUMMY__(unusable)
  end subroutine set_integer32


  module subroutine set_integer64(this, value, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_IntNode
     class(BaseNode), target, intent(inout) :: this
     integer(kind=INT64), intent(in) :: value
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc
     
     call this%set(IntNode(value), SELECTORS, err_msg=err_msg, rc=rc)

     __UNUSED_DUMMY__(unusable)
  end subroutine set_integer64


  module subroutine set_real32(this, value, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_FloatNode
     class(BaseNode), target, intent(inout) :: this
     real(kind=REAL32), intent(in) :: value
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc
     
     call this%set(FloatNode(value), SELECTORS, err_msg=err_msg, rc=rc)

     __UNUSED_DUMMY__(unusable)
  end subroutine set_real32


  module subroutine set_real64(this, value, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_FloatNode
     class(BaseNode), target, intent(inout) :: this
     real(kind=REAL64), intent(in) :: value
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc
     
     call this%set(FloatNode(value), SELECTORS, err_msg=err_msg, rc=rc)

     __UNUSED_DUMMY__(unusable)
  end subroutine set_real64


  module subroutine set_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_SequenceNode
     use fy_BoolNode
     class(BaseNode), target, intent(inout) :: this
     logical, intent(in) :: values(:)
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc

     type(Sequence) :: s
     integer :: i

     do i = 1, product(shape(values)) ! size(values)
        call s%push_back(BoolNode(values(i)))
     end do

     call this%set(SequenceNode(s), SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine set_logical_1d

  module subroutine set_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_SequenceNode
     use fy_IntNode
     class(BaseNode), target, intent(inout) :: this
     integer(kind=INT32), intent(in) :: values(:)
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc

     type(Sequence) :: s
     integer :: i

     do i = 1, product(shape(values)) ! size(values)
        call s%push_back(IntNode(values(i)))
     end do

     call this%set(SequenceNode(s), SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine set_integer32_1d

  module subroutine set_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_SequenceNode
     use fy_IntNode
     class(BaseNode), target, intent(inout) :: this
     integer(kind=INT64), intent(in) :: values(:)
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc

     type(Sequence) :: s
     integer :: i

     do i = 1, product(shape(values)) ! size(values)
        call s%push_back(IntNode(values(i)))
     end do

     call this%set(SequenceNode(s), SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine set_integer64_1d

  module subroutine set_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_SequenceNode
     use fy_FloatNode
     class(BaseNode), target, intent(inout) :: this
     real(kind=REAL32), intent(in) :: values(:)
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc

     type(Sequence) :: s
     integer :: i

     do i = 1, product(shape(values)) ! size(values)
        call s%push_back(FloatNode(values(i)))
     end do

     call this%set(SequenceNode(s), SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine set_real32_1d

  module subroutine set_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
     use fy_KeywordEnforcer
     use fy_SequenceNode
     use fy_FloatNode
     class(BaseNode), target, intent(inout) :: this
     real(kind=REAL64), intent(in) :: values(:)
     class(*), optional, intent(in) :: SELECTORS
     class(KeywordEnforcer), optional, intent(in) :: unusable
     STRING_DUMMY, optional, intent(inout) :: err_msg
     integer, optional, intent(out) :: rc

     type(Sequence) :: s
     integer :: i

     do i = 1, product(shape(values)) ! size(values)
        call s%push_back(FloatNode(values(i)))
     end do

     call this%set(SequenceNode(s), SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine set_real64_1d

   module subroutine set_node(this, node, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      use fy_SequenceNode
      use fy_MappingNode
      class(BaseNode), target, intent(inout) :: this
      class(YAML_Node), intent(in) :: node
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      type(sequence) :: selectors_seq
      integer :: status

      selectors_seq = selectors(SELECTORS, err_msg=err_msg, rc=status)
      __VERIFY2__(err_msg, status)

      call set_node_p(this, node, selectors_seq, err_msg=err_msg, rc=status)
      __VERIFY2__(err_msg, status)

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)

   end subroutine set_node

   subroutine set_node_p(this, node, selectors_seq, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      use fy_SequenceNode, only: clone
      use fy_MappingNode, only: clone
      class(BaseNode), target, intent(inout) :: this
      class(YAML_Node), intent(in) :: node
      type(Sequence), target, intent(in) :: selectors_seq
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      class(YAML_Node), pointer :: ptr
      integer :: status
      logical, pointer :: bool_ptr
      logical :: was_found
      type(UnlimitedVector) :: v
      integer(kind=INT64), pointer :: i

      class(YAML_Node), pointer :: parent_node
      class(YAML_Node), pointer :: last_selector
      class(YAML_Node), pointer :: new_node
      Type(Sequence) :: parent_selectors
      type(Sequence), pointer :: seq
      type(Mapping), pointer :: map
      type(SequenceNode), pointer :: p_seq
      type(MappingNode), pointer :: p_map

      __ASSERT2__(selectors_seq%size() > 0, YAFYAML_MISSING_SELECTOR)

      last_selector => selectors_seq%back()
      call clone(selectors_seq, parent_selectors)
      call parent_selectors%pop_back()

      !TODO: make at_vector_selector() a TBP
      parent_node => at_vector_selector(this, parent_selectors, err_msg=err_msg, rc=status)
      __VERIFY2__(err_msg, status)

      select type (q => parent_node)
      type is (SequenceNode)

         seq => to_sequence(q, err_msg=err_msg, rc=status)
         __VERIFY2__(err_msg, status)
         select type (idx => last_selector)
         type is (IntNode)
            i => to_int(idx, rc=rc)
            if (rc /= YAFYAML_SUCCESS) return
            __ASSERT2__(i > 0 .and. i <= seq%size(), YAFYAML_SEQUENCE_INDEX_OUT_OF_BOUNDS)
            call seq%set(i, node)
         class default
            __FAIL2__(YAFYAML_TYPE_MISMATCH)
         end select

      type is (MappingNode)
         map => to_mapping(q, err_msg=err_msg, rc=status)
         __VERIFY2__(err_msg, status)

         ! If node is a nested type (sequence or mapping) then some
         ! compilers fail to correctly do a deep copy, so we must
         ! proceed in an indirect fashion.
         select type (qq => node)
         type is (SequenceNode)
            call map%set(last_selector, SequenceNode())
            new_node => map%at(last_selector)
            call clone(from=qq, to=new_node)
         type is (MappingNode)
            call map%set(last_selector, MappingNode())
            new_node => map%at(last_selector)
            call clone(from=qq, to=new_node)
         class default
            call map%set(last_selector, node)
         end select
      class default
         ! Not the ideal error code.  May need a new one
         __FAIL2__(YAFYAML_SELECTOR_NOT_FOUND)
      end select
         

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end subroutine set_node_p


   module function has_selector(this, SELECTORS) result(has)
      logical :: has
      class(BaseNode), intent(in) :: this
      class(*), intent(in) :: s1
      class(*), optional, intent(in) :: OPT_SELECTORS

      logical :: was_found
      integer :: status
      class(YAML_Node), pointer :: node_ptr

      node_ptr => this%at(SELECTORS, found=was_found, rc=status)
      has = was_found

   end function has_selector

   subroutine selectors_to_vector(v, SELECTORS)
      type (UnlimitedVector), intent(out) :: v
      class(*), optional, intent(in) :: SELECTORS
      
      if (present(s1)) call save_one(v, s1)
      if (present(s2)) call save_one(v, s2)
      if (present(s3)) call save_one(v, s3)
      if (present(s4)) call save_one(v, s4)
      if (present(s5)) call save_one(v, s5)
      if (present(s6)) call save_one(v, s6)
      if (present(s7)) call save_one(v, s7)
      if (present(s8)) call save_one(v, s8)
      if (present(s9)) call save_one(v, s9)

contains

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
   end subroutine selectors_to_vector


end submodule BaseNode_implementation
