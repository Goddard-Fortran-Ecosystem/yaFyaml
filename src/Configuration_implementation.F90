#include "error_handling.h"
#include "string_handling.h"
#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9
#define OPT_SELECTORS s2, s3, s4, s5, s6, s7, s8, s9

submodule (fy_Configuration)  Configuration_implementation
   use, intrinsic :: iso_fortran_env, only: INT32, INT64, REAL32, REAL64
contains
   
   module function size_config(this) result(size)
      integer(kind=INT64) :: size
      class(Configuration), intent(in) :: this
      type(Mapping), pointer :: m
      type(Sequence), pointer :: s

      if (this%is_scalar()) then
         size = 1
      elseif (this%is_mapping()) then
         m => to_mapping(this%node)
         size = m%size()
      elseif (this%is_sequence()) then
         s => to_sequence(this%node)
         size = s%size()
      else
         error stop "impossible type of config"
      end if
         
   end function size_config

   ! With error handling
   module function at_multi_selector(this, SELECTORS, unusable, found, err_msg, rc) result(subcfg)
      use fy_KeywordEnforcer
      type(Configuration) :: subcfg
      class(Configuration), target, intent(in) :: this
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      subcfg%node => this%node%at(SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end function at_multi_selector

   ! No error handling
   module function of_multi_selector(this, SELECTORS) result(subcfg)
      type(Configuration) :: subcfg
      class(Configuration), target, intent(in) :: this
      class(*), optional, intent(in) :: SELECTORS

      subcfg%node => this%node%of(SELECTORS)

   end function of_multi_selector

   module function has_selector(this, s1, OPT_SELECTORS) result(has)
      logical :: has
      class(Configuration), target, intent(in) :: this
      class(*), optional, intent(in) :: SELECTORS

      has = this%node%has(SELECTORS)

   end function has_selector


   module subroutine get_logical(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      logical, intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical


   module subroutine get_logical_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      logical, allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical_1d
   

   module subroutine get_string(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      character(:), allocatable, intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_string


   module subroutine get_integer32(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      integer(kind=INT32), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32

   module subroutine get_integer32_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      integer(kind=INT32), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32_1d

   module subroutine get_integer64(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      integer(kind=INT64), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64

   module subroutine get_integer64_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      integer(kind=INT64), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64_1d


   module subroutine get_real32(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      real(kind=REAL32), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32

   module subroutine get_real32_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      real(kind=REAL32), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32_1d

   module subroutine get_real64(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      real(kind=REAL64), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64

   module subroutine get_real64_1d(this, values, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      real(kind=REAL64), allocatable, intent(inout) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64_1d

   module subroutine get_subconfig(this, value, SELECTORS, unusable, err_msg, rc)
      use fy_KeywordEnforcer
      class(Configuration), target, intent(in) :: this
      type(Configuration), intent(inout) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      value%node => this%node%at(SELECTORS, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_subconfig



   module subroutine assign_to_logical(flag, this)
      logical, intent(out) :: flag
      class(Configuration), intent(in) :: this
      flag = this%node
   end subroutine assign_to_logical

   module subroutine assign_to_string(string, this)
      character(:), allocatable, intent(out) :: string
      class(Configuration), intent(in) :: this
      string = this%node
   end subroutine assign_to_string

   module subroutine assign_to_integer32(i32, this)
      integer(kind=INT32), intent(out) :: i32
      class(Configuration), intent(in) :: this
      i32 = this%node
   end subroutine assign_to_integer32

   module subroutine assign_to_integer64(i64, this)
      integer(kind=INT64), intent(out) :: i64
      class(Configuration), intent(in) :: this
      i64 = this%node
   end subroutine assign_to_integer64

   module subroutine assign_to_real32(r32, this)
      real(kind=REAL32), intent(out) :: r32
      class(Configuration), intent(in) :: this
      r32 = this%node
   end subroutine assign_to_real32

   module subroutine assign_to_real64(r64, this)
      real(kind=REAL64), intent(out) :: r64
      class(Configuration), intent(in) :: this
      r64 = this%node
   end subroutine assign_to_real64




   module function is_sequence(this)
      logical :: is_sequence
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_sequence = node%is_sequence()
   end function is_sequence

   module function is_mapping(this)
      logical :: is_mapping
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_mapping = node%is_mapping()
   end function is_mapping

   module function is_scalar(this)
      logical :: is_scalar
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_scalar = node%is_scalar()
   end function is_scalar

   module function is_bool(this)
      logical :: is_bool
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_bool = node%is_bool()
   end function is_bool

   module function is_string(this)
      logical :: is_string
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_string = node%is_string()
   end function is_string

   module function is_int(this)
      logical :: is_int
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_int = node%is_int()
   end function is_int
   
   module function is_float(this)
      logical :: is_float
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_float = node%is_float()
   end function is_float


   ! Factory methods to create an iterator
   module function begin_cfg(this, unusable, err_msg, rc) result(iter)
      use fy_KeywordEnforcer
      type(MappingIterator) :: iter
      class(Configuration), intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(out) :: err_msg
      integer, optional, intent(out) :: rc

      type(Mapping), pointer :: m
      integer :: status

      if (this%is_mapping()) then
         m => to_mapping(this%node)
         iter = m%begin()
      else
         __FAIL2__(YAFYAML_NOT_A_MAPPING)
         error stop "expected mapping"
      end if
         
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function begin_cfg

   module function end_cfg(this, unusable, err_msg, rc) result(iter)
      use fy_KeywordEnforcer
      type(MappingIterator) :: iter
      class(Configuration), intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      STRING_DUMMY, optional, intent(out) :: err_msg
      integer, optional, intent(out) :: rc

      type(Mapping), pointer :: m
      integer :: status

      if (this%is_mapping()) then
         m => to_mapping(this%node)
         iter = m%end()
      else
         __FAIL2__(YAFYAML_NOT_A_MAPPING)
         error stop "epected mapping"
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function end_cfg

   module subroutine clear(this)
      class(Configuration), intent(inout) :: this
      deallocate(this%node)
      nullify(this%node)
   end subroutine clear


  module subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
    class(Configuration), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    class(AbstractNode), pointer :: node

    node => this%node
    call node%write_node_formatted(unit, iotype, v_list, iostat, iomsg)
!!$    write(unit,'(DT)', iostat=iostat)this%node
    
  end subroutine write_formatted

end submodule Configuration_implementation
