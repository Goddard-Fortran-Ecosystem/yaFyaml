#include "error_handling.h"
#include "string_handling.h"
#define SELECTORS s1, s2, s3, s4, s5, s6, s7, s8, s9


! This class exists largely to shield Fortran developers from the
! inherent polymorphism that underpins the representation of different
! yaml types: seqence, mapping, and scalars (int, float, bool,
! string).

! Unfortunately that still requires a great deal of trivial wrappers
! ...

   ! Users that wish to grow a Configuration by inserting new
   ! elements, as opposed to just using the parser to construct a
   ! configuration, will need to use the lower level types.


module fy_newConfiguration
   use fy_AbstractNode
   use fy_Mapping
   use fy_MappingNode
   use fy_Sequence
   use fy_SequenceNode
   use fy_KeywordEnforcer
   use fy_ErrorCodes
   use fy_ErrorHandling
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   implicit none
   private

   public :: Configuration

   type :: Configuration
!!$      private
      class(AbstractNode), pointer :: node => null()
   contains
      procedure :: at_multi_selector
      generic :: at => at_multi_selector
      procedure :: of_multi_selector
      generic :: of => of_multi_selector

      procedure :: get_logical
      procedure :: get_string
      procedure :: get_integer32
      procedure :: get_integer64
      procedure :: get_real32
      procedure :: get_real64
      procedure :: get_logical_1d
      procedure :: get_integer32_1d
      procedure :: get_integer64_1d
      procedure :: get_real32_1d
      procedure :: get_real64_1d
      generic :: get => get_logical, get_logical_1d
      generic :: get => get_string
      generic :: get => get_integer32, get_integer32_1d
      generic :: get => get_integer64, get_integer64_1d
      generic :: get => get_real32, get_real32_1d
      generic :: get => get_real64, get_real64_1d

      procedure, pass(this) :: assign_to_logical
      procedure, pass(this) :: assign_to_string
      procedure, pass(this) :: assign_to_integer32
      procedure, pass(this) :: assign_to_integer64
      procedure, pass(this) :: assign_to_real32
      procedure, pass(this) :: assign_to_real64
      generic :: assignment(=) => assign_to_logical
      generic :: assignment(=) => assign_to_string
      generic :: assignment(=) => assign_to_integer32
      generic :: assignment(=) => assign_to_integer64
      generic :: assignment(=) => assign_to_real32
      generic :: assignment(=) => assign_to_real64

      ! using YAML terminology
      procedure :: is_sequence
      procedure :: is_mapping
      procedure :: is_scalar
      procedure :: is_bool
      procedure :: is_string
      procedure :: is_int
      procedure :: is_float

      procedure :: size
      procedure :: clear

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

      procedure :: begin => begin_cfg
      procedure :: end => end_cfg

   end type Configuration

   interface Configuration
      module procedure new_Configuration
   end interface Configuration

contains

   function new_Configuration(node) result(config)
      type(Configuration) :: config
      class(AbstractNode), target, intent(in) :: node

      config%node => node

   end function new_Configuration

   integer(kind=INT64)function size(this)
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
         
   end function size

   ! With error handling
   function at_multi_selector(this, SELECTORS, unusable, found, err_msg, rc) result(subcfg)
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
   function of_multi_selector(this, SELECTORS) result(subcfg)
      type(Configuration) :: subcfg
      class(Configuration), target, intent(in) :: this
      class(*), optional, intent(in) :: SELECTORS

      subcfg%node => this%node%of(SELECTORS)

   end function of_multi_selector


   subroutine get_logical(this, value, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      logical, intent(out) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical


   subroutine get_logical_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      logical, allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_logical_1d
   

   subroutine get_string(this, value, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      character(:), allocatable, intent(out) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_string


   subroutine get_integer32(this, value, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      integer(kind=INT32), intent(out) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32

   subroutine get_integer32_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      integer(kind=INT32), allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer32_1d

   subroutine get_integer64(this, value, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      integer(kind=INT64), intent(out) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64

   subroutine get_integer64_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      integer(kind=INT64), allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_integer64_1d


   subroutine get_real32(this, value, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      real(kind=REAL32), intent(out) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32

   subroutine get_real32_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      real(kind=REAL32), allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real32_1d

   subroutine get_real64(this, value, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      real(kind=REAL64), intent(out) :: value
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(value, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64

   subroutine get_real64_1d(this, values, SELECTORS, unusable, found, err_msg, rc)
      class(Configuration), target, intent(in) :: this
      real(kind=REAL64), allocatable, intent(out) :: values(:)
      class(*), optional, intent(in) :: SELECTORS
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(out) :: found
      STRING_DUMMY, optional, intent(inout) :: err_msg
      integer, optional, intent(out) :: rc

      call this%node%get(values, SELECTORS, found=found, err_msg=err_msg, rc=rc)

      __UNUSED_DUMMY__(unusable)
   end subroutine get_real64_1d



   subroutine assign_to_logical(flag, this)
      logical, intent(out) :: flag
      class(Configuration), intent(in) :: this
      flag = this%node
   end subroutine assign_to_logical

   subroutine assign_to_string(string, this)
      character(:), allocatable, intent(out) :: string
      class(Configuration), intent(in) :: this
      string = this%node
   end subroutine assign_to_string

   subroutine assign_to_integer32(i32, this)
      integer(kind=INT32), intent(out) :: i32
      class(Configuration), intent(in) :: this
      i32 = this%node
   end subroutine assign_to_integer32

   subroutine assign_to_integer64(i64, this)
      integer(kind=INT64), intent(out) :: i64
      class(Configuration), intent(in) :: this
      i64 = this%node
   end subroutine assign_to_integer64

   subroutine assign_to_real32(r32, this)
      real(kind=REAL32), intent(out) :: r32
      class(Configuration), intent(in) :: this
      r32 = this%node
   end subroutine assign_to_real32

   subroutine assign_to_real64(r64, this)
      real(kind=REAL64), intent(out) :: r64
      class(Configuration), intent(in) :: this
      r64 = this%node
   end subroutine assign_to_real64




   logical function is_sequence(this)
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_sequence = node%is_sequence()
   end function is_sequence

   logical function is_mapping(this)
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_mapping = node%is_mapping()
   end function is_mapping

   logical function is_scalar(this)
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_scalar = node%is_scalar()
   end function is_scalar

   logical function is_bool(this)
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_bool = node%is_bool()
   end function is_bool

   logical function is_string(this)
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_string = node%is_string()
   end function is_string

   logical function is_int(this)
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_int = node%is_int()
   end function is_int
   
   logical function is_float(this)
      class(Configuration), intent(in) :: this
      class(AbstractNode), pointer :: node
      node => this%node
      is_float = node%is_float()
   end function is_float


   ! Factory methods to create an iterator
   function begin_cfg(this, unusable, err_msg, rc) result(iter)
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
         error stop
      end if
         
      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function begin_cfg

   function end_cfg(this, unusable, err_msg, rc) result(iter)
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
         error stop
      end if

      __RETURN__(YAFYAML_SUCCESS)
      __UNUSED_DUMMY__(unusable)
   end function end_cfg

   subroutine clear(this)
      class(Configuration), intent(inout) :: this
      deallocate(this%node)
      nullify(this%node)
   end subroutine clear


  subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
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

end module fy_newConfiguration
