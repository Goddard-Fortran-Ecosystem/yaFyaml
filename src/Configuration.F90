module fy_Configuration
  use, intrinsic :: iso_fortran_env, only: INT32
  use, intrinsic :: iso_fortran_env, only: REAL32
  use gFTL_UnlimitedVector
  use gFTL_StringUnlimitedMap
  use fy_ArrayWrapper
  use fy_KeywordEnforcer
  implicit none
  private

  public :: Configuration
  public :: AllocatableConfiguration
  public :: PointerConfiguration
  public :: NONE

  public :: YAFYAML_SUCCESS
  public :: YAFYAML_TYPE_MISMATCH
  public :: YAFYAML_NOT_AN_ARRAY
  public :: YAFYAML_NOT_A_SCALAR

  integer, parameter :: YAFYAML_SUCCESS = 0
  integer, parameter :: YAFYAML_TYPE_MISMATCH = 1
  integer, parameter :: YAFYAML_NOT_AN_ARRAY = 2
  integer, parameter :: YAFYAML_NOT_A_SCALAR = 3

  type :: NoneObject
  end type NoneObject
  type (NoneObject), target :: NONE

  ! Use this class for memory management
  type, abstract :: Configuration
     private
!!$     class(*), pointer :: node
   contains

     procedure(get_node_interface), deferred :: get_node

     procedure :: is_none
     generic :: assignment(=) => to_logical
     generic :: assignment(=) => to_integer_int32
     generic :: assignment(=) => to_real_real32
     generic :: assignment(=) => to_string
     generic :: assignment(=) => to_logical_array
     generic :: assignment(=) => to_integer_int32_array
     generic :: assignment(=) => to_real_real32_array
     generic :: assignment(=) => to_string_array
     generic :: assignment(=) => to_string_vector
     generic :: assignment(=) => to_unlimited_map

     generic :: get => get_logical

     generic :: at => at_key
     generic :: at => at_index

     procedure, pass(this) :: to_logical
     procedure, pass(this) :: to_integer_int32
     procedure, pass(this) :: to_real_real32
     procedure, pass(this) :: to_string
     procedure, pass(this) :: to_logical_array
     procedure, pass(this) :: to_integer_int32_array
     procedure, pass(this) :: to_real_real32_array
     procedure, pass(this) :: to_string_array
     procedure, pass(this) :: to_string_vector
     procedure, pass(this) :: to_unlimited_map

     procedure :: get_logical

     procedure :: at_key
     procedure :: at_index

  end type Configuration


  type, extends(Configuration) :: AllocatableConfiguration
     private
     class(*), allocatable :: node
   contains
     procedure :: get_node => get_node_alloc
  end type AllocatableConfiguration

  type, extends(Configuration) :: PointerConfiguration
     private
     class(*), pointer :: node => null()
   contains
     procedure :: get_node => get_node_pointer
  end type PointerConfiguration

  interface Configuration
     module procedure new_Configuration_scalar
     module procedure new_Configuration_array
  end interface Configuration

  abstract interface
     subroutine get_node_interface(this, node)
       import Configuration
       class(Configuration), target, intent(in) :: this
       class(*), pointer :: node
     end subroutine get_node_interface
  end interface

contains

  subroutine get_node_alloc(this, node)
    class(AllocatableConfiguration), target, intent(in) :: this
    class(*), pointer :: node

    node => this%node
  end subroutine get_node_alloc

  subroutine get_node_pointer(this, node)
    class(PointerConfiguration), target, intent(in) :: this
    class(*), pointer :: node

    node => this%node
  end subroutine get_node_pointer

  function new_Configuration_scalar(scalar) result(config)
    type (AllocatableConfiguration) :: config
    class(*), intent(in) :: scalar

    config%node = scalar
!!$    allocate(config%node, source=scalar)

  end function new_Configuration_scalar


  function new_Configuration_array(array) result(config)
    type (AllocatableConfiguration) :: config
    class(*), intent(in) :: array(:)

    config%node = ArrayWrapper(array)
!!$    type :: Workaround
!!$       class(*), allocatable :: q
!!$    end type Workaround
!!$    type (Workaround), pointer :: x
!!$
!!$    allocate(x)
!!$    x%q = ArrayWrapper(array)
!!$    config%node => x%q
!!$
!!$
!!$    type(ArrayWrapper) :: wrapper
!!$
!!$    wrapper = ArrayWrapper(array)
!!$    allocate(config%node, source=wrapper)
!!$    select type (array)
!!$    type is (logical)
!!$       print*, 'raw: ', array
!!$    end select
!!$    select type (q => config%node)
!!$    type is (logical)
!!$       print*, 'node inside: ', q
!!$    end select
!!$    select type (q => wrapper%elements)
!!$    type is (logical)
!!$       print*, 'wrapper inside: ', q
!!$    end select
  end function new_Configuration_array


!!$  function get(this) result(config)
!!$    class(Configuration), intent(in) :: this
!!$    type(Configuration) :: config
!!$
!!$    config%node => this%node
!!$
!!$  end function get
!!$
    
    
  logical function is_none(this)
    class(Configuration), target, intent(in) :: this

    class(*), pointer :: node

    call this%get_node(node)
    is_none = same_type_as(node, NONE)
!!$    is_none = associated(this%node, NONE)
  end function is_none

  subroutine to_logical(value, this)
    logical, intent(out) :: value
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    call this%get_node(node)

!!$    select type(node)
    select type(q => node)
    type is (logical)
       value = q
    class default
       value = default_logical()
    end select

  end subroutine to_logical

  subroutine to_integer_int32(value, this)
    integer, intent(out) :: value
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (integer(kind=INT32))
       value = q
    class default
       value = default_int32()
    end select

  end subroutine to_integer_int32

  subroutine to_real_real32(value, this)
    real, intent(out) :: value
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (real(kind=REAL32))
       value = q
    class default
       value = default_real32()
    end select

  end subroutine to_real_real32
  
  subroutine to_string(value, this)
    character(:), allocatable, intent(out) :: value
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (character(*))
       value = q
    class default
       value = default_string()
    end select

  end subroutine to_string
  
  subroutine to_logical_array(values, this)
    logical, allocatable, intent(out) :: values(:)
    class(Configuration), intent(in) :: this

    integer :: i, n

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (logical)
          print*,'qq: ', qq
          values = qq
       class default ! type mismatch
          values = [logical :: ] ! empty array
       end select
    type is (UnlimitedVector)
       ! Check if all elements are logical
       n = q%size()
       do i = 1, n
          select type (qq => q%at(i))
          type is (logical)
          class default
             values = [logical :: ]
             return
          end select
       end do
       allocate(values(n))
       do i = 1, n
          select type (qq => q%at(i))
          type is (logical)
             values(i) = qq
          end select
       end do
    class default ! category mismatch - not an array
       values = [logical :: ] ! empty array
    end select

  end subroutine to_logical_array


  subroutine to_integer_int32_array(values, this)
    integer, allocatable, intent(out) :: values(:)
    class(Configuration), intent(in) :: this

    integer :: i, n

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (integer(kind=INT32))
          values = qq
       class default ! type mismatch
          values = [integer(INT32) :: ] ! empty array
       end select
    type is (UnlimitedVector)
       ! Check if all elements are INT32
       n = q%size()
       do i = 1, n
          select type (qq => q%at(i))
          type is (integer(INT32))
          class default
             values = [integer(INT32) :: ]
             return
          end select
       end do
       allocate(values(n))
       do i = 1, n
          select type (qq => q%at(i))
          type is (integer(INT32))
             values(i) = qq
          end select
       end do
    class default ! category mismatch - not an array
       values = [integer(int32) :: ] ! empty array
    end select

  end subroutine to_integer_int32_array


  subroutine to_real_real32_array(values, this)
    real, allocatable, intent(out) :: values(:)
    class(Configuration), intent(in) :: this

    integer :: i, n

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (real(kind=REAL32))
          values = qq
       class default ! type mismatch
          values = [real(REAL32) :: ] ! empty array
       end select
    type is (UnlimitedVector)
       ! Check if all elements are REAL32
       n = q%size()
       do i = 1, n
          select type (qq => q%at(i))
          type is (real(REAL32))
          class default
             values = [real(REAL32) :: ]
             return
          end select
       end do
       allocate(values(n))
       do i = 1, n
          select type (qq => q%at(i))
          type is (real(REAL32))
             values(i) = qq
          end select
       end do
    class default ! category mismatch - not an array
       values = [real(real32) :: ] ! empty array
    end select

  end subroutine to_real_real32_array


  subroutine to_string_array(values, this)
    character(:), allocatable, intent(out) :: values(:)
    class(Configuration), intent(in) :: this

    integer :: i, n
    integer :: maxlen

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (character(*))
          values = qq
       class default ! type mismatch
          values = [character(0) :: ] ! empty array
       end select
    type is (UnlimitedVector)
       ! Check if all elements are REAL32
       n = q%size()
       maxlen = 0
       do i = 1, n
          select type (qq => q%at(i))
          type is (character(*))
             maxlen = max(maxlen, len(qq))
          class default
             values = [character(0) :: ] ! empty array
             return
          end select
       end do
       allocate(character(maxlen) :: values(n))
       do i = 1, n
          select type (qq => q%at(i))
          type is (character(*))
             values(i) = qq
          end select
       end do
    class default ! category mismatch - not an array
       values = [character(0) :: ] ! empty array
    end select

  end subroutine to_string_array


  subroutine to_string_vector(values, this)
    use gFTL_StringVector
    type (StringVector), intent(out) :: values
    class(Configuration), intent(in) :: this

    integer :: i, n

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (ArrayWrapper)
       select type(qq => q%elements)
       type is (character(*))
          do i = 1, size(qq)
             call values%push_back(qq(i))
          end do
       class default ! type mismatch
          values = StringVector()
       end select
    type is (UnlimitedVector)
       ! Check if all elements are REAL32
       n = q%size()
       do i = 1, n
          select type (qq => q%at(i))
          type is (character(*))
          class default
             values = StringVector()
             return
          end select
       end do
       do i = 1, n
          select type (qq => q%at(i))
          type is (character(*))
             call values%push_back(qq)
          end select
       end do
    class default ! category mismatch - not an array
       values = StringVector()
    end select

  end subroutine to_string_vector


  subroutine to_unlimited_map(values, this)
    use gFTL_StringVector
    type (StringUnlimitedMap), intent(out) :: values
    class(Configuration), intent(in) :: this

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (StringUnlimitedMap)
       values = q
    class default
       values = StringUnlimitedMap() ! empty map
    end select

  end subroutine to_unlimited_map



  subroutine get_logical(this, value, unused, default, is_present, rc)
    class(Configuration), intent(in) :: this
    type (Logical) :: value
    class (KeywordEnforcer), optional, intent(in) :: unused
    type (Logical), optional, intent(in) :: default
    type (Logical), optional, intent(in) :: is_present
    integer, optional, intent(out) :: rc

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (logical)
    end select
    
  end subroutine get_logical


  function at_key(this, key) result(sub_config)
    type(PointerConfiguration) :: sub_config
    class(Configuration), intent(in) :: this
    character(*), intent(in) :: key

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (StringUnlimitedMap)
       if (q%count(key) > 0) then
          sub_config%node => q%at(key)
       else
          ! sub_config is empty
          sub_config%node => NONE ! YAFYAML_NO_SUCH_KEY
       end if
    class default
       ! sub_config is empty
       sub_config%node => NONE ! YAFYAML_NOT_A_MAP
    end select

  end function at_key


  function at_index(this, index) result(sub_config)
    type(PointerConfiguration) :: sub_config
    class(Configuration), intent(in) :: this
    integer, intent(in) :: index

    class(*), pointer :: node
    call this%get_node(node)

    select type(q => node)
    type is (UnlimitedVector)
       if (index >= 1 .and. index <= q%size()) then
          sub_config%node => q%at(index)
       else
          ! sub_config is empty
          sub_config%node => NONE ! YAFYAML_INDEX_OUT_OF_BOUNDS
       end if
    class default
       ! sub_config is empty
       sub_config%node => NONE ! YAFYAML_NOT_A_VECTOR
    end select
  end function at_index



  logical function default_logical()
    default_logical = .false.
  end function default_logical

  integer(kind=INT32) function default_int32()
    default_int32 = -HUGE(1)
  end function default_int32
  
  real(kind=REAL32) function default_real32()
    use, intrinsic :: ieee_arithmetic
    default_real32 = ieee_value(1._REAL32,  IEEE_QUIET_NAN)
  end function default_real32

  function default_string() result(s)
    character(len=0) :: s
    s = ''
  end function default_string



end module fy_Configuration

