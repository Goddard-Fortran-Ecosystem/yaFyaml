module fy_UnlimitedUtilities
  use fy_String
  use fy_ArrayWrapper
  use gFTL_UnlimitedVector
  implicit none
  private

  public :: is_logical
  public :: is_integer
  public :: is_real
  public :: is_string

  interface is_logical
     module procedure is_logical_scalar
     module procedure is_logical_array
  end interface is_logical

  interface is_integer
     module procedure is_integer_scalar
!!$     module procedure is_integer_array
  end interface is_integer

  interface is_real
     module procedure is_real_scalar
!!$     module procedure is_real_array
  end interface is_real

  interface is_string
     module procedure is_string_scalar
!!$     module procedure is_string_array
  end interface is_string


contains


  recursive logical function is_logical_scalar(u) result(is_logical)
    class(*), intent(in) :: u
    type (UnlimitedVectorIterator) :: iter
    
    select type (u)
    type is (logical)
       is_logical = .true.
    type is (ArrayWrapper)
       is_logical = is_logical_array(u%elements)
    type is (UnlimitedVector)
       is_logical = .true. ! unless
       iter = u%begin()
       do while (iter /= u%end())
          if (.not. is_logical_scalar(iter%get())) then
             is_logical = .false.
             return
          end if
          call iter%next()
       end do
    class default
       is_logical = .false.
    end select
    
  end function is_logical_scalar


  logical function is_logical_array(u) result(is_logical)
    class(*), intent(in) :: u(:)

    select type (u)
    type is (logical)
       is_logical = .true.
    class default
       is_logical = .false.
    end select
    
  end function is_logical_array


  logical function is_integer_scalar(u) result(is_integer)
    class(*), intent(in) :: u

    select type (u)
    type is (integer)
       is_integer = .true.
    class default
       is_integer = .false.
    end select
    
  end function is_integer_scalar


  logical function is_real_scalar(u) result(is_real)
    class(*), intent(in) :: u

    select type (u)
    type is (real)
       is_real = .true.
    class default
       is_real = .false.
    end select
    
  end function is_real_scalar


  logical function is_string_scalar(u) result(is_string)
    class(*), intent(in) :: u

    select type (u)
    type is (character(*))
       is_string = .true.
    type is (String)
       is_string = .true.
    class default
       is_string = .false.
    end select
    
  end function is_string_scalar

end module fy_UnlimitedUtilities
