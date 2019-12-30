module fy_FailsafeSchema
  use fy_AbstractSchema
  implicit none
  private

  public :: FailsafeSchema

  type, extends(AbstractSchema) :: FailsafeSchema
     private
   contains
     procedure, nopass :: matches_null
     procedure, nopass :: matches_logical
     procedure, nopass :: matches_integer
     procedure, nopass :: matches_real

     procedure, nopass :: to_logical
     procedure, nopass :: to_integer
     procedure, nopass :: to_real
  end type FailsafeSchema

contains


  logical function matches_null(text) result(matches)
    character(*), intent(in) :: text

    matches = .false.

  end function matches_null


  logical function matches_logical(text) result(matches)
    character(*), intent(in) :: text

    matches = .false.

  end function matches_logical
  

  ! Matches: 0 | -? [1-9] [0-9]*
  logical function matches_integer(text) result(matches)
    character(*), intent(in) :: text

    matches = .false.
          
  end function matches_integer
  

  ! Matches: -? ( 0 | [1-9] [0-9]* ) ( \. [0-9]* )? ( [eE] [-+]? [0-9]+ )?
  logical function matches_real(text) result(matches)
    character(*), intent(in) :: text

    matches = .false.

  end function matches_real


  logical function to_logical(text)
    character(*), intent(in) :: text
    error stop 'Failsafe schema does not support bool'
  end function to_logical

  integer function to_integer(text)
    character(*), intent(in) :: text
    error stop 'Failsafe schema does not support integer'
  end function to_integer

  real function to_real(text)
    character(*), intent(in) :: text
    error stop 'Failsafe schema does not support float'
  end function to_real

end module fy_FailsafeSchema
  
