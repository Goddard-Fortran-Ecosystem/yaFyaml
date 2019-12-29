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

end module fy_FailsafeSchema
  
