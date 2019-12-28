program main
  use yaFyaml
  use fy_TokenVector
  implicit none

  type(Lexer) :: lexr
  class(AbstractToken), allocatable :: token
  integer :: status
  character(:), allocatable :: id


  lexr = Lexer(Reader(EscapedTextStream("---\n- a \n- b \n- c \n...\n")))

  do
     token = lexr%get_token(rc=status)
     print*,__FILE__,__LINE__,status
     id = token%get_id()
     print*,__FILE__,__LINE__, id
     select type(token)
     type is(StreamEndToken)
        exit
     type is (ScalarToken)
        print*,__FILE__,__LINE__, 'value = <',token%value,'>'
     end select
  end do
     
end program main

  
