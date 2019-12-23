module fy_ErrorCodes
  implicit none
  public

  enum, bind(c)
     enumerator :: SUCCESS = 0
     enumerator :: UNEXPECTED_CHARACTER
     enumerator :: UNEXPECTED_COLON_IN_PLAIN_SCALAR
     enumerator :: END_OF_STREAM_INSIDE_QUOTES
     enumerator :: UNEXPECTED_DOCUMENT_SEPARATOR
     enumerator :: UNKNOWN_ESCAPE_CHARACTER_IN_DOUBLE_QUOTED_SCALAR
  end enum
  
end module fy_ErrorCodes
