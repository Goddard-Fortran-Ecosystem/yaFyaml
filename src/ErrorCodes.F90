module fy_ErrorCodes
  implicit none
  public

  enum, bind(c)
     enumerator :: NONSPECIFIC_ERROR = -1
     enumerator :: SUCCESS = 0
     enumerator :: UNEXPECTED_CHARACTER
     enumerator :: UNEXPECTED_COLON_IN_PLAIN_SCALAR
     enumerator :: UNEXPECTED_MAPPING_KEY
     enumerator :: END_OF_STREAM_INSIDE_QUOTES
     enumerator :: UNEXPECTED_DOCUMENT_SEPARATOR
     enumerator :: UNKNOWN_ESCAPE_CHARACTER_IN_DOUBLE_QUOTED_SCALAR
     enumerator :: MISSING_COLON_WHILE_SCANNING_A_SIMPLE_KEY
     enumerator :: ILLEGAL_VALUE_IN_MAPPING
     enumerator :: ILLEGAL_SEQUENCE_ENTRY
     enumerator :: PARSER_ERROR
  end enum
  
end module fy_ErrorCodes
