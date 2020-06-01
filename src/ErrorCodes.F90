module fy_ErrorCodes
  implicit none
  public

  enum, bind(c)
     enumerator :: SUCCESS = 0
     ! Parsing errors
     enumerator :: NONSPECIFIC_ERROR
     enumerator :: UNEXPECTED_CHARACTER
     enumerator :: UNEXPECTED_COLON_IN_PLAIN_SCALAR
     enumerator :: UNEXPECTED_MAPPING_KEY
     enumerator :: END_OF_STREAM_INSIDE_QUOTES
     enumerator :: UNEXPECTED_DOCUMENT_SEPARATOR
     enumerator :: UNKNOWN_ESCAPE_CHARACTER_IN_DOUBLE_QUOTED_SCALAR
     enumerator :: MISSING_COLON_WHILE_SCANNING_A_SIMPLE_KEY
     enumerator :: ILLEGAL_VALUE_IN_MAPPING
     enumerator :: ILLEGAL_SEQUENCE_ENTRY
     enumerator :: IMPOSSIBLE_COMBINATION
     enumerator :: PARSER_ERROR
     enumerator :: NON_ALPHANUMERIC_CHARACTER
     ! Config use errors
     enumerator :: INCONSISTENT_TYPE
     enumerator :: KEY_NOT_FOUND
     enumerator :: SELECTOR_NOT_FOUND
     enumerator :: ATTEMPTED_MAP_ACCESS_ON_NONMAP_NODE
     enumerator :: ATTEMPTED_VECTOR_ACCESS_ON_NONVECTOR_NODE

  end enum

contains

   function error_message(code) result(message)
      character(:), allocatable :: message
      integer, intent(in) :: code


      select case (code)
      case (SUCCESS)
         message = 'SUCCESS'
      case (NONSPECIFIC_ERROR)
         message = "Nonspecific failure while parsing."
      case (UNEXPECTED_CHARACTER)
         message = "While lexing for the next token, found character that cannot start any token."
      case (UNEXPECTED_COLON_IN_PLAIN_SCALAR)
         message = "Found unexpected ':' while lexing a plain scalar."
      case (UNEXPECTED_MAPPING_KEY)
         message = "Mapping keys not allowed here."
      case (END_OF_STREAM_INSIDE_QUOTES)
         message = "End of stream while lexing a quoted scalar."
      case (UNEXPECTED_DOCUMENT_SEPARATOR)
         message = "Found document separator while scanning a quoted scalar."
      case (UNKNOWN_ESCAPE_CHARACTER_IN_DOUBLE_QUOTED_SCALAR)
         message = "Found unknown escape character while scanning a double quoted scalar."
      case (MISSING_COLON_WHILE_SCANNING_A_SIMPLE_KEY)
         message = "Did not find expected ':' while scanning a simple key."
      case (ILLEGAL_VALUE_IN_MAPPING)
         message = "Mapping values not allowed here."
      case (ILLEGAL_SEQUENCE_ENTRY)
         message = "Sequence entries not allowed here."
      case (IMPOSSIBLE_COMBINATION)
         message = "Impossible situaition in lexer."
      case (PARSER_ERROR)
         message = "Error during parsing."
      case (NON_ALPHANUMERIC_CHARACTER)
         message = "Found nonalphanumeric character in anchor."
      case (INCONSISTENT_TYPE)
         message = 'Type of request does not match type in config.'
      case (KEY_NOT_FOUND)
         message = 'Requested key is not present in config.'
      case (SELECTOR_NOT_FOUND)
         message = 'Selector is not an entry in config.'
      case (ATTEMPTED_MAP_ACCESS_ON_NONMAP_NODE)
         message = 'Cannot use a string key to access a config vector.'
      case (ATTEMPTED_VECTOR_ACCESS_ON_NONVECTOR_NODE)
         message = 'Cannot use an integer to access a config map.'
      case default
         message = 'Unkown error code'
      end select

   end function error_message
      
end module fy_ErrorCodes
