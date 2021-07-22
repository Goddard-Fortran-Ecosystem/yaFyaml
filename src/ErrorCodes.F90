module fy_ErrorCodes
  implicit none
  public

  enum, bind(c)
     enumerator :: YAFYAML_SUCCESS = 0
     ! Parsing errors
     enumerator :: YAFYAML_NONSPECIFIC_ERROR
     enumerator :: YAFYAML_UNEXPECTED_CHARACTER
     enumerator :: YAFYAML_UNEXPECTED_COLON_IN_PLAIN_SCALAR
     enumerator :: YAFYAML_UNEXPECTED_MAPPING_KEY
     enumerator :: YAFYAML_END_OF_STREAM_INSIDE_QUOTES
     enumerator :: YAFYAML_UNEXPECTED_DOCUMENT_SEPARATOR
     enumerator :: YAFYAML_UNKNOWN_ESCAPE_CHARACTER_IN_DOUBLE_QUOTED_SCALAR
     enumerator :: YAFYAML_MISSING_COLON_WHILE_SCANNING_A_SIMPLE_KEY
     enumerator :: YAFYAML_ILLEGAL_VALUE_IN_MAPPING
     enumerator :: YAFYAML_ILLEGAL_SEQUENCE_ENTRY
     enumerator :: YAFYAML_IMPOSSIBLE_COMBINATION
     enumerator :: YAFYAML_PARSER_ERROR
     enumerator :: YAFYAML_NON_ALPHANUMERIC_CHARACTER
     ! Configuration usage errors
     enumerator :: YAFYAML_SELECTOR_NOT_FOUND
     enumerator :: YAFYAML_TYPE_MISMATCH
     enumerator :: YAFYAML_INVALID_SEQUENCE_INDEX
     enumerator :: YAFYAML_SEQUENCE_INDEX_OUT_OF_BOUNDS
     enumerator :: YAFYAML_INVALID_MAPPING_KEY
     enumerator :: YAFYAML_MAPPING_KEY_NOT_FOUND
     enumerator :: YAFYAML_NOT_A_COLLECTION
     enumerator :: YAFYAML_NOT_A_MAPPING

  end enum

contains

   function error_message(code) result(message)
      character(:), allocatable :: message
      integer, intent(in) :: code


      select case (code)
      case (YAFYAML_SUCCESS)
         message = 'SUCCESS'
      case (YAFYAML_NONSPECIFIC_ERROR)
         message = "Nonspecific failure while parsing."
      case (YAFYAML_UNEXPECTED_CHARACTER)
         message = "While lexing for the next token, found character that cannot start any token."
      case (YAFYAML_UNEXPECTED_COLON_IN_PLAIN_SCALAR)
         message = "Found unexpected ':' while lexing a plain scalar."
      case (YAFYAML_UNEXPECTED_MAPPING_KEY)
         message = "Mapping keys not allowed here."
      case (YAFYAML_END_OF_STREAM_INSIDE_QUOTES)
         message = "End of stream while lexing a quoted scalar."
      case (YAFYAML_UNEXPECTED_DOCUMENT_SEPARATOR)
         message = "Found document separator while scanning a quoted scalar."
      case (YAFYAML_UNKNOWN_ESCAPE_CHARACTER_IN_DOUBLE_QUOTED_SCALAR)
         message = "Found unknown escape character while scanning a double quoted scalar."
      case (YAFYAML_MISSING_COLON_WHILE_SCANNING_A_SIMPLE_KEY)
         message = "Did not find expected ':' while scanning a simple key."
      case (YAFYAML_ILLEGAL_VALUE_IN_MAPPING)
         message = "Mapping values not allowed here."
      case (YAFYAML_ILLEGAL_SEQUENCE_ENTRY)
         message = "Sequence entries not allowed here."
      case (YAFYAML_IMPOSSIBLE_COMBINATION)
         message = "Impossible situaition in lexer."
      case (YAFYAML_PARSER_ERROR)
         message = "Error during parsing."
      case (YAFYAML_NON_ALPHANUMERIC_CHARACTER)
         message = "Found nonalphanumeric character in anchor."

      case (YAFYAML_SELECTOR_NOT_FOUND)
         message = 'Selected item does not exist and no default value is provided.'
      case (YAFYAML_TYPE_MISMATCH)
         message = 'Type of request does not match type in config.'
      case (YAFYAML_INVALID_SEQUENCE_INDEX)
         message = 'Selector applied to a sequence node must be an integer.'
      case (YAFYAML_SEQUENCE_INDEX_OUT_OF_BOUNDS)
         message = 'Sequence index is out of bounds.'
      case (YAFYAML_INVALID_MAPPING_KEY)
         message = 'Selector applied to a mapping node must be an one of {integer, real, logical, string}.'
      case (YAFYAML_MAPPING_KEY_NOT_FOUND)
         message = 'Selector applied to a mapping node did not match any key.'
      case (YAFYAML_NOT_A_COLLECTION)
         message = 'Selector applied to a non-collection.'
      case (YAFYAML_NOT_A_MAPPING)
         message = 'Can only construct iterator for mappings.'
      case default
         message = 'Unkown error code'
      end select

   end function error_message
      
end module fy_ErrorCodes
