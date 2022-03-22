module yafyaml
  use fy_YAML_Node, only: YAML_Node
  use fy_YAML_Node, only: YAML_NodeIterator
!!$  use fy_YAML_Node, only: Configuration => YAML_Node
!!$  use fy_YAML_Node, only: ConfigurationIterator => YAML_NodeIterator
  use fy_Parser
  use fy_ErrorCodes
  use fy_Nodes
  use fy_String
  use fy_File
  use fy_FileStream
  use fy_AbstractTextStream
  use fy_TextStream
  use fy_EscapedTextStream

  ! These should be private
!!$  use fy_Abstractfile
!!$  use fy_Tokens
!!$  use fy_Reader
!!$  use fy_Lexer
end module yafyaml
