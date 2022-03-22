add_library (yafyaml
  analyze.F90
  
  ArrayWrapper.F90
  AbstractFile.F90
  File.F90
  StringNodeMap.F90

  String.F90
  UnlimitedUtilities.F90
  Tokens.F90
  TokenVector.F90
  AbstractTextStream.F90
  TextStream.F90
  EscapedTextStream.F90
  FileStream.F90
  Reader.F90
  Lexer.F90
  Parser.F90
  AbstractSchema.F90
  FailsafeSchema.F90
  JSONSchema.F90
  CoreSchema.F90

  YAML_Node.F90
  YAML_Node_implementation.F90
  YAML_NodeIterator_implementation.F90
  KeywordEnforcer.F90
  ErrorCodes.F90
  SimpleKey.F90
  ErrorHandling.F90
  IntegerSimpleKeyMap.F90

  Nodes/AbstractNode.F90
  Nodes/BaseNode.F90
  Nodes/BaseNode_implementation.F90
  Nodes/SequenceNode.F90
  Nodes/SequenceNode_implementation.F90
  Nodes/MappingNode.F90
  Nodes/MappingNode_implementation.F90
  Nodes/BoolNode.F90
  Nodes/BoolNode_implementation.F90
  Nodes/IntNode.F90
  Nodes/IntNode_implementation.F90
  Nodes/FloatNode.F90
  Nodes/FloatNode_implementation.F90
  Nodes/StringNode.F90
  Nodes/StringNode_implementation.F90
  Nodes/Sequence.F90
  Nodes/Mapping.F90
  Nodes/Nodes.F90
  # package
  yafyaml.F90
  )

add_library(YAFYAML::yafyaml ALIAS yafyaml)

set_target_properties (yafyaml PROPERTIES Fortran_MODULE_DIRECTORY ${YAFYAML_BINARY_DIR}/include)
target_link_libraries(yafyaml GFTL::gftl-v2 GFTL_SHARED::gftl-shared-v1 GFTL_SHARED::gftl-shared-v2)
target_include_directories(yafyaml PRIVATE ${YAFYAML_SOURCE_DIR}/include)

set (dest "YAFYAML-${YAFYAML_VERSION_MAJOR}.${YAFYAML_VERSION_MINOR}")

target_include_directories(yafyaml PUBLIC
  $<BUILD_INTERFACE:${YAFYAML_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:${dest}/include>
)  

add_executable(to_json.x to_json.F90)
target_link_libraries(to_json.x PRIVATE yafyaml)


install (TARGETS yafyaml EXPORT YAFYAML DESTINATION ${dest}/lib)
install (DIRECTORY  ${PROJECT_BINARY_DIR}/include/ DESTINATION ${dest}/include)
install (TARGETS to_json.x DESTINATION ${dest}/bin)


