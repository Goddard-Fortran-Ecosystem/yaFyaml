add_library (yafyaml
  OrderedStringUnlimitedMap.F90
  ArrayWrapper.F90
  AbstractFile.F90
  File.F90
  StringUnlimitedPointerMap.F90

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
  None.F90

  Configuration.F90
  KeywordEnforcer.F90
  ErrorCodes.F90
  SimpleKey.F90
  ErrorHandling.F90
  IntegerSimpleKeyMap.F90
  # package
  yafyaml.F90
  )

add_library(YAFYAML::yafyaml ALIAS yafyaml)

set_target_properties (yafyaml PROPERTIES Fortran_MODULE_DIRECTORY ${YAFYAML_BINARY_DIR}/include)
target_link_libraries(yafyaml GFTL_SHARED::gftl-shared)

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


