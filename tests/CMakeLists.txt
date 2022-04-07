set (pf_tests
  Test_FileStream.pf
  Test_File.pf
  Test_Reader.pf
  Test_Lexer.pf
  Test_newParser.pf
  Test_JSONSchema.pf
  Test_CoreSchema.pf
  Test_BoolNode.pf
  Test_IntNode.pf
  Test_StringNode.pf
  Test_FloatNode.pf
  Test_SequenceNode.pf
  Test_MappingNode.pf
  Test_ComplexNode.pf
  Test_Node_Set.pf
  Test_Iterators.pf
  )


add_pfunit_ctest (yafyaml_tests.x
  TEST_SOURCES ${pf_tests}
  OTHER_SOURCES MockFile.F90
  LINK_LIBRARIES yafyaml
  OTHER_SOURCES    InitializeTests.F90
  EXTRA_INITIALIZE Initialize
  EXTRA_USE        InitializeTests_mod
  )

set_target_properties (yafyaml_tests.x PROPERTIES EXCLUDE_FROM_ALL True)

add_dependencies (build-tests yafyaml_tests.x)

add_executable(demo demo.F90)
target_link_libraries(demo PRIVATE yafyaml)

add_executable(is_int.x is_int.F90)
target_link_libraries(is_int.x yafyaml)

add_executable(standalone.x standalone.F90)
target_link_libraries(standalone.x yafyaml PFUNIT::funit)
