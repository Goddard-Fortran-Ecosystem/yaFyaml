set (pf_tests
  Test_Configuration_casting.pf
  Test_Configuration_dump.pf
  Test_Configuration_anchors.pf
  Test_FileStream.pf
  Test_File.pf
  Test_Reader.pf
  Test_Lexer.pf
  Test_Parser.pf
  Test_UnlimitedUtilities.pf
  Test_JSONSchema.pf
  Test_CoreSchema.pf
  )


add_pfunit_ctest (yafyaml_tests.x
  TEST_SOURCES ${pf_tests}
  OTHER_SOURCES MockFile.F90
  LINK_LIBRARIES yafyaml
  )

set_target_properties (yafyaml_tests.x PROPERTIES EXCLUDE_FROM_ALL True)

add_dependencies (build-tests yafyaml_tests.x)

add_executable(demo demo.F90)
target_link_libraries(demo PRIVATE yafyaml)

