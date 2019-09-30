set (pf_tests
  Test_Configuration_casting.pf
  Test_File.pf
  Test_YAML_Parser.pf
  )


add_pfunit_ctest (yafyaml_tests.x
  TEST_SOURCES ${pf_tests}
  OTHER_SOURCES MockFile.F90
  LINK_LIBRARIES yafyaml
  )

set_target_properties (yafyaml_tests.x PROPERTIES EXCLUDE_FROM_ALL True)

add_dependencies (tests yafyaml_tests.x)

