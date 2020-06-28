foreach (test_name
    trivial
    integer-array
    nested-object-array
    )

  add_subdirectory("${test_name}")
  
  add_test(NAME "${test_name}"
    COMMAND "$<TARGET_FILE:test-${test_name}>"
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${test_name}
    )

  set_tests_properties(${test_name}
    PROPERTIES PASS_REGULAR_EXPRESSION "Test passed."
    )
  add_dependencies (build-tests test-${test_name})
endforeach ()
