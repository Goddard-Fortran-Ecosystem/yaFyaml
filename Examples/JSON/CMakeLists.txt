get_filename_component(barename ${CMAKE_Fortran_COMPILER} NAME)

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -std=f2018")
  if("${barename}" MATCHES gfortran)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcoarray=single")
  endif()
endif()

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
