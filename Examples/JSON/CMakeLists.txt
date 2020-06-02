get_filename_component(barename ${CMAKE_Fortran_COMPILER} NAME)

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -std=f2018")
  if("${barename}" MATCHES gfortran)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcoarray=single")
  endif()
endif()

foreach(test_name
  trivial
  integer-array
  nested-object-array
)
  add_subdirectory("${test_name}")
  if(barename MATCHES caf)
    set(launch_command "cafrun" "-n" "2")
  else()
    set(launch_command "")
  endif()

  add_test(NAME "${test_name}"
    COMMAND ${launch_command} "$<TARGET_FILE:test-${test_name}>"
    WORKING_DIRECTORY "${test_name}"
  )
  set_tests_properties(${test_name}
     PROPERTIES PASS_REGULAR_EXPRESSION "Test passed."
  )
endforeach()
