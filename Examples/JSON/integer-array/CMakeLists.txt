add_executable (test-integer-array test-integer-array.f90)

target_link_libraries (test-integer-array
  PRIVATE yafyaml
)

file(COPY integer-array.json DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
