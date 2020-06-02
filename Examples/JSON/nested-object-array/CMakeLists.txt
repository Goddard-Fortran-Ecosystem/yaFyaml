add_executable (test-nested-object-array test-nested-object-array.f90)

target_link_libraries (test-nested-object-array
  PRIVATE yafyaml
)

file(COPY nested-object-array.json DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
