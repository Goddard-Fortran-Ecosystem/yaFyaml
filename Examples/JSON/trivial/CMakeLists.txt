add_executable (test-trivial test-trivial.f90)

target_link_libraries (test-trivial
  PRIVATE yafyaml
)

file(COPY trivial.json DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
