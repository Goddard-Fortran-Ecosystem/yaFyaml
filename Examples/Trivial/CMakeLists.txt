add_executable (trivial.x trivial.F90)
target_link_libraries (trivial.x yafyaml)
add_dependencies (examples trivial.x)

FILE (COPY trivial.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

