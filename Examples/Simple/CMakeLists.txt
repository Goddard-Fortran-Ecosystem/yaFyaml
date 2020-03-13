add_executable (simple.x simple.F90)
target_link_libraries (simple.x yafyaml gftl-shared)
add_dependencies (examples simple.x)

FILE (COPY simple.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

