add_executable (simple.x simple.F90)
target_link_libraries (simple.x yafyaml gftl-shared)

add_executable (simple2.x simple2.F90)
target_link_libraries (simple2.x yafyaml gftl-shared)
add_dependencies (examples simple2.x)

add_dependencies (examples simple.x simple2.x)

FILE (COPY simple.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
FILE (COPY simple2.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
FILE (COPY foo.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_executable (foo.x foo.F90)
target_link_libraries (foo.x yafyaml gftl-shared)
add_dependencies (examples foo.x)
