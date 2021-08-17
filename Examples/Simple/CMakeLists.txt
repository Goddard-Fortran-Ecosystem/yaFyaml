add_executable (simple.x simple.F90)
target_link_libraries (simple.x yafyaml GFTL::gftl-v2)

add_executable (simple2.x simple2.F90)
target_link_libraries (simple2.x yafyaml GFTL::gftl-v2)
add_dependencies (examples simple2.x)

add_dependencies (examples simple.x simple2.x)

FILE (COPY simple.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
FILE (COPY simple2.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
