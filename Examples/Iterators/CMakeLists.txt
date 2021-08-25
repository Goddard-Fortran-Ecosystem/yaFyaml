add_executable (iterator.x iterator.F90)
target_link_libraries (iterator.x yafyaml GFTL::gftl-v2)
add_dependencies (examples iterator.x)

FILE (COPY iterator.yaml DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

