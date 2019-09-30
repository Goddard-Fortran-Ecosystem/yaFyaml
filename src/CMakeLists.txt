add_library (yafyaml
  ArrayWrapper.F90
  AbstractFile.F90
  File.F90
  Configuration.F90
  KeywordEnforcer.F90
  YAML_Parser.F90
  # package
  yafyaml.F90
  )

set_target_properties (yafyaml PROPERTIES Fortran_MODULE_DIRECTORY ${YAFYAML_BINARY_DIR}/include)
target_link_libraries(yafyaml gftl-shared)

set (dest "FARGPARSE-${FARGPARSE_VERSION_MAJOR}.${FARGPARSE_VERSION_MINOR}")

target_include_directories(yafyaml PUBLIC
  $<BUILD_INTERFACE:${YAFYAML_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:${dest}/include>
)  
