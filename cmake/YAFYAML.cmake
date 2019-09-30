#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Export all exported targets to the build tree for use by parent project
#-----------------------------------------------------------------------------
export (
  TARGETS ${yaFyaml_LIBRARIES_TO_EXPORT}
  FILE ${yaFyaml_PACKAGE}-targets.cmake
  NAMESPACE ${yaFyaml_PACKAGE}::
  )

  
