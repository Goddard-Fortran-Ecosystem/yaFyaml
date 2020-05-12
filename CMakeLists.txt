cmake_minimum_required(VERSION 3.8)

project (YAFYAML
  VERSION 0.3.2
  LANGUAGES Fortran)

# Most users of this software do not (should not?) have permissions to
# install in the cmake default of /usr/local (or equiv on other os's).
# Below, the default is changed to a directory within the build tree
# unless the user explicitly sets CMAKE_INSTALL_PREFIX in the cache.
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/installed" CACHE PATH "default install path" FORCE )
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake")
include(${CMAKE_Fortran_COMPILER_ID} RESULT_VARIABLE found)
if(NOT found)
  message( FATAL_ERROR "Unrecognized Fortran compiler. Please use ifort, gfortran, NAG, PGI, or XL.")
endif()

set(CMAKE_POSITION_INDEPENDENT_CODE ON)

find_package (GFTL)
find_package (GFTL_SHARED)
find_package (PFUNIT 4.1 QUIET)

add_subdirectory (src)

#if (examples)
#  add_subdirectory(Examples)
#endif()

if (PFUNIT_FOUND)
  set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${GFTL_SOURCE_DIR}/cmake_utils")
  include (${CMAKE_Fortran_COMPILER_ID} RESULT_VARIABLE found)

  enable_testing()
  if (NOT TARGET (tests))
    add_custom_target(tests COMMAND ${CMAKE_CTEST_COMMAND})
  endif ()
  
  add_subdirectory(tests)
endif ()

if (NOT TARGET (examples))
  add_custom_target(examples COMMAND ${CMAKE_CTEST_COMMAND})
endif ()
add_subdirectory (Examples EXCLUDE_FROM_ALL)

# find_package() support
configure_file (YAFYAMLConfig-version.cmake.in ${PROJECT_BINARY_DIR}/YAFYAMLConfig-version.cmake @ONLY)
configure_file (YAFYAMLConfig.cmake.in ${PROJECT_BINARY_DIR}/YAFYAMLConfig.cmake @ONLY)

set (top_dir YAFYAML-${YAFYAML_VERSION_MAJOR}.${YAFYAML_VERSION_MINOR})
install (
  FILES ${PROJECT_BINARY_DIR}/YAFYAMLConfig.cmake ${PROJECT_BINARY_DIR}/YAFYAMLConfig-version.cmake
  DESTINATION "${top_dir}/cmake")

# The following is needed for external projects using *nix make when
# parent project builds yafyaml as a subproject.
set (YAFYAML_TOP_DIR "${CMAKE_INSTALL_PREFIX}/${top_dir}" CACHE PATH "")
configure_file (YAFYAML.mk.in ${PROJECT_BINARY_DIR}/YAFYAML.mk @ONLY)
#install (
#  FILES ${CMAKE_CURRENT_BINARY_DIR}/YAFYAML.mk ${CMAKE_CURRENT_BINARY_DIR}/YAFYAML.mk
#  DESTINATION "${top_dir}/include")

#install (EXPORT YAFYAML DESTINATION "${top_dir}/cmake")

