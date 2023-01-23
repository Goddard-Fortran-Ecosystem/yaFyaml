cmake_minimum_required(VERSION 3.12)

project (YAFYAML
  VERSION 1.0.5
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

if (NOT TARGET GFTL::gftl)
  find_package (GFTL REQUIRED VERSION 1.7.2)
endif ()

if (NOT TARGET GFTL_SHARED::gftl-shared)
  find_package (GFTL_SHARED REQUIRED VERSION 1.4.1)
endif ()
find_package (PFUNIT 4.2 QUIET)

add_subdirectory (src)

enable_testing()

if (NOT TARGET tests)
  add_custom_target(tests
    COMMAND ${CMAKE_CTEST_COMMAND}
    EXCLUDE_FROM_ALL)
endif ()
if (NOT TARGET build-tests)
  add_custom_target(build-tests)
  add_dependencies(tests build-tests)
endif ()

if (PFUNIT_FOUND)
  set (CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${GFTL_SOURCE_DIR}/cmake_utils")
  include (${CMAKE_Fortran_COMPILER_ID} RESULT_VARIABLE found)
  add_subdirectory (tests EXCLUDE_FROM_ALL)
endif ()

# The following forces tests to be built when using "make ctest" even if some targets
# are EXCLUDE_FROM_ALL
# From https://stackoverflow.com/questions/733475/cmake-ctest-make-test-doesnt-build-tests/56448477#56448477
build_command(CTEST_CUSTOM_PRE_TEST TARGET build-tests)
string(CONFIGURE \"@CTEST_CUSTOM_PRE_TEST@\" CTEST_CUSTOM_PRE_TEST_QUOTED ESCAPE_QUOTES)
file(WRITE "${CMAKE_BINARY_DIR}/CTestCustom.cmake" "set(CTEST_CUSTOM_PRE_TEST ${CTEST_CUSTOM_PRE_TEST_QUOTED})" "\n")

if (NOT TARGET (examples))
  add_custom_target(examples
    COMMAND ${CMAKE_CTEST_COMMAND}
    EXCLUDE_FROM_ALL)
endif ()

add_subdirectory (Examples EXCLUDE_FROM_ALL)

# The following is needed for external projects using *nix make when
# parent project builds gFTL-shared as a subproject.
set (top_dir YAFYAML-${YAFYAML_VERSION_MAJOR}.${YAFYAML_VERSION_MINOR})
set (YAFYAML_TOP_DIR "${CMAKE_INSTALL_PREFIX}/${top_dir}" CACHE PATH "")

include(CMakePackageConfigHelpers)
configure_package_config_file(YAFYAMLConfig.cmake.in YAFYAMLConfig.cmake
  INSTALL_DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/YAYFAMLConfig.cmake
  )
write_basic_package_version_file(YAFYAMLConfig-version.cmake
  VERSION ${PACKAGE_VERSION}
  COMPATIBILITY SameMajorVersion
  )

install (
  FILES ${PROJECT_BINARY_DIR}/YAFYAMLConfig.cmake ${PROJECT_BINARY_DIR}/YAFYAMLConfig-version.cmake
  DESTINATION "${top_dir}/cmake")

configure_file (YAFYAML.mk.in ${PROJECT_BINARY_DIR}/YAFYAML.mk @ONLY)
install (
  FILES ${CMAKE_CURRENT_BINARY_DIR}/YAFYAML.mk ${CMAKE_CURRENT_BINARY_DIR}/YAFYAML.mk
  DESTINATION "${top_dir}/include")


install (EXPORT YAFYAML
  FILE YAFYAMLTargets.cmake
  NAMESPACE YAFYAML::
  DESTINATION "${top_dir}/cmake")

export(EXPORT YAFYAML
  FILE "${CMAKE_CURRENT_BINARY_DIR}/YAFYAMLTargets.cmake"
  NAMESPACE YAFYAML::
  )

