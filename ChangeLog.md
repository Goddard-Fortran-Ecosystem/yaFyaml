# ChangeLog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added `IntelLLVM.cmake` file as a copy of `Intel.cmake` to support the LLVM Intel compiler frontends

## [1.0.4] 2022-06-30

### Fixed

- Fixed bug in Parser.F90 for using alias in a sequence.  Isolating test added to test suite.

## [1.0.3] 2022-06-30

### Fixed
- Removed all stop statements and replaced with proper error handling and return codes
- Fixed schemas "to" calls to produce INT64 and REAL64 rather than plain integer and real
- Implemented workarounds for GNU in some unit tests

## [1.0.2] 2022-06-01

### Fixed

- Small problem in CMake logic exposed in some combinations with other GFE projects.

## [1.0.1] 2022-05-31

### Fixed

- Found various workarounds to issues with NAG 7.1 (7110) compiler.

## Changed

- Updated GitHub Actions
  - OSs
    - Remove macos-10.15
    - Add ubuntu-22.04 and macos-12
  - Compilers
   - Removed gfortran-8
   - Added gfortran-11
   - Added gfortran-12 (for ubuntu-22.04)


## [1.0.0] 2022-05-08

### Fixed

- Found various/improved workarounds for various compiler issues.   Most problems were with GFortran (11.2).  
  1. Redemontrated that intrinsic FINAL does not work for map containers - needs user code to add RECURSIVE.
  2. Improved workaround for Parser.  GFortran recursion was doing very weird things with local polymorphic allocatable variables.  Previous workaround was a global stack, but this iteration found that a simple wrapper type suffices to workaround the issue.   Various other compiler workarounds from earlier releases could then be removed - mostly this was unnecessary explicit deep-copies in containers (except when definitely required by Fortran (e.g., Set containers).

## [1.0-beta8] 2022-04-08


### Fixed

- Error in CMake check for the target "test" that prevents yaFyaml
  from being bundled with other GFE packages.
  
## [1.0-beta7] 2022-04-08


### Changed

- YAML_Node is now an abstract type.  Semantics are slightly changed
  as a consequence, but most interfaces are unaffected.
  Main object should now be declared as `class(YAML_Node), allocatable ::`
  
- Iteration on MappingNode and SequenceNode objects is now done with abstract NodeIterator.
  The concrete subclasses: MappingNodeIterator SequenceNodeIterator
  have methods for both vector and map iterators but return an optional error if used in the
  wrong case. Also they produce null() pointer results if used in the wrong case.
  
### Added

- Test for reproducer of ifort issue encountered in pFlogger upstream.
	  


## [1.0-beta6] 2022-03-24

### Added

- Interfaces for config setters
  `call node%set(<value>, SELECTORS, err_msg, rc)`
  Where <value> can be:
      . integer (32/64 bit), real (32/64 bit), logical or string.
      . scalar or 1D array
  
- Add `NVHPC.cmake` and `PGI.cmake` files for NVHPC support (requires nvfortran 22.3)
	  
- A new interface to initialize `YAML_node` (formerly `Configuration`)
  objects.  This is now the interface that associates the internal
  pointer with the target argument.  Previously was the constructor.

### Changed

- The derived type `Configuration` has been renamed to `YAML_Node`.  A
  temporary workaround is provided to users to allow the older name to
  be used, but it is deprecated and will go away whev V2.0.0 is
  formally released.
  
- `YAML_Node` constructor now _copies_ the argument.  Before it only
  associated pointer with target.

## [1.0-beta5] 2022-03-08

### Added

- New interface for Parser::load() that allows user to just specify a file name and not require them to separately construct a FileStream from the filename.

### Changed

- Added gfortran-8 and macos-11 to CI

## [1.0-beta2]

### Added

- Introduced new overload for 'Configuration::get()' which returns a
  subconfig.
- Introduced ConfigurationIterator to reduced difficulty in accessing
  string keys.  Ended up with fairly general overloads of `get_key()`
  and `get_value()` which are analogus to `get()`.
  
### Changed

- Introduced Fortran submodule for Configuration.  Code was already long and now need to introduce
  an iterator class in the same namespace.
  
### Fixed

- Eliminated co-array reference in JSON examples
  
## [1.0-beta]

- The interfaces to configuration have been considerably modified to
  better support some advanced use cases.  Existing code that uses
  yaFyaml will generally require some changes See Examples directories
  for defacto documentation.

- Tested with Intel 2021, gfortran 8.5, 9.4, 10.3, 11.2

## [0.5.1] - 2021-04-01

### Fixed

- Ability to embed yaFyaml alongside the other GFE libraries in a superproject.

### Added

- Allow `get()` to have 0 selectors.

## [0.5.0] - 2021-03-15

### Changed

## Added

- Mappings with non-string keys (including complex keys) now supported.

## Changed

- Introduced new interfaces, thought very similar to before.  Most routines now also
  can provide an optional error message.
- Adopted CMake namespaces for package integration/management

## [0.4.2] - 2020-12-07

### Fixed

- Incorrect handling of rc argument in `Configuration::at()`.  The
  primary results were correct but the status was not filled.
  


## [0.4.1] - 2020-08-25

### Fixed
- Bug fixed when value of key value pair starts with the dash character.
  The check to ensure the next char was not whitespace was incorrectly
  implemented.   Reproducer added to test suite.
- Introduced workaround for gfortran-10 that was causing all sorts of 
  memory corruption.  (Ugly pervasive manual deallocation of polymorphic
  objects.)



## [0.4.0] - 2020-06-28
### Added
- Support for simple anchor/alias use cases.

- Simple driver "to_json.x" that takes a YAML file name argument on the
  command line, parses the file, and dumps JSON formet to OUTPUT_UNIT.
  E.g., 
  ```./to_json.x test.yaml >  test.json```

### Fixed
- Improved formatting of JSON output.


## [0.3.5] - 2020-06-05

### Fixed
- Mistake in push for 0.3.4 including logic for uncommitted files.

## [0.3.4] - 2020-06-05

### Fixed
- Quoted strings that contained non-string values (e.g., "2004") were
  being intrepreted as non-strings.
  
### Added
- Additional examples in Examples/JSON folder.

### Added
- Support for simple anchor/alias use cases.
  No merges, just vanilly copy-in-place.

- Simple driver "to_json.x" that takes a YAML file name argument on the
  command line, parses the file, and dumps JSON formet to OUTPUT_UNIT.
  E.g., 
  ```./to_json.x test.yaml >  test.json```

### Fixed
- Improved formatting of JSON output.
	
## [0.3.3] - 2020-05-16

### Fixed
- adapting to recent minor interface change in gFTL-shared
  Derived type "Pair" is now given a less generic name.

## [0.3.2] - 2020-05-12

### Changed

- Enabled Position Independent Code

### Added

- New example Serial2.F90

### Fixed
	
## [0.3.1] - 2020-4-13

- workarounds for gFortran

## [0.3.0] - 2020-04-06

### Fixed

- various missing checks on return status
- eliminated some debug print statements

## [0.2.2] - 2020-03-16

### Fixed

- Bugfix for #include.   GNU CPP requires double quotes for include files.

## [0.2.1] - 2020-03-13

### Added

- Added examples - apparently failed to commit thes previously.  Build
  with 'make Examples', and go to the build directory to run.

## [0.2.0] - 2020-03-11

### Added

- New interfaces for Configuration::get()
  - allow for default values and testing if present
  - overload for intrinsics:  integer, logical, real, deferred length char
- Introduced iterator to loop over elements at a single level
  - still requires SELECT TYPE as iterator must use CLASS(*)
- Improved error handling throughout
  - still needs more work.

### Fixed

- Some errors in lexing were exposed with pFlogger use cases.

## [0.1.0] - 2020-02-07
	
### Changed

- Initial implementation and unit tests
- Supports basic yaml subset.  Exceptions include variant multiline strings, anchors, etc.
- "Works" with ifort 19.0.5, NAG 6.2, and gfortran 9.2
- Created some simple examples.  (Still needs real documentation.)
