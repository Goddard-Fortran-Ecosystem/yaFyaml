# ChangeLog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
