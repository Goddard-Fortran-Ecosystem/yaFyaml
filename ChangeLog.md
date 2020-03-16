# ChangeLog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
## Changed
## Added
## Fixed

## [0.3.0] - 2020-4-06

## Fixed
 - varous missing checks on return status
 - eliminated some debug print statements

## [0.2.2] - 2020-03-16

## Fixed

- Bugfix for #include.   GNU CPP requires double quotes for include files.

## [0.2.1] - 2020-03-13

## Added

- Added examples - apparently failed to commit thes previously.  Build
  with 'make Examples', and go to the build directory to run.


## [0.2.0] - 2020-03-11

### Added

- New interfaces for Configuration::get()
  . allow for default values and testing if present
  . overload for intrinsics:  integer, logical, real, deferred length char
- Introduced iterator to loop over elements at a single level
  . still requires SELECT TYPE as iterator must use CLASS(*)
- Improved error handling throughout
  . still needs more work.

### Fixed

- Some errors in lexing were exposed with pFlogger use cases.


## [0.1.0] - 2020-02-07
	
	
### Changed
- Initial implementation and unit tests
- Supports basic yaml subset.  Exceptions include variant multiline strings, anchors, etc.
- "Works" with ifort 19.0.5, NAG 6.2, and gfortran 9.2
- Created some simple examples.  (Still needs real documentation.)
