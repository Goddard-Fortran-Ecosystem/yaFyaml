# ChangeLog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## Added

- New interfaces for Configuration::get()
  . allow for default values and testing if present
  . overload for each type; just scalars for now
- Improved error handling throughout
  . still needs more work.


## [0.1.0] - 2020-02-07
	
	
### Changed
- Initial implementation and unit tests
- Supports basic yaml subset.  Exceptions include variant multiline strings, anchors, etc.
- "Works" with ifort 19.0.5, NAG 6.2, and gfortran 9.2
- Created some simple examples.  (Still needs real documentation.)