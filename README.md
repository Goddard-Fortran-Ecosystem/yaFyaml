# yaFyaml: yet another Fortran (implementation of) YAML


There is at least one other open source Fortran-based YAML parser.  

The rationale for this one is simply to be compatible with the containers in gFTL.   It is not intended to be a complete YAML parser, just the subset needed by my own projects.    (Happy to accept additional contributed YAML capabilities, of course!)

Requirements:
  - [gFTL 1.5.0](https://github.com/Goddard-Fortran-Ecosystem/gFTL) or later
  - [gFTL-shared 1.3.0](https://github.com/Goddard-Fortran-Ecosystem/gFTL-shared) or later

Supported Fortran compilers:
  - ifort 2021.3.0
  - gfortran 8.5, 9.4, 10.3, 11.2
  - NAG 7.0 (7057)
