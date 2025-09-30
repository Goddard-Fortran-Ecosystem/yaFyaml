# yaFyaml: yet another Fortran (implementation of) YAML


There is at least one other open source Fortran-based YAML parser.  

The rationale for this one is simply to be compatible with the containers in gFTL.   It is not intended to be a complete YAML parser, just the subset needed by my own projects.    (Happy to accept additional contributed YAML capabilities, of course!)

Requirements:
  - [gFTL 1.16.0](https://github.com/Goddard-Fortran-Ecosystem/gFTL) or later
  - [gFTL-shared 1.11.0](https://github.com/Goddard-Fortran-Ecosystem/gFTL-shared) or later

Supported Fortran compilers:
  - ifort 2021.6.0, 2021.13.0
  - ifx 2025.2.0
  - gfortran 12+
  - NAG 7.2
