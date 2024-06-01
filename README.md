# SymChar

This package contains functions to compute the irreducible characters of symmetric groups.

## Usage

The library functions are located in `src/Lib.hs`, which contains code to completely compute the character table of symmetric groups. The individual characters may be computed using `chi`, which is based on the Murnaghan-Nakayama formula.

## Installation

To build this package, clone the repository and run `stack build`. The main executable is `symchar-exe`, which can be run using `stack exec symchar-exe`. Currently, the executable is set up to print the character table of S_n where n is read from stdin.
