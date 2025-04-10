# F-Scheme

Implementation of a LISP interpreter written in Haskell.

Based on the tutorial [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

## Executing

Clone the repository, traverse to the root folder and once in there type `cabal run f-scheme`.

You must naturally have the Haskell Cabal package manager installed.

## Implementation status

As of February 27th of 2023, the implementation status can be resumed as follows:

### Data types

- [x] Atoms
- [x] Booleans
- [x] Of course, lists, dotted ones included
- [x] Integers (decimal, octary, binary and hexadecimal)
- [x] Strings
- [x] Characters
- [ ] Floats
- [ ] Arrays
- [ ] Vectors

### General features

- [x] LISP primitives (car, cdr, cons, etc.)
- [x] REPL
- [x] Conditionals (cond)
- [x] Backquoting
- [ ] Macros
- [ ] Standard library
- [ ] Comprenhensive documentation
