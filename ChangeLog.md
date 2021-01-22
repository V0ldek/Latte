# Changelog for Latte

## 0.8

- Added the frontend including the Lexer, Parser, Rewriter and Analyser.

## 0.9

- Added the intermediate representation language, Espresso, with its own lexer and parser.
- Added IR generation.
- Added Control Flow Graph and variable liveness analysis.
- Added x86_64 assembler code generation for the core of the language.

## 0.9.1

- Fixed an issue where locals were unnecessarily moved out of stack and back again at end of a basic block when they were not alived in the preceding block.

## 0.10.0

- Added structs.
- Added arrays.
- Added objects with inheritance and virtual methods.

## 1.0.0

Release version.

- Added SSA transformation.
- Added GCSE and other Espresso optimisations.
- Added graph colouring based global register allocation.