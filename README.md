# Latte v1.0.0

Compiler of the [Latte programming language](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/description.html) written in Haskell.

## Compiling the project

Use `stack build` to compile the project.

Used version of GHC is 8.8.4 (LTS 16.22). For the breakdown of used packages consult `package.yaml`.

## Running the project

Use the `latc_x86_64` executable to compile `.lat` source files. Additionally, `espi` executable containing an IR interpreter is generated - this is not intended for end users but rather as a development tool.

### Flags

- `-v` - enable verbose mode
- `-g` - generate intermediate steps

When running on a file `<p>.lat`, where `<p> = <dir_path/<file_path>` is some path:

1. when the flag `-g` is not specified, two files are created: `<p>.s` containing generated x86_64 assembly and an executable `<p>`;

2. when `-g` is specified the following intermediate representations are generated additionally:

- `<p>.esp`, `<p>.cfg` - Espresso intermediate code and text representation of its Control Flow Graph.
- `<p>.1.reach.esp`, `<p>.1.reach.cfg` - Espresso with unreachable blocks removed.
- `<p>.2.liv.esp`, `<p>.2.liv.cfg` - Espresso with initial liveness analysis.
- `<p>.3.ssa.esp`, `<p>.3.ssa.cfg` - Espresso converted to SSA form.
- `<p>.4.opt.esp`, `<p>.4.opt.cfg` - Espresso with optimisations applied.
- `<p>.5.liv.esp`, `<p>.5.liv.cfg` - Livenness analysis after optimisation.
- `<p>.6.regs.esp`, `<p>.6.regs.cfg`, `<p>.6.regs.ig` - Espresso after global register allocation with spilling code inserted. The `ig` file contains the generated interference graphs for all methods.
- `<p>.7.nophi.esp`, `<p>.7.nophi.cfg` - Espresso with phony `phi` functions eliminated.
- `<p>.8.opt.esp`, `<p>.8.opt.cfg` - Espresso after final optimisation pass.
- `<p>.9.final.esp`, `<p>.9.final.cfg` - Final version of Espresso used for codegen including final liveness analysis pass.
- `<p>.noopt.s` - Generated assembly before peephole optimisation phase.

## Testing the project

Use `stack test` to run all included tests. The `lattests` directory contains all tests provided on
the [assignment page](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/latte-en.html) (these are unchanged)
and additional custom tests. The `esptests` directory contains a handful of tests for the IR language Espresso.

There are two test suites, `Latte-test` and `Latte-test-exec`.

- `Latte-test` tests parsing, semantic analysis and IR code generation using an interpreter for the generated IR.
These are quick to run but do not test anything related to x86_64 assembly generation.

- `Latte-test-exec` tests the entire compiler by running the executable on all valid tests, asserting they compile and then running the generated executables. It creates a temporary work directory in `lattests` that gets cleaned up after the test.

## Features

- Full lexer and parser for Latte with objects, virtual methods and arrays.
- Full semantic analysis with type checks and reachability analysis.
- Internal IR language - Espresso - with a separate lexer/parser and a small interpreter.
- Compilation to Espresso and generation of additional annotations - Control Flow Graph and variable liveness analysis.
- Support for arrays of integers, booleans or references to objects.
- Support for objects with single inheritance and virtual methods.
- SSA transformation.
- Constant and copy propagation.
- Global Common Subexpression Elimination.
- Global register allocation based on interference graph colouring for chordal graphs.
- Spill code handled with simple cost heuristic.
- x86_64 assembly code generation.
- Peephole optimisations of the generated assembly to fix common trivial inefficiencies.

## Custom extensions

The grammar has been extended to include a `var` type declaration that can be used when declaring local variables to infer
their type to the compile-time type of the initialiser expression. It cannot be used in any other context and it cannot be
used in declarations without an initialiser. The motivation behind this is mainly so that `for` loop rewrite works correctly,
but it is also useful as a language feature so it is exposed to the user.

## Runtime

The runtime is small and contains the basic library functions `readInt`, `readString`, `printInt`, `printString` and `error` as well as internal core functions for string allocation and manipulation, allocating objects and arrays. It is written in C and included in `lib/runtime.c`. 

## Compilation process

After lexing and parsing that is automated using BNFC the compilation proceeds in phases.

### Phase one - syntactical rewrite

The `Syntax.Rewriter` module rewrites the parsed syntax tree into a desugarised, simplified version of the code.
The most important jobs performed by the Rewriter are rewriting `for` loops and computing constant expressions.

#### `for` loops

A `for` loop has the general form of:

```
for (<type> <loop_var_ident> : <expr>)
    <stmt>
```

This is desugarised to simpler language constructs into a sequence of statements with semantics equivalent to:

```
{
    var ~l_arr = <expr>;
    int ~l_idx = 0;

    while (~l_idx < ~l_arr.length) {
        <type> <loop_var_ident> = ~l_arr[~l_idx];
        <stmt>
    }
}
```

Note: all identifiers starting with the tylda character are internal identifiers used by the compiler.
By design, these are inexpressible using lexical rules of Latte, so they can only be accessed by generated code
and not by user supplied source.

#### Constant expressions

Expressions that are computable at compile-time are rewritten during this phase into their result.
This includes simple arithmetic expressions and relational operations that contain only constants
as their atomic components. No constant propagation done in this step.

### Phase two - top level metadata

The `SemanticAnalysis.TopLevel` module parses definitions of classes, methods and top level functions
and converts them to metadata containing field and method tables of all classes and their
inheritance hierarchies. The important jobs in this phase are:

- resolving inheritance hierarchies and asserting they contain no cycles;
- creating class method tables, taking method overriding into account;
- analysing field, method and formal parameter names asserting there are no duplicates;
- wrapping all top level functions into the special `~cl_TopLevel` class.

### Phase three - sematic analysis

The `SemanticAnalysis.Analyser` module computes type annotations, computes symbol tables
and performs control flow analysis. This is the biggest part of the frontend and contains all typing,
scoping and control flow rules.

The scoping rules are straightforward. No two symbols can be declared with the same identifier in the same scope.
Each block introduces a new scope. Additionally, every `if`, `while` and `for` statement introduces an implicit
block, even if it is single statement. This is to prevent code like:

```
if (cond) int x = 0;
return x;
```
from compiling.

The type rules introduce internal types for functions and a `Ref t` type, which is a reference to a symbol of type `t`.
The type rules do not cause a type to be wrapped in more than one `Ref` layer. The `Ref` layer is used to distinguish
between l-values and r-values: an assignment is valid if and only if its left-hand-side is a `Ref` type. Fora actual
typing rules consult the code in `SemanticAnalysis.Analyser`.

The control flow rules currently concern themselves with function return statements. Any non-void function is required
to have a `return` statement on each possible execution path. The Analyser tracks reachability of statements and combines
branch reachability of `if` and `while` statements. An important optimisation is that if a condition of a conditional
statement is trivially true (or false) the branch is considered to be always (or never) entered. Trivially true or false
means that it is either a true or false literal, but since this phase is performed after the Rewriter all constant boolean expressions are already collapsed to a single literal.

### Phase four - Espresso codegen

The modules in `Espresso.CodeGen` process the Latte code with annotations from semantic analysis and generates the intermediate representation in a quadruple code language Espresso. The grammar for the language can be found in `src/Espresso/Syntax/Espresso.cf`.

A program in Espresso starts with a `.metadata` table that contains type information about all classes and functions defined in the Latte code, plus runtime functions. Then a sequence of `.method` definitions follows as a sequence of quadruples including labels, jumps and operations. For a detailed scription of the instruction set refer to the grammar file.

Code generation ensures that there is no fall-through between labels, each basic block ends with a conditional or unconditional jump. Therefore, the blocks can be reordered arbitrarily.

### Phase five - Espresso analysis

The generated code needs additional annotations, mainly liveness information for all values and instructions. These are done by modules in `Espresso.ControlFlow`. First the code is divided into basic blocks and a Control Flow Graph is constructed, with blocks unreachable from `.L_entry` removed. Then global liveness analysis is performed.

### Phase six - SSA translation

The generated code needs to be translated to Single Static Assignment form to facilitate optimisations and register allocation. The algorithm is simple, inserting phony phi functions at the start of each basic block and then removing the unneeded ones.

### Phase seven - optimisation pipeline.

The SSA code is then optimised, performing the following steps:

- Liveness is reanalysed on the SSA graph.
- Dead code (assignments to dead values and unreachable instructions) is removed.
- Global Common Subexpression Elimination is performed.
- Constants and copies are propagated.

These steps preserve SSA form and are applied continously until reaching a fixpoint where no new optimisations are possible.

### Phase eight - global register allocation

Values in the optimised SSA Espresso code are assigned registers. First, an interference graph is constructed. The resulting graph is chordal and thus can be coloured efficiently with greedy colouring after computing the perfect elimination ordering using a lexicographical BFS.

If the graph cannot be coloured for x86_64 14 general-purpose register set (`%rsp` and `%rbp` excluded), spill code is inserted similar to the original spill code step of register allocation by Chaitin et al.

### Phase nine - `phi` elimination

Phony `phi` functions must be eliminnated before assembly codegen, but without creating new values since registers are already allocated. The algorithm inserts sequences of swaps, sets, stores and loads in blocks preceeding a block with a `phi` function that ensures correct placement of values in target registers.

### Phase ten - block contraction

The previous step generates blocks that can be easily optimised to avoid a few jumps. Namely, if the CFG contains edges `v -> u` such that `u` is the only successor of `v` and `v` is the only predecessor of `u`, the edge can be collapsed an the two blocks merged into one, since the jump between them always happens.

### Phase eleven - x86_64 assembly codegen

Assembly generation proceeds using the computed register allocation.

### Phase twelve - peephole optimisations

The result code is analysed by matching a number of patterns of common unoptimal code and fixing them locally. The process is repeated until a fixpoint is reached, i.e. no more optimisations are applicable.

### Phase thirteen - assembly and linking

As the final phase, `gcc` is used to compile the generated assembly and link it with the runtime.

## Grammar conflicts

### Latte

The Latte grammar contains 3 shift/reduce conflicts.

The first conflict is the standard issue with single-statement `if` statements that makes statements of the following form ambiguous:
```
if cond1
if cond2
  stmt1
else
  stmt2
```

The second conflict is the ambiguity between `(new t) [n]` and `new t[n]`. We cannot distinguish between a creation of an array
and an instantiation of a type with immediate indexing into it. The conflict is correctly solved in favour of array creation.

The third conflict is between a parenthesised single expression and a casted `null` expression, which is correctly resolved in favour of the `null` expression.

### Espresso

There is 1 shift/reduce conflict between `VNegInt` and `UnOpNeg`. `VNegInt` is a negative integer required to allow passing literal negative values as arguments without creating values for them, which would be tedious. Therefore the expression:
```
%v_0 := -42;
```
is ambiguous between `IUnOp` on `42` or `ISet` on `-42`. This is inconsequential and can be resolved either way without changing semantics.

## Sources

A few parts of the code were directly copied or heavily inspired by my earlier work on the Harper language (https://github.com/V0ldek/Harper), most notably the control flow analysis monoid based approach.

The grammar rules for `null` literals are a slightly modified version of rules proposed by Krzysztof Małysa.

