# Latte v0.9

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
- `<p>.1.opt.esp`, `<p>.1.opt.cfg` - Optimised Espresso code and its CFG.
- `<p>.2.phi.esp`, `<p>.2.phi.cfg` - Optimised Espresso code with unfolded phony `phi` function usage and its CFG.
- `<p>.3.liv.esp`, `<p>.3.liv.cfg` - Same code as above but with liveness annotations in form of comments on every instruction and the CFG with liveness annotations for the begin and end of each node.
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
- x86_64 assembly code generation with register handling done locally via register/variable descriptions.
- Peephole optimisations of the generated assembly to fix common trivial inefficiencies.

## Unimplemented extensions

Objects, arrays and virtual methods are implemented in the static analysis phase, but not in Espresso or x86_64 codegen. These will be implemented in the next version of the compiler.

## Known issues

1. The way strings work is currently inconsistent with how objects will work in general. For example, the compiler assumes a default value for a string is `null`, but there is no way to express a string `null` literal in Latte code (the grammar does not allow `(string) null`). One can achieve it by declaring an uninitialised string variable, as the default value for such variables is `null`. It is planned to change in the final version where `strings` will most likely be defined as actual object types with special handling for string literals.

2. The generated code is wasteful when it comes to string literals. For example the code:

```
string x = "foo";
string y = "bar";
string z = x + y;
printString(z);
```

causes three string allocations for each of the variables `x`, `y`, `z`. This is planned to change in the final version where constant propagation will be implemented.

3. There is a slight issue with string allocation. Codegen for a string literal:
```
// Espresso code
%v_0 := "literal";
call void foo(string& %v_0);
```

looks like this:

```as
__const_1:
  .string "literal"

...

lea __const_1(%rip), %r10
movq %r10, %rdi # moving %v_0
movl $7, %esi
movq %r10, %r12 # moving %v_0 <---
call lat_new_string
movq %rax, %rdi # moving %v_0
call __cl_TopLevel.foo
```
the indicated `mov` instruction is redundant. The compiler sees that %v_0 is alive after the `IStr` instruction (namely used in the call to `foo`) so it tries to preserve it through the call to `lat_new_string` (since `%r10` is caller-saved). This is wasteful, since the value inside `%r10` at that point is the address of the string literal constant, but the logical value of `%v_0` is the result of the call to `lat_new_string`. Fixing this is nontrivial, so it will be done if time permits for the next version.

4. Conditional jumps where locals have to be persisted between blocks result in inefficient codegen. For example, the way a `<=` conditional is generated in Espresso is:

```
%v_cond := %v_0 <= %v_1;
jump if %v_cond then .L_then else .L_else;
```
Assume only %v_0 is alive at the end of this block. These two instructions are independently translated. First, the boolean value is created (assume `%v_0` in `%eax` and `%v_1` in `%edx`):
```
cmpl %eax, %edx
setle %dl
```
Then the conditional jump:
```
testb %dl, %dl
movl %eax, 8(%rbp) # save %v_0 on the stack
jz .L_else
jmp .L_then
```
But clearly this can be more efficiently realised with:
```
cmpl %eax, %edx
movl %eax, 8(%rbp) # save %v_0 on the stack
jg .L_else
jmp .L_then
```
Some of these are fixed by peephole optimisations, but that approach fails when there are the save-on-stack `mov`s in between the `set`-`test`-`jz` sequence. This is non-trivial to fix, so it will be done in the next version of the compiler.

## Custom extensions

The grammar has been extended to include a `var` type declaration that can be used when declaring local variables to infer
their type to the compile-time type of the initialiser expression. It cannot be used in any other context and it cannot be
used in declarations without an initialiser. The motivation behind this is mainly so that `for` loop rewrite works correctly,
but it is also useful as a language feature so it is exposed to the user.

## Runtime

The runtime is small and contains the basic library functions `readInt`, `readString`, `printInt`, `printString` and `error` as well as internal core functions for string allocation and manipulation. It is written in C and included in `lib/runtime.c`. 

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
as their atomic components.

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

The instruction set includes the phony function `phi` akin to LLVM's `phi`. It sets a value based on the label from which a jump was performed. The code generated by `Espresso.CodeGen` _is not_ in SSA form, but it uses `phi` for setting the return value of a method.

Code generation ensures that there is no fall-through between labels, each basic block ends with a conditional or unconditional jump. Therefore, the blocks can be reordered arbitrarily.

### Phase five - Espresso analysis

The generated code needs additional annotations, mainly liveness information for all values and instructions. These are done by modules in `Espresso.ControlFlow`. First the code is divided into basic blocks and a Control Flow Graph is constructed. Then the phony function usage is unfolded, since it is untranslateable into assembly directly. Then liveness analysis is performed on the new CFG graph.

This phase will also contain optimisation steps in the future version of the compiler.

### Phase six - x86_64 assembly codegen

Assembly generations proceeds by simulating the state of the target machine with register/value descriptions. Locals are persisted on the stack between basic blocks, while registers are greedily allocated within the block based on variable next use data computed in the previous phase. This phase leaves a lot of garbage code, like empty `addq $0, %rsp` instructions, but these are easily cleared in the next phase.

### Phase seven - peephole optimisations

The result code is analysed by matching a number of patterns of common unoptimal code and fixing them locally. The process is repeated until a fixpoint is reached, i.e. no more optimisations are applicable.

### Phase eight - assembly and linking

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