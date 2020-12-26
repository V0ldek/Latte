# Latte v0.8

Compiler of the [Latte programming language](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/Latte/description.html) written in Haskell.

## Compiling the project

Use `stack build` to compile the project. Use the `latc` executable to compile `.lat` source files.

Used version of GHC is 8.8.4 (LTS 16.22). For the breakdown of used packages consult `package.yaml`.

## Testing the project

Use `stack test` to run all included tests. The `lattest` directory contains all tests provided on
the [assignment page](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2020/latte-en.html) (these are unchanged)
and additional custom tests.

## Features

- Full lexer and parser for Latte with objects, virtual methods and arrays.
- Full semantic analysis with type checks and reachability analysis.

## Custom extensions

The grammar has been extended to include a `var` type declaration that can be used when declaring local variables to infer
their type to the compile-time type of the initialiser expression. It cannot be used in any other context and it cannot be
used in declarations without an initialiser. The motivation behind this is mainly so that `for` loop rewrite works correctly,
but it is also useful as a language feature so it is exposed to the user.

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
means that it is either a true or false literal, but since this phase is performed after the Rewriter all constant boolean expressions
are already collapsed to a single literal.

## Grammar conflicts

The grammar contains 3 shift/reduce conflicts.

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

## Sources

A few parts of the code were directly copied or heavily inspired by my earlier work on the Harper language (https://github.com/V0ldek/Harper),
most notably the control flow analysis monoid based approach.

The grammar rules for `null` literals are a slightly modified version of rules proposed by Krzysztof Małysa.