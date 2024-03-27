## Usage

### Building the compiler

Requirements:
- GHC (tested with version 9.4.8)
- `ghc` in `PATH`

Build the `jlc` compiler by running `make`. This will produce the compiler `jlc` executable in the root directory.

### Using the compiler
Run the compiler by calling the `jlc` executable, the compiler will then wait for the contents of a javalette `.jl` file (via stdin) to compile.

### Outcomes
The outcome is written to stderr and are the following:
- ERROR: Syntax error: \<explanation\>. There was a syntax error.
- ERROR: Type error: \<explanation\>. There an error during typechecking.
- OK. If the program has no syntax or type error.

### Options
There are no options as of now

### Building the lexer and parser from Javalette.cf

To build completely from scratch, there are additional requirements:
- The haskell libraries BNFC, alex, and happy. (These can be installed easily using Cabal)
-  `bnfc`, `alex`, and `happy` in `PATH`.

`make scratch` will then build the lexer and parser and finally the `jlc` executable. 

## Language Specification
See Javalette.cf file ([link](src/Javalette.cf)).

## Shift / Reduce conflicts

There is only a single shift/reduce conflict which is the standard dangling else.

```
Cond.      Stmt ::= "if" "(" Expr ")" Stmt  ;

CondElse.  Stmt ::= "if" "(" Expr ")" Stmt "else" Stmt  ;
```