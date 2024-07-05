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
- OK. If the program has no syntax or type error, the resulting LLVM IR will then be written to stdout.

### Building the lexer and parser from Javalette.cf

To build completely from scratch, there are additional requirements:
- The haskell libraries BNFC, alex, and happy. (These can be installed easily using Cabal)
-  `bnfc`, `alex`, and `happy` in `PATH`.

`make scratch` will then build the lexer and parser and finally the `jlc` executable. 

## Language Specification
See Javalette.cf file ([link](src/Javalette.cf)).

## Shift / Reduce conflicts

There are two shift/reduce conflicts
1. Just the standard dangling else.

```
Cond.      Stmt ::= "if" "(" Expr ")" Stmt  ;

CondElse.  Stmt ::= "if" "(" Expr ")" Stmt "else" Stmt  ;
```

2. When parsing eg `new int[2][3]` the `new int[2]` part could be reduced to an `Expr6` and then we'd get an `Indexed` instead of an `ENewArr`. This is not an issue since the parser always chooses the shift action, which produces the intended result.

```
Indexed.   Indexed ::= Expr6 IndexOp ;

IndexOp.   IndexOp ::= "[" Expr "]" ;

ENewArr.   Expr6 ::= "new" Type [IndexOp] ;

separator nonempty IndexOp "" ;
```

# Test-suite
Test-suite for the compiler can be found at https://github.com/TDA283-compiler-construction/project/tree/master/tester