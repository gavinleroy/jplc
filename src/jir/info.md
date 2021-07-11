# JIR

Just another Intermediate Representation, or *JIR* is the JPL IR that will be used to produce LLVM code. Previously, I tried using a simplified IR in the directory `../flattener/ast.ml` which was a flattened version of JPL. While JPL is a trivial language there *should* be a type of IR which represents a control flow graph (CFG). JIR will provide this control flow graph and made this explicit. 

:warning: this document is mostly for my thought process and will get updated alongside changes to the JIR.

## grammar

The most basic structure is a *Body*, this represents a function and contains a list of basic blocks:

```txt
JIR = 
    FN( <TYPE ... > ) -> <TYPE> {
        <BINDING> ... 
        <BASIC_BLOCK> <BASIC_BLOCK> ...
    }
    
BASIC_BLOCK = 
    <STATEMENT> ...  <TERMINATOR>
    
// NOTE future statements will be introduced into the JIR
STATEMENT =
    // normal assign statement
    | <LVALUE> = <RVALUE>

TERMINATOR =
    | GOTO <BASIC_BLOCK>                                              // jump to BASIC_BLOCK
    | PANIC <BASIC_BLOCK>                                             // initiate stack unwinding going to BASIC_BLOCK
    | IF( <LVALUE>, <BASIC_BLOCK~0>, <BASIC_BLOCK~1> )                // test LVALUE, if true, branch to 0 o.t. 1
    | CALL ( <LVALUE~0> <LVALUE~1> ( <LVALUE> ...  ), <BASIC_BLOCK> ) // call LVALUE~1 with the list of LVALUEs storing the result in LVALUE~0
                                                                      // after a successful return go to BASIC_BLOCK
    | RETURN                                                          // return to the caller
    
LVALUE = 
    | B              // reference to a user declared binidng
    | TEMP           // compiler introduced temporary
    | LVALUE.f       // project a tuple field
    | RETURN         // fn return pointer
    
RVALUE = 
    | <UNOP> <LVALUE>
    | <LVALUE> <BINOP> <LVALUE>
    | ( <LVALUE> <LVALUE> ... )
    | [ <LVALUE> <LVALUE> ... ]
    | <CONSTANT>
    
CONSTANT =
    | INT 
    | FLOAT 
    | TRUE 
    | FALSE
    | STATIC_STRING

```
