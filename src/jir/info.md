# JIR

Just another Intermediate Representation, or *JIR* is the JPL IR that will be used to produce LLVM code. Previously, I tried using a simplified IR in the directory `../flattener/ast.ml` which was a flattened version of JPL. While JPL is a trivial language there *should* be a type of IR which represents a control flow graph (CFG). JIR will provide this control flow graph and made this explicit. 

## grammar

The most basic structure is a *Body*, this represents a function and contains a list of basic blocks:

*TODO* finisht the following
```txt
Body = 
    | [<BasicBlock> <BasicBlock> ...]
    
BasicBlock =
    | <Statement> ... <Terminator>
    
Statement =
    |
    
Terminator =
    |
    
RValue = 
    |
    
LValue = 
    |
```
