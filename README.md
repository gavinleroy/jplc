# JPL


**NOTE** 

:warning: no stable version available. The interpreter for full JPL is available, however, the compiler is not (nor is the staged interpreter as planned).

*Just another Programming Language*, or JPL, is a simple array based language that is good for image processing or simple neural networks. This language was created for the course CS4470 at the University of Utah by [Pavel Panchekha](https://pavpanchekha.com/) and [John Regehr](https://www.cs.utah.edu/~regehr/).

The implementation here strays from the [original specification](spec.md) and is **a work in progress**.

## Changes to the JPL original spec

* Whitespace and newlines are completely ignroed and relies on semicolons to separate commands and statements. 

* Strings may allow many different types of characters; not only those that fall in ASCII range [0, 255].

* JIR is the intermediate language that JPL is written to before code generation.

* JPL is converted to LLVM IR for the backend intead of NASM.

## What is comming in the future

* Writing JPL to LLVM IR (currently only interpreter support).

* A staged interpreter utilizing MetaOCaml.

* Record types.

* *File Attributes* a term I came up with to describe macros at the module level.

* Lazy JPL, maybe just for fun.

