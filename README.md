JPL
===


**NOTE** 

:warning: no stable version available, only parsing and typechecking support full JPL.

*Just another Programming Language*, or JPL, is a simple array based language that is good for image processing or simple neural networks. This language was created for the course CS4470 at the University of Utah by [Pavel Panchekha](https://pavpanchekha.com/) and [John Regehr](https://www.cs.utah.edu/~regehr/).

The implementation here strays from the [original specification](spec.md) and is **a work in progress**.

The changes to JPL are as follows:

* Whitespace and newlines are completely ignroed and relies on semicolons to separate commands and statements. 

* Strings may allow many different types of characters; not only those that fall in ASCII range [0, 255].

* JIR is the intermediate language that JPL is written to before code generation.

* JPL is converted to LLVM IR for the backend intead of NASM.
