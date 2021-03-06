(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

open Core

val emit_llvm_module: Llvm.llmodule -> string

val gen_code_of_prog: Jir.Jir_lang.jir -> Llvm.llmodule Or_error.t
