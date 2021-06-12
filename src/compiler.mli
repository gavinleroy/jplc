(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

val compile_prog:
  ?skip_typecheck:bool
  -> ?skip_flatten:bool
  -> ?skip_codegen:bool
  -> ?skip_assembler:bool
  -> Lexing.lexbuf
  -> unit
