(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

val compile_prog:
  ?emit_parse:bool
  -> ?emit_type:bool
  -> ?emit_jir:bool
  -> ?emit_llvm:bool
  -> string
  -> Lexing.lexbuf
  -> unit
