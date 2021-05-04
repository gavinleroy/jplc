(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

val parse_prog: Lexing.lexbuf -> Ast.prog Or_error.t

