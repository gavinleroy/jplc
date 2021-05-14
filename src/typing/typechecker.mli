(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

val type_prog: Parsing.Ast.prog -> Ast.prog Or_error.t
