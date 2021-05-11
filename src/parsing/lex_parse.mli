(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

(* module type PARSING = sig *)
val parse_prog: Lexing.lexbuf -> Ast.prog Or_error.t
(* end *)

(* module Inc_parser : PARSING *)
(* module Mon_parser : PARSING *)

