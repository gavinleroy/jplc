(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

(* open Core *)
open Compiler

let ppp_ast str =
  compile_prog ~skip_typecheck:true (Lexing.from_string str)
