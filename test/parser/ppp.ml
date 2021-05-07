(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

(* open Core *)
open Compile_jpl

let ppp_ast str =
  compile_prog (Lexing.from_string str)
