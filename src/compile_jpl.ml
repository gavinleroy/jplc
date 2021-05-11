(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Utils
open Parsing

let maybe_exit ok_exit pp cnv ast =
  if ok_exit then
    (cnv ast |> pp; Error (Error.of_string ""))
  else Ok ast

let compile_prog lexbuf =
  let open Result in
  Lex_parse.Mon_parser.parse_prog lexbuf
  (* turn this into a flag *)
  >>= maybe_exit true Pp.print_sexp Sexp_ast.sexp_of_prog
  |> function
    (* TODO *)
  | Ok _ -> ()
  | Error msg ->
    Error.to_string_hum msg |> Printf.printf "%s"