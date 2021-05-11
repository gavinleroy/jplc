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

let compile_prog ?(skip_typecheck = false) lexbuf =
  let open Result in
  Lex_parse.parse_prog lexbuf
  (* turn this into a flag *)
  >>= maybe_exit skip_typecheck Pp.print_sexp Sexp_ast.sexp_of_prog
  |> function
  | Ok _ -> (* TODO something *)
    ANSITerminal.(printf [magenta; Bold] "WARNING: unimplemented\n")
  | Error msg -> (* print the error message to the console *)
    Error.to_string_hum msg
    |> ANSITerminal.(eprintf [yellow; Bold] "%s")
