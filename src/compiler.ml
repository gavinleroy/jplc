(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Utils

let maybe_exit ok_exit pp cnv ast =
  if ok_exit then
    (cnv ast |> pp; Error (Error.of_string ""))
  else Ok ast

let compile_prog
    ?(skip_typecheck = false)
    ?(skip_flatten = false)
    ?(skip_codegen = false)
    lexbuf =
  let open Result in
  Parsing.Lex_parse.parse_prog lexbuf
  >>= maybe_exit skip_typecheck Pp.print_sexp Parsing.Sexp_ast.sexp_of_prog
  >>= Typing.Typechecker.type_prog
  >>= maybe_exit skip_flatten Pp.print_sexp Typing.Sexp_ast.sexp_of_prog
  >>= Flattening.Flattener.flatten_prog
  >>= maybe_exit skip_codegen Pp.print_sexp Typing.Sexp_ast.sexp_of_prog
  |> function
  | Ok _ -> (* TODO something *)
    ANSITerminal.(printf [yellow; Bold] "WARNING: unimplemented codegen\n")
  | Error msg -> (* print the error message to the console *)
    Error.to_string_hum msg
    |> ANSITerminal.(eprintf [red; Bold] "%s")
