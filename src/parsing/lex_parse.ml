(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Lexing

(* Prints the line number and character number where the error occurred.*)
let print_error_pos lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "line: %d column: %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let cerr_msg str1 str2 str3 =
  Printf.sprintf "%s \n\t~~ %s: \"%s\"\n" str1 str2 str3

let parse_prog (lexbuf : Lexing.lexbuf)  =
  try Ok (Parser.prog Lexer.read_token lexbuf) with
  | Lexer.SyntaxError msg ->
    let error_msg = cerr_msg (print_error_pos lexbuf) "lexer" msg in
    Error (Error.of_string error_msg)
  | Parser.Error ->
    let error_msg = cerr_msg (print_error_pos lexbuf) "parser" "syntax error" in
    Error (Error.of_string error_msg)
