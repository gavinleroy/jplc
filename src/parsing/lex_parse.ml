(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Lexing

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "[ %d : %d ]" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

module type PARSING = sig
  val parse_prog: Lexing.lexbuf -> Ast.prog Or_error.t
end

module Mon_parser : PARSING = struct
  let parse_prog (lexbuf : Lexing.lexbuf)  =
    try Ok (Parser.prog Lexer.read_token lexbuf) with
    (* catch exception and turn into Error *)
    | Lexer.SyntaxError msg ->
      let error_msg =
        Printf.sprintf "%s \n\t~~ %s\n" (print_error_position lexbuf) msg in
      Error (Error.of_string error_msg)
    | Parser.Error ->
      let error_msg =
        Printf.sprintf "%s \n\t~~ parser syntax error found\n" (print_error_position lexbuf) in
      Error (Error.of_string error_msg)
end

module Inc_parser : PARSING = struct
  let parse_prog (_lexbuf : Lexing.lexbuf)  =
    Error (Error.of_string "unimplemented")
end
