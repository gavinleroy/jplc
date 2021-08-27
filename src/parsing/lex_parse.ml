(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Lexing
open Utils

module I = Parser.MenhirInterpreter

let get_p =
  Lexing.lexeme_start_p

let get_parse_error env =
  match I.stack env with
  | lazy Nil -> "invalid syntax"
  | lazy (Cons (I.Element (state, _, _, _), _)) ->
    try (Parser_messages.message (I.number state)) with
    | _ -> "invalid syntax"

let rec parse lexbuf (checkpoint : Ast.prog I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
    let token = Lexer.read_token lexbuf in
    let startp = lexbuf.lex_start_p
    and endp = lexbuf.lex_curr_p in
    let checkpoint = I.offer checkpoint (token, startp, endp) in
    parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    parse lexbuf checkpoint
  | I.HandlingError env' ->
    let err = get_parse_error env' in
    Err.cerr_msg ~pos:(get_p lexbuf) ~t:"parser" ~msg:err
  | I.Accepted v -> Ok v
  | I.Rejected ->
    Err.cerr_msg ~pos:(get_p lexbuf) ~t:"parser" ~msg:"syntax error"

let parse_prog (lexbuf : Lexing.lexbuf) =
  try parse lexbuf (Parser.Incremental.prog lexbuf.lex_curr_p) with
  | Lexer.SyntaxError msg ->
    Err.cerr_msg ~pos:(get_p lexbuf) ~t:"lexer" ~msg:msg

(* function for the monolithic api *not used anymore* *)
(* let parse_prog (lexbuf : Lexing.lexbuf)  =
 *   try Ok (Parser.prog Lexer.read_token lexbuf) with
 *   | Lexer.SyntaxError msg ->
 *     let error_msg = cerr_msg (print_error_pos lexbuf) "lexer" msg in
 *     Error (Error.of_string error_msg)
 *   | Parser.Error ->
 *     let error_msg = cerr_msg (print_error_pos lexbuf) "parser" "syntax error" in
 *     Error (Error.of_string error_msg) *)
