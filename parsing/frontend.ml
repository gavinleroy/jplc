(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

(* Prints the line number and character number where the error occurred.*)
let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_prog lexbuf =
  try Ok (Parser.prog Lexer.read_token lexbuf) with
  (* catch exception and turn into Error *)
  | SyntaxError msg ->
      let error_msg = Fmt.str "%s: %s@." (print_error_position lexbuf) msg in
      Error (Error.of_string error_msg)
  | Parser.Error ->
      let error_msg = Fmt.str "%s: syntax error@." (print_error_position lexbuf) in
      Error (Error.of_string error_msg)

let pprint_ast ppf (prog : Ast.prog) =
  Print_ast.pprint_program ppf prog

