(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let print_error_pos (p : Lexing.position) =
  (* let pos = lexbuf.lex_curr_p in *)
  (* pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) *)
  (* let p = Lexing.lexeme_start_p lexbuf in *)
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
  Printf.sprintf "line: %d column: %d"
    line_number column

let cerr_msg ~pos ~t ~msg =
  let m = Printf.sprintf "%s \n\t~~ %s: %s\n"
      (print_error_pos pos)
      t msg in
  Error (Error.of_string m)
