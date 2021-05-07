{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1 }
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+ 
let float = '-'?((digit* '.' digit+) | (digit+ '.'))
let iden = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token = parse
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "array"     { ARRAY }
  | "sum"       { SUM }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "let"       { LET }
  | "return"    { RETURN }
  | "assert"    { ASSERT }
  | "read"      { READ }
  | "image"     { IMAGE }
  | "video"     { VIDEO }
  | "write"     { WRITE }
  | "to"        { TO }
  | "print"     { PRINT }
  | "show"      { SHOW }
  | "time"      { TIME }
  | "fn"        { FN }
  (* | "attribute" { ATTRIBUTE } *)
  | "\\"  { next_line lexbuf; read_token lexbuf }
  | "//"  { read_single_line_comment lexbuf }
  | "/*"  { read_multi_line_comment lexbuf }
  | ":"         { COLON }
  | ";"         { SEMICOLON }
  | "{"         { LCURLY }
  | "}"         { RCURLY }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | ","         { COMMA }
  | "["         { LSQUARE }
  | "]"         { RSQUARE }
  | "=="        { CMP }
  | "<="        { LTE }
  | ">="        { GTE }
  | "!="        { NEQ }
  | "&&"        { AND }
  | "||"        { OR }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { MUL }
  | "/"         { DIV }
  | "%"         { MOD }
  | "<"         { LT }
  | ">"         { GT }
  | "!"         { BANG }
  | "="         { EQ }
  | int   { INT (int_of_string (Lexing.lexeme lexbuf))}
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | iden  { IDEN (Lexing.lexeme lexbuf) }
  | whitespace { read_token lexbuf }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | newline    { next_line lexbuf; read_token lexbuf }
  | eof        { EOF }
  | _          { raise (SyntaxError ("lexer ~ illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf } 
  | eof     { EOF }
  | _       { read_single_line_comment lexbuf } 
  
and read_multi_line_comment = parse
  | "*/"    { read_token lexbuf } 
  | newline { next_line lexbuf; read_multi_line_comment lexbuf } 
  | eof     { raise (SyntaxError ("unterminated comment")) }
  | _       { read_multi_line_comment lexbuf } 
  
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("unterminated string")) }
