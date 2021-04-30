(*******************)
(*   Gavin  Gray   *)
(*     05.2021     *)
(*******************)

  (* [0-9]+ *)
  %token <int> INT
  (* \d*\.\d+|\d+\.\d* *)
  %token <float> FLOAT
  (* [a-zA-Z][a-zA-Z_\.]* *)
  %token <string> IDEN
  (* "( [\x20-\x7E]* )" *)
  %token <string> STRING
  (* : *)
  %token COLON
  (* { *)
  %token LCURLY
  (* } *)
  %token RCURLY
  (* ( *)
  %token LPAREN
  (* ) *)
  %token RPAREN
  (* , *)
  %token COMMA
  (* [ *)
  %token LSQUARE
  (* ] *)
  %token RSQUARE
  (* = *)
  %token EQUALS
  (* ==|<=|>=|!=|\+|\-|\*|\/|%|&&|\|\||<|>|!| *)
  %token <string> OP
  (* array *)
  %token ARRAY
  (* sum *)
  %token SUM
  (* if *)
  %token IF
  (* then *)
  %token THEN
  (* else *)
  %token ELSE
  (* let *)
  %token LET
  (* return *)
  %token RETURN
  (* assert *)
  %token ASSERT
  (* read *)
  %token READ
  (* write *)
  %token WRITE
  (* to *)
  %token TO
  (* print *)
  %token PRINT
  (* show *)
  %token SHOW
  (* time *)
  %token TIME
  (* fn *)
  %token FN
  (* attribute *)
  %token ATTRIBUTE
  (* \n character HEX: \x0A *)
  %token NEWLINE 
  (* An error represents a lexical error in the program *)
  %token ERROR
  (* End of the input stream *)
  %token EOF

