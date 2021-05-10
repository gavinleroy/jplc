/*******************/
/*   Gavin  Gray   */
/*     05.2021     */
/*******************/

%{
  open Ast
  open Ast_utils
%}

/* token declarations */

%token <int64> INT
%token <float> FLOAT
%token <string> IDEN
%token <string> STRING
%token COLON
%token SEMICOLON
%token LCURLY
%token RCURLY
%token LPAREN
%token RPAREN
%token COMMA
%token LSQUARE
%token RSQUARE

%token CMP
%token LTE 
%token GTE 
%token NEQ 
%token AND 
%token OR 
%token PLUS 
%token MINUS 
%token MUL 
%token DIV 
%token MOD 
%token LT 
%token GT 
%token BANG 
%token EQ 

%token TRUE
%token FALSE
%token ARRAY
%token SUM
%token IF
%token THEN
%token ELSE
%token LET
%token RETURN
%token ASSERT
%token READ
%token WRITE
%token TO
%token PRINT
%token SHOW
%token TIME
%token FN
%token IMAGE
%token VIDEO
/* %token ATTRIBUTE */
/* %token ERROR */
%token EOF

%right ELSE RSQUARE
%left AND OR
%left CMP NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left MUL DIV MOD
%nonassoc UNOP
%left LSQUARE LCURLY

/* starting production */
%start prog

%type <Ast.prog> prog
%type <type_expr> typee
%type <expr> expr
%type <arg> arg
%type <lvalue> lvalue
%type <binding> binding
%type <stmt> stmt
%type <cmd> ocmd

%% /* start the grammar productions */

prog:
  | cmds=list(ocmd); EOF { cmds }

ocmd:
  | FN; fn=IDEN; LPAREN; ps=separated_list(COMMA,binding); RPAREN; COLON; t=typee; LCURLY; ss=list(cstmt); RCURLY
    { FnC($startpos,Varname.of_string fn,ps,t,ss) }
  | c=cmd; SEMICOLON { c }

cmd:
  | READ; IMAGE; s=STRING; TO; a=arg { ReadimgC($startpos,Filename.of_string s,a)  }
  | READ; VIDEO; s=STRING; TO; a=arg { ReadvidC($startpos,Filename.of_string s,a)  }
  | WRITE; IMAGE; e=expr; TO; s=STRING { WriteimgC($startpos,e,Filename.of_string s) }
  | WRITE; VIDEO; e=expr; TO; s=STRING { WritevidC($startpos,e,Filename.of_string s) }
  | PRINT; s=STRING { PrintC($startpos,s) }
  | SHOW; e=expr { ShowC($startpos,e) }
  | TIME; c=cmd { TimeC($startpos,c) }
  | s=stmt { StmtC($startpos,s) }

cstmt:
  | s=stmt; SEMICOLON { s }

stmt:
  | LET; lv=lvalue; EQ; e=expr { LetS($startpos,lv,e) }
  | ASSERT; e=expr; COMMA; s=STRING { AssertS($startpos,e,s) }
  | RETURN; e=expr { ReturnS($startpos,e) }

binding:
  | LCURLY; bs=separated_list(COMMA,binding); RCURLY; { CrossbindB($startpos, bs) }
  | a=arg; COLON; t=typee { ArgB($startpos,a,t) }

lvalue:
  | LCURLY; lvs=separated_list(COMMA,lvalue); RCURLY { CrossbindLV($startpos, lvs) }
  | a=arg { ArgLV($startpos,a) }

arg:
  | fn=IDEN; LSQUARE; is=separated_list(COMMA,IDEN); RSQUARE
    { ArraybindA($startpos,Varname.of_string fn,List.map Varname.of_string is) }
  | i=IDEN { VarA($startpos,Varname.of_string i) }

expr:
  /* highest precedence group */
  | LPAREN; e=expr; RPAREN { e }
  | TRUE { TrueE $startpos }
  | FALSE { FalseE $startpos }
  | vn=IDEN { VarE($startpos,Varname.of_string vn) }
  | i=INT { IntE($startpos,i) }
  | f=FLOAT { FloatE($startpos,f) }
  /* second highest precedence group */
  | e=expr; LCURLY; i=INT; RCURLY { CrossidxE($startpos,e,i) }
  | e=expr; LSQUARE; es=separated_list(COMMA,expr); RSQUARE { ArrayidxE($startpos,e,es) }
  | uop=un_op; e=expr %prec UNOP { UnopE($startpos,uop,e) }
  | lhs=expr; bop=bin_op; rhs=expr
    { match bop with
      | And -> IteE($startpos,lhs,rhs,FalseE $startpos)
      | Or  -> IteE($startpos,lhs,TrueE $startpos,rhs)
      | _   -> BinopE($startpos,lhs,bop,rhs) }
  /* second lowest precedence group */
  | LCURLY; es=separated_list(COMMA,expr); RCURLY { CrossE($startpos,es) }
  | LSQUARE; es=separated_list(COMMA,expr); RSQUARE { ArrayCE($startpos,es) }
  | v=IDEN; LPAREN; ps=separated_list(COMMA,expr); RPAREN { AppE($startpos,Varname.of_string v,ps) }
  /* lowest precedence group */
  | IF; cnd=expr; THEN; ife=expr; ELSE; elsee=expr { IteE($startpos,cnd,ife,elsee) }
  | ARRAY; LSQUARE; ps=separated_list(COMMA,arr_bounds_e); RSQUARE; b=expr { ArrayLE($startpos,ps,b) }
  | SUM; LSQUARE; ps=separated_list(COMMA,arr_bounds_e); RSQUARE; b=expr { SumLE($startpos,ps,b) }

/* helper method for parsing array/sum exprs */
arr_bounds_e:
  | v=IDEN; COLON; e=expr { Varname.of_string v, e }

typee:
  | LCURLY; ts=separated_list(COMMA,typee); RCURLY { CrossT ts }
  | t=typee; LSQUARE; cs=list(COMMA); RSQUARE { ArrayT(t, Int64.of_int (List.length cs + 1)) }
  | i=IDEN
    { if i="int" then IntT
      else if i="bool" then BoolT
      else if i="float" then FloatT
      else if i="float3" then CrossT [FloatT; FloatT; FloatT]
      else CrossT [FloatT; FloatT; FloatT; FloatT] }

%inline bin_op:
  | AND { And }
  | OR { Or }
  | CMP { Cmp }
  | NEQ { Neq }
  | LT { Lt }
  | GT { Gt }
  | LTE { Lte }
  | GTE { Gte }
  | PLUS { Plus }
  | MINUS { Minus }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }

%inline un_op:
  | MINUS { Neg }
  | BANG { Bang }
