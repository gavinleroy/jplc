(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast
open Ast_utils

let sexp_of_binop o =
  Sexp.Atom (match o with
      | Lt -> "<"
      | Gt -> ">"
      | Cmp -> "=="
      | Lte -> "<="
      | Gte -> ">="
      | Neq -> "!="
      | Or -> "||"
      | Mul -> "*"
      | Div -> "/"
      | Mod -> "%"
      | Plus -> "+"
      | And -> "&&"
      | Minus -> "-")

let sexp_of_unop o =
  Sexp.Atom 
    (match o with
     | Bang -> "!" | Neg -> "-")

let rec sexp_of_expr e =
  let sexp_of_loopbind = fun (a,b) -> 
    Sexp.(List[Atom (Varname.to_string a); sexp_of_expr b]) in
  match e with
  | IntE i -> Sexp.(List[Atom "IntExpr"; sexp_of_type IntT; Int64.sexp_of_t i])
  | FloatE f -> Sexp.(List[Atom "FloatExpr"; sexp_of_type FloatT; Float.sexp_of_t f])
  | TrueE -> Sexp.(List[Atom "TrueExpr"; sexp_of_type BoolT])
  | FalseE -> Sexp.(List[Atom "FalseExpr"; sexp_of_type BoolT])
  | VarE (t,vn) -> Sexp.(List[Atom "VarExpr"; sexp_of_type t; Atom (Varname.to_string vn)])
  | CrossE (t,es) -> Sexp.(List[Atom "CrossExpr"; sexp_of_type t; List.sexp_of_t sexp_of_expr es])
  | ArrayCE (t,es) -> Sexp.(List[Atom "ArrayConsExpr"; sexp_of_type t; List.sexp_of_t sexp_of_expr es])
  | BinopE (t,lhs,op,rhs) ->
    Sexp.(List[Atom "BinopExpr"; sexp_of_type t
              ; sexp_of_expr lhs
              ; sexp_of_binop op
              ; sexp_of_expr rhs])
  | UnopE (t,op,e') -> Sexp.(List[Atom "UnopExpr"; sexp_of_type t; sexp_of_unop op; sexp_of_expr e'])
  | CastE(t,e') -> Sexp.(List[Atom "CastExpr"; sexp_of_type t; sexp_of_expr e'; sexp_of_type t])
  | CrossidxE (t,e',i) ->
    Sexp.(List[Atom "CrossidxExpr"; sexp_of_type t; sexp_of_expr e'; Int.sexp_of_t i])
  | ArrayidxE (t,base,idxs) ->
    Sexp.(List[Atom "ArrayidxExpr"; sexp_of_type t; sexp_of_expr base
              ; List.sexp_of_t sexp_of_expr idxs])
  | IteE (t,cnd,ie,ee) ->
    Sexp.(List[Atom "IteExpr"; sexp_of_expr cnd
              ; sexp_of_type t; sexp_of_expr ie; sexp_of_expr ee])
  | ArrayLE (t,vnes,bdy) ->
    Sexp.(List[Atom "ArrayExpr"
              ; sexp_of_type t
              ; List.sexp_of_t sexp_of_loopbind vnes
              ; sexp_of_expr bdy])
  | SumLE (t,vnes,bdy) ->
    Sexp.(List[Atom "SumExpr"
              ; sexp_of_type t
              ; List.sexp_of_t sexp_of_loopbind vnes
              ; sexp_of_expr bdy])
  | AppE (t,vn,es) ->
    Sexp.(List[Atom "AppExpr"
              ; sexp_of_type t
              ; Atom (Varname.to_string vn)
              ; List.sexp_of_t sexp_of_expr es])

let sexp_of_arg = function
  | VarA (t,vn) -> Sexp.(List[Atom "VarArg"; sexp_of_type t; Atom (Varname.to_string vn)])
  | ArraybindA (t,vn,vns) ->
    Sexp.(List[Atom "ArraybindArg" ; sexp_of_type t
              ; Atom (Varname.to_string vn)
              ; List.sexp_of_t (fun v -> Atom (Varname.to_string v)) vns])

let rec sexp_of_lvalue = function
  | ArgLV (t,a) -> Sexp.(List[Atom "ArgLValue"; sexp_of_type t; sexp_of_arg a])
  | CrossbindLV (t,lvs) ->
    Sexp.(List[Atom "CrossbindLV"; sexp_of_type t; List.sexp_of_t sexp_of_lvalue lvs])

let rec sexp_of_binding = function
  | ArgB (t,a) ->
    Sexp.(List[Atom "ArgBinding"; sexp_of_type t; sexp_of_arg a; sexp_of_type t])
  | CrossbindB (t,bs) ->
    Sexp.(List[Atom "CrossBinding"; sexp_of_type t; List.sexp_of_t sexp_of_binding bs])

let sexp_of_stmt = function
  | LetS (lv,e) -> Sexp.(List[Atom "LetStmt"; sexp_of_type Unit; sexp_of_lvalue lv; sexp_of_expr e])
  | AssertS (e,str) -> Sexp.(List[Atom "AssertStmt"; sexp_of_type Unit; sexp_of_expr e; Atom str])
  | ReturnS(t,e) -> Sexp.(List[Atom "ReturnStmt"; sexp_of_type t; sexp_of_type Unit; sexp_of_expr e])

let rec sexp_of_cmd = function
  | ReadimgC (fn,a) ->
    Sexp.(List [Atom "ReadImageCmd" 
               ; Atom (Filename.to_string fn); sexp_of_arg a])
  | ReadvidC (fn,a) ->
    Sexp.(List [Atom "ReadVideoCmd"
               ; Atom (Filename.to_string fn); sexp_of_arg a])
  | WriteimgC (e,fn) ->
    Sexp.(List [Atom "WriteImageCmd"
               ; sexp_of_expr e; Atom (Filename.to_string fn)])
  | WritevidC (e,fn) ->
    Sexp.(List [Atom "WriteVideoCmd"
               ; sexp_of_expr e; Atom (Filename.to_string fn)])
  | PrintC s -> Sexp.(List[Atom "PrintCmd"; Atom s ])
  | ShowC e -> Sexp.(List[Atom "ShowCmd"; sexp_of_expr e])
  | TimeC c -> Sexp.(List[Atom "TimeCmd"; sexp_of_cmd c])
  | FnC (t,vn,bs,te,ss) ->
    Sexp.(List[Atom "Func"
              ; sexp_of_type t
              ; Atom (Varname.to_string vn)
              ; List.sexp_of_t sexp_of_binding bs
              ; sexp_of_type te
              ; List.sexp_of_t sexp_of_stmt ss])
  | StmtC s ->
    Sexp.(List[Atom "StmtCmd"; sexp_of_type Unit; sexp_of_stmt s])

let sexp_of_prog (p : Ast.prog) =
  Sexp.(List[Atom "Prog"; List.sexp_of_t sexp_of_cmd p])
