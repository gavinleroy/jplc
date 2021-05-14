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
  | IntE (_,i) -> Sexp.(List[Atom "IntExpr"; Int64.sexp_of_t i])
  | FloatE (_,f) -> Sexp.(List[Atom "FloatExpr"; Float.sexp_of_t f])
  | TrueE _ -> Sexp.(List[Atom "TrueExpr"])
  | FalseE _ -> Sexp.(List[Atom "FalseExpr"])
  | VarE (_,vn) -> Sexp.(List[Atom "VarExpr"; Atom (Varname.to_string vn)])
  | CrossE (_,es) -> Sexp.(List[Atom "CrossExpr"; List.sexp_of_t sexp_of_expr es])
  | ArrayCE (_,es) -> Sexp.(List[Atom "ArrayConsExpr"; List.sexp_of_t sexp_of_expr es])
  | BinopE (_,lhs,op,rhs) -> 
    Sexp.(List[Atom "BinopExpr"
              ; sexp_of_expr lhs
              ; sexp_of_binop op
              ; sexp_of_expr rhs])
  | UnopE (_,op,e') -> Sexp.(List[Atom "UnopExpr"; sexp_of_unop op; sexp_of_expr e'])
  | CastE(_,e',te) -> Sexp.(List[Atom "CastExpr"; sexp_of_expr e'; sexp_of_type te])
  (* NOTE cross index must be int to support static typing *)
  | CrossidxE (_,e',i) -> Sexp.(List[Atom "CrossidxExpr"; sexp_of_expr e'; Int64.sexp_of_t i])
  | ArrayidxE (_,base,idxs) ->
    Sexp.(List[Atom "ArrayidxExpr"; sexp_of_expr base
              ; List.sexp_of_t sexp_of_expr idxs])
  | IteE (_,cnd,ie,ee) ->
    Sexp.(List[Atom "IteExpr"; sexp_of_expr cnd
              ; sexp_of_expr ie; sexp_of_expr ee])
  | ArrayLE (_,vnes,bdy) ->
    Sexp.(List[Atom "ArrayExpr"
              ; List.sexp_of_t sexp_of_loopbind vnes
              ; sexp_of_expr bdy])
  | SumLE (_,vnes,bdy) ->
    Sexp.(List[Atom "SumExpr"
              ; List.sexp_of_t sexp_of_loopbind vnes
              ; sexp_of_expr bdy])
  | AppE (_,vn,es) ->
    Sexp.(List[Atom "AppExpr"
              ; Atom (Varname.to_string vn)
              ; List.sexp_of_t sexp_of_expr es])

let sexp_of_arg = function
  | VarA (_,vn) -> Sexp.(List[Atom "VarArg"; Atom (Varname.to_string vn)])
  | ArraybindA (_,vn,vns) -> 
    Sexp.(List[Atom "ArraybindArg"; Atom (Varname.to_string vn)
              ; List.sexp_of_t (fun v -> Atom (Varname.to_string v)) vns])

let rec sexp_of_lvalue = function
  | ArgLV (_,a) -> Sexp.(List[Atom "ArgLValue"; sexp_of_arg a])
  | CrossbindLV (_,lvs) -> 
    Sexp.(List[Atom "CrossbindLV"; List.sexp_of_t sexp_of_lvalue lvs])

let rec sexp_of_binding = function
  | ArgB (_,a,te) ->
    Sexp.(List[Atom "ArgBinding"; sexp_of_arg a; sexp_of_type te])
  | CrossbindB (_,bs) ->
    Sexp.(List[Atom "CrossBinding"; List.sexp_of_t sexp_of_binding bs])

let sexp_of_stmt = function
  | LetS (_,lv,e) -> Sexp.(List[Atom "LetStmt"; sexp_of_lvalue lv; sexp_of_expr e])
  | AssertS (_,e,str) -> Sexp.(List[Atom "AssertStmt"; sexp_of_expr e; Atom str])
  | ReturnS (_,e) -> Sexp.(List[Atom "ReturnStmt"; sexp_of_expr e])

let rec sexp_of_cmd = function
  | ReadimgC (_,fn,a) ->
    Sexp.(List [Atom "ReadImageCmd" 
               ; Atom (Filename.to_string fn); sexp_of_arg a])
  | ReadvidC (_,fn,a) ->
    Sexp.(List [Atom "ReadVideoCmd"
               ; Atom (Filename.to_string fn); sexp_of_arg a])
  | WriteimgC (_,e,fn) -> 
    Sexp.(List [Atom "WriteImageCmd"
               ; sexp_of_expr e; Atom (Filename.to_string fn)])
  | WritevidC (_,e,fn) -> 
    Sexp.(List [Atom "WriteVideoCmd"
               ; sexp_of_expr e; Atom (Filename.to_string fn)])
  | PrintC (_,s) -> Sexp.(List[Atom "PrintCmd"; Atom s ])
  | ShowC (_,e) -> Sexp.(List[Atom "ShowCmd"; sexp_of_expr e])
  | TimeC (_,c) -> Sexp.(List[Atom "TimeCmd"; sexp_of_cmd c])
  | FnC (_,vn,bs,te,ss) ->
    Sexp.(List[Atom "Func"
              ; Atom (Varname.to_string vn)
              ; List.sexp_of_t sexp_of_binding bs
              ; sexp_of_type te
              ; List.sexp_of_t sexp_of_stmt ss])
  | StmtC (_,s) -> 
    Sexp.(List[Atom "StmtCmd"; sexp_of_stmt s])

let sexp_of_prog (p : Ast.prog) =
  Sexp.(List[Atom "Prog"; List.sexp_of_t sexp_of_cmd p])
