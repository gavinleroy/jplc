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
  Sexp.Atom (match o with
      | Bang -> "!" | Neg -> "-")

let rec sexp_of_expr = function
  | TrueE ->
    Sexp.(List[Atom "TrueExpr"; sexp_of_type BoolT])
  | FalseE ->
    Sexp.(List[Atom "FalseExpr"; sexp_of_type BoolT])
  | IntE i ->
    Sexp.(List [ Atom "IntExpr" ; sexp_of_type IntT ; Int64.sexp_of_t i ])
  | FloatE f ->
    Sexp.(List [ Atom "FloatExpr" ; sexp_of_type FloatT ; Float.sexp_of_t f ])
  | VarE (t,vn) ->
    Sexp.(List [ Atom "VarExpr"; sexp_of_type t; Atom vn ])
  | CrossE (t,es) ->
    Sexp.(List [ Atom "CrossExpr"; sexp_of_type t; List.sexp_of_t String.sexp_of_t es ])
  | ArrayCE (t,es) ->
    Sexp.(List [ Atom "ArrayConsExpr" ; sexp_of_type t ; List.sexp_of_t String.sexp_of_t es ])
  | BinopE (t,lhs,op,rhs) ->
    Sexp.(List [ Atom "BinopExpr"; sexp_of_type t
               ; Atom lhs ; sexp_of_binop op ; Atom rhs ])
  | UnopE (t,op,e') ->
    Sexp.(List [ Atom "UnopExpr"; sexp_of_type t; sexp_of_unop op; Atom e' ])
  | CastE(t,e') ->
    Sexp.(List [ Atom "CastExpr"; sexp_of_type t; Atom e' ])
  | CrossidxE (t,e',i) ->
    Sexp.(List[Atom "CrossidxExpr"; sexp_of_type t; Atom e'; Int.sexp_of_t i])
  | ArrayidxE (t,base,idxs) ->
    Sexp.(List[Atom "ArrayidxExpr"; sexp_of_type t; Atom base
              ; List.sexp_of_t String.sexp_of_t idxs])
  | IteE (t,cnd,ie,ee) ->
    Sexp.(List[Atom "IteExpr"
              ; sexp_of_type t ; Atom cnd
              ; sexp_of_returning_block ie
              ; sexp_of_returning_block ee])
  | ArrayLE (t,vnes,bdy) ->
    Sexp.(List[Atom "ArrayExpr"
              ; sexp_of_type t
              ; List.sexp_of_t sexp_of_loop_binding vnes
              ; sexp_of_returning_block bdy])
  | SumLE (t,vnes,bdy) ->
    Sexp.(List[Atom "SumExpr"
              ; sexp_of_type t
              ; List.sexp_of_t sexp_of_loop_binding vnes
              ; sexp_of_returning_block bdy])
  | AppE (t,vn,es) ->
    Sexp.(List[Atom "AppExpr"
              ; sexp_of_type t
              ; Atom vn
              ; List.sexp_of_t String.sexp_of_t es])
  (* STMT *)
  | LetE (lv,e) ->
    Sexp.(List [ Atom "LetExpr"; sexp_of_type Unit; Atom lv; sexp_of_expr e ])
  | AssertE (e,str) ->
    Sexp.(List [ Atom "AssertExpr"; sexp_of_type Unit; Atom e; Atom str ])
  | ReturnE(t,e) ->
    Sexp.(List[Atom "ReturnExpr"; sexp_of_type t; Atom e ])
  (* CMD *)
  | ReadimgE (fn,a) ->
    Sexp.(List [ Atom "ReadImageExpr" ; Atom fn; Atom a ])
  | ReadvidE (fn,a) ->
    Sexp.(List [ Atom  "ReadVideoExpr" ; Atom fn; Atom a ])
  | WriteimgE (e,fn) ->
    Sexp.(List [ Atom "WriteImageExpr" ; Atom e; Atom fn ])
  | WritevidE (e,fn) ->
    Sexp.(List [ Atom  "WriteVideoExpr" ; Atom e; Atom fn ])
  | PrintE s ->
    Sexp.(List [ Atom "PrintExpr"; Atom s ])
  | ShowE e ->
    Sexp.(List [ Atom "ShowExpr"; Atom e ])

and sexp_of_returning_block es =
  List.sexp_of_t sexp_of_expr es

(* Array/Tuple deconstructions must be expanded *)
and sexp_of_param_binding { var; bind_type; } =
  Sexp.(List [ Atom var; sexp_of_type bind_type ])

and sexp_of_loop_binding { var; bound; } =
  Sexp.(List [ Atom var; Atom bound ])

let sexp_of_fn { fn_type; name; params; body; } =
  Sexp.(List [ Atom "Func"
             ; sexp_of_type fn_type
             ; Atom name
             ; List.sexp_of_t sexp_of_param_binding params
             ; sexp_of_returning_block body])

let sexp_of_prog (p : Ast.prog) =
  Sexp.(List [ Atom "Prog"
             ; List.sexp_of_t sexp_of_fn p ])
