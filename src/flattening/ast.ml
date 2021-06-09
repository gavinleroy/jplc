(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

(* NOTE this file is a
 * mess. I need to untangle
 * the cyclic dependencies
 * between modules. *)

open Core
open Runtime

type var_name =
  | Varname of runtime_type * string

(* Array/Tuple deconstructions must be expanded *)
type param_binding =
  { var       : string
  ; bind_type : runtime_type }

type loop_binding =
  { var   : var_name
  ; bound : var_name } (* bound is a variable reference to an integer *)

type expr =
  | TrueE | FalseE
  | IntE of Int64.t
  | FloatE of float
  | CrossE of runtime_type * var_name list
  | ArrayCE of runtime_type * var_name list
  | BinopE of runtime_type * var_name * Ast_utils.bin_op * var_name
  | UnopE of runtime_type * Ast_utils.un_op * var_name
  | CastE of runtime_type * var_name
  | CrossidxE of runtime_type * var_name * int
  | ArrayidxE of runtime_type * var_name * var_name list
  | IteE of runtime_type * var_name * returning_block * returning_block
  | ArrayLE of runtime_type * loop_binding list * returning_block
  | SumLE of runtime_type * loop_binding list * returning_block
  | AppE of runtime_type * var_name * var_name list
  (* previously seen as Stmts *)
  | LetE of var_name * expr
  | AssertE of var_name * string
  | ReturnE of var_name
  (* previously seen as Cmds *)
  | ReadimgE of string * var_name
  | ReadvidE of string * var_name
  | WriteimgE of var_name * string
  | WritevidE of var_name * string
  | PrintE of string
  | ShowE of var_name
  (* A returning_block is a list of expressions where the
   * final expressions must write a value to a variable.
   * Examples include: function bodies, if/else bodies, etc ... *)
and returning_block = expr list

let sexp_of_binop o =
  let open Ast_utils in
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
  let open Ast_utils in
  Sexp.Atom (match o with
      | Bang -> "!" | Neg -> "-")

let sexp_of_varname = function
  | Varname(t,s) -> Sexp.(List [ sexp_of_rtype t; Atom s ])

let rec sexp_of_expr = function
  | TrueE ->
    Sexp.(List[Atom "TrueExpr"; sexp_of_rtype BoolRT])
  | FalseE ->
    Sexp.(List[Atom "FalseExpr"; sexp_of_rtype BoolRT])
  | IntE i ->
    Sexp.(List [ Atom "IntExpr" ; sexp_of_rtype IntRT ; Int64.sexp_of_t i ])
  | FloatE f ->
    Sexp.(List [ Atom "FloatExpr" ; sexp_of_rtype FloatRT ; Float.sexp_of_t f ])
  | CrossE (t,es) ->
    Sexp.(List [ Atom "CrossExpr"; sexp_of_rtype t; List.sexp_of_t sexp_of_varname es ])
  | ArrayCE (t,es) ->
    Sexp.(List [ Atom "ArrayConsExpr" ; sexp_of_rtype t ; List.sexp_of_t sexp_of_varname es ])
  | BinopE (t,lhs,op,rhs) ->
    Sexp.(List [ Atom "BinopExpr"; sexp_of_rtype t
               ; sexp_of_varname lhs ; sexp_of_binop op ; sexp_of_varname rhs ])
  | UnopE (t,op,e') ->
    Sexp.(List [ Atom "UnopExpr"; sexp_of_rtype t; sexp_of_unop op; sexp_of_varname e' ])
  | CastE(t,e') ->
    Sexp.(List [ Atom "CastExpr"; sexp_of_rtype t; sexp_of_varname e' ])
  | CrossidxE (t,e',i) ->
    Sexp.(List[Atom "CrossidxExpr"; sexp_of_rtype t; sexp_of_varname e'; Int.sexp_of_t i])
  | ArrayidxE (t,base,idxs) ->
    Sexp.(List[Atom "ArrayidxExpr"; sexp_of_rtype t; sexp_of_varname base
              ; List.sexp_of_t sexp_of_varname idxs])
  | IteE (t,cnd,ie,ee) ->
    Sexp.(List[Atom "IteExpr"
              ; sexp_of_rtype t ; sexp_of_varname cnd
              ; sexp_of_returning_block ie
              ; sexp_of_returning_block ee])
  | ArrayLE (t,vnes,bdy) ->
    Sexp.(List[Atom "ArrayExpr"
              ; sexp_of_rtype t
              ; List.sexp_of_t sexp_of_loop_binding vnes
              ; sexp_of_returning_block bdy])
  | SumLE (t,vnes,bdy) ->
    Sexp.(List[Atom "SumExpr"
              ; sexp_of_rtype t
              ; List.sexp_of_t sexp_of_loop_binding vnes
              ; sexp_of_returning_block bdy])
  | AppE (t,vn,es) ->
    Sexp.(List[Atom "AppExpr"
              ; sexp_of_rtype t
              ; sexp_of_varname vn
              ; List.sexp_of_t sexp_of_varname es])
  (* STMT *)
  | LetE (lv,e) ->
    Sexp.(List [ Atom "LetExpr"; sexp_of_rtype UnitRT; sexp_of_varname lv; sexp_of_expr e ])
  | AssertE (e,str) ->
    Sexp.(List [ Atom "AssertExpr"; sexp_of_rtype UnitRT; sexp_of_varname e; Atom str ])
  | ReturnE e ->
    Sexp.(List[Atom "ReturnExpr"; sexp_of_varname e ])
  (* CMD *)
  | ReadimgE (fn,a) ->
    Sexp.(List [ Atom "ReadImageExpr" ; Atom fn; sexp_of_varname a ])
  | ReadvidE (fn,a) ->
    Sexp.(List [ Atom  "ReadVideoExpr" ; Atom fn; sexp_of_varname a ])
  | WriteimgE (e,fn) ->
    Sexp.(List [ Atom "WriteImageExpr" ; sexp_of_varname e; Atom fn ])
  | WritevidE (e,fn) ->
    Sexp.(List [ Atom  "WriteVideoExpr" ; sexp_of_varname e; Atom fn ])
  | PrintE s ->
    Sexp.(List [ Atom "PrintExpr"; Atom s ])
  | ShowE e ->
    Sexp.(List [ Atom "ShowExpr"; sexp_of_varname e ])

and sexp_of_returning_block es =
  List.sexp_of_t sexp_of_expr es

(* Array/Tuple deconstructions must be expanded *)
and sexp_of_param_binding { var; bind_type; } =
  Sexp.(List [ Atom var; sexp_of_rtype bind_type ])

and sexp_of_loop_binding { var; bound; } =
  Sexp.(List [ sexp_of_varname var; sexp_of_varname bound ])

module Fn = struct
  module T = struct
    type t =
      { name    : var_name (* NOTE the function name at this point must be unique*)
      ; params  : param_binding list
      ; body    : returning_block }

    let compare t1 t2 =
      let Varname (_, s1) = t1.name in
      let Varname (_, s2) = t2.name in
      String.compare s1 s2

    let sexp_of_t t : Sexp.t =
      let Varname (fn_type, name) = t.name in
      List [ Atom "Func"
           ; sexp_of_rtype fn_type
           ; Atom name
           ; List.sexp_of_t
               sexp_of_param_binding t.params
           ; sexp_of_returning_block t.body]
  end
  include T
  include Comparator.Make(T)
end

type prog = Fn.t list

let sexp_of_prog (p : prog) =
  Sexp.(List [ Atom "Prog"
             ; List.sexp_of_t Fn.sexp_of_t p ])

let get_expr_type = function
  | TrueE | FalseE -> BoolRT
  | IntE _ -> IntRT
  | FloatE _ -> FloatRT
  (* other exprs return t *)
  | CrossE(t,_) | ArrayCE(t,_) | BinopE(t,_,_,_) | UnopE(t,_,_)
  | CastE(t,_) | CrossidxE(t,_,_) | ArrayidxE(t,_,_) | IteE(t,_,_,_)
  | ArrayLE(t,_,_) | SumLE(t,_,_) | AppE(t,_,_) -> t
  (* Stmt and Cmd don't return type *)
  | LetE _ | AssertE _ | ReturnE _ | ReadimgE _ | ReadvidE _
  | WriteimgE _ | WritevidE _ | PrintE _ | ShowE _ -> UnitRT
