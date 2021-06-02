(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast_utils

type var_name = string

(* Array/Tuple deconstructions must be expanded *)
type param_binding =
  { var       : string
  ; bind_type : type_expr }

type loop_binding =
  { var   : var_name
  ; bound : var_name } (* bound is a variable reference to an integer *)

type expr =
  | TrueE | FalseE
  | IntE of Int64.t
  | FloatE of float
  (* NOTE all previous exprs are no longer recursive *)
  | VarE of type_expr * var_name
  | CrossE of type_expr * var_name list
  | ArrayCE of type_expr * var_name list
  | BinopE of type_expr * var_name * bin_op * var_name
  | UnopE of type_expr * un_op * var_name
  | CastE of type_expr * var_name
  | CrossidxE of type_expr * var_name * int
  | ArrayidxE of type_expr * var_name * var_name list
  | IteE of type_expr * var_name * returning_block * returning_block
  | ArrayLE of type_expr * loop_binding list * returning_block
  | SumLE of type_expr * loop_binding list * returning_block
  | AppE of type_expr * var_name * var_name list
  (* previously seen as Stmts *)
  | LetE of var_name * expr
  | AssertE of var_name * string
  | ReturnE of type_expr * var_name
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

(* FOR THE DERIVING INTERFACE *)
let sexp_of_expr = Sexp_ast.sexp_of_expr

(* TODO *)
let compare_expr _e1 _e2 =
  0

module Expr = struct
  module T = struct
    type t = expr
    [@@deriving compare, sexp_of]
  end
  include T
  include Comparator.Make(T)
end

module Fn = struct
  module T = struct
    type t =
      { fn_type : type_expr
      ; name    : var_name (* NOTE the function name at this point must be unique*)
      ; params  : param_binding list
      ; body    : returning_block }

    let compare t1 t2 =
      String.compare t1.name t2.name

    let sexp_of_t t : Sexp.t =
      List [ Atom "Func"
           ; sexp_of_type t.fn_type
           ; Atom t.name
           ; List.sexp_of_t
               Sexp_ast.sexp_of_param_binding t.params
           ; Sexp_ast.sexp_of_returning_block t.body]

  end
  include T
  include Comparator.Make(T)
end

type prog = Fn.t list
