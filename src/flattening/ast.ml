(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

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

type fn_cmd =
  { fn_type : type_expr
  ; name    : var_name
  ; params  : param_binding list
  ; body    : returning_block }

type prog = fn_cmd list
