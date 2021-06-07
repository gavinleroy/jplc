(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast_utils

type var_name =
  | Varname of type_expr * string

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

module Fn : sig
  module T : sig
    type t =
      { name    : var_name (* NOTE the function name at this point must be unique*)
      ; params  : param_binding list
      ; body    : returning_block }
    val compare: t -> t -> int
    val sexp_of_t: t -> Sexp.t
  end
  type t = T.t =
    { name    : var_name (* NOTE the function name at this point must be unique*)
    ; params  : param_binding list
    ; body    : returning_block }
  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  type comparator_witness = Base.Comparator.Make(T).comparator_witness
  val comparator : (t, comparator_witness) Comparator.t
end

type prog = Fn.t list

val sexp_of_prog: prog -> Sexp.t
