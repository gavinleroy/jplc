(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Ast_utils

type expr =
  | IntE of Int64.t
  | FloatE of float
  | TrueE
  | FalseE
  | VarE of type_expr * Varname.t
  | CrossE of type_expr * expr list
  | ArrayCE of type_expr * expr list
  | BinopE of type_expr * expr * bin_op * expr
  | UnopE of type_expr * un_op * expr
  | CastE of type_expr * expr
  | CrossidxE of type_expr * expr * int
  | ArrayidxE of type_expr * expr * expr list
  | IteE of type_expr * expr * expr * expr
  | ArrayLE of type_expr * (Varname.t * expr) list * expr
  | SumLE of type_expr * (Varname.t * expr) list * expr
  | AppE of type_expr * Varname.t * expr list

type arg =
  | VarA of type_expr * Varname.t
  | ArraybindA of type_expr * Varname.t * Varname.t list

type lvalue =
  | ArgLV of type_expr * arg
  | CrossbindLV of type_expr * lvalue list

type binding =
  | ArgB of type_expr * arg
  | CrossbindB of type_expr * binding list

type stmt =
  | LetS of lvalue * expr
  | AssertS of expr * string
  | ReturnS of type_expr * expr

type cmd =
  | ReadimgC of Filename.t * arg
  | ReadvidC of Filename.t * arg
  | WriteimgC of expr * Filename.t
  | WritevidC of expr * Filename.t
  | PrintC of string
  | ShowC of expr
  | TimeC of cmd
  | FnC of type_expr * Varname.t * binding list * type_expr * stmt list
  | StmtC of stmt

type prog = cmd list

val extract_expr_type: expr -> type_expr

val extract_binding_type: binding -> type_expr
