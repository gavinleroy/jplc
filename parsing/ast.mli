(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Ast_utils

type expr =
   | IntE of loc * int
   | FloatE of loc * float
   | TrueE of loc
   | FalseE of loc
   | VarE of loc * Varname.t
   | CrossE of loc * expr list
   | ArrayCE of loc * expr list
   | BinopE of loc * expr * bin_op * expr
   | UnopE of loc * un_op * expr
   (* NOTE cross index must be int to support static typing *)
   | CrossIdxE of loc * expr * int
   | ArrayIdxE of loc * expr * expr list
   | IteE of loc * expr * expr * expr
   | ArrayLE of loc * (Varname.t * expr) list * expr
   | SumLE of loc * (Varname.t * expr) list * expr
   | AppE of loc * Varname.t * expr list

type arg =
  | VarA of loc * Varname.t
  | ArraybindA of loc * Varname.t * Varname.t list

type lvalue =
  | ArgLV of loc * arg
  | CrossbindLV of loc * lvalue list

type binding =
  | ArgB of loc * arg * type_expr
  | CrossbindB of loc * binding list

type stmt =
  | LetS of loc * lvalue * expr
  | AssertS of loc * expr * string
  | ReturnS of loc * expr

type cmd =
  | ReadimgC of loc * Filename.t * arg
  | ReadvidC of loc * Filename.t * arg
  | WriteimgC of loc * expr * Filename.t
  | WritevidC of loc * expr * Filename.t
  | PrintC of loc * string
  | ShowC of loc * expr
  | TimeC of loc * cmd
  | FnC of loc * Varname.t * binding list * type_expr * stmt list
  | StmtC of loc * stmt

type prog = cmd list

