(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Ast_utils

type bin_op =
  [ `Lt
  | `Gt
  | `Cmp
  | `Lte
  | `Gte
  | `Neq
  | `Mul
  | `Div
  | `Mod
  | `Plus
  | `Minus ]

type un_op =
  [ `Bang
  | `Neg ]

type expr =
  | IntE of loc * Int64.t
  | FloatE of loc * float
  | TrueE of loc
  | FalseE of loc
  | VarE of loc * Varname.t
  | CrossE of loc * expr list
  | ArrayCE of loc * expr list
  | BinopE of loc * expr * bin_op * expr
  | UnopE of loc * un_op * expr
  | CastE of loc * expr * type_expr
  (* NOTE cross index must be int to support static typing *)
  | CrossidxE of loc * expr * Int64.t
  | ArrayidxE of loc * expr * expr list
  | IteE of loc * expr * expr * expr
  | ArrayLE of loc * (Varname.t * expr) list * expr
  | SumLE of loc * (Varname.t * expr) list * expr
  | AppE of loc * Varname.t * expr list

(* return the position of the ast expr *)
let extract_expr_pos = function
  | IntE (l,_) | FloatE (l,_)
  | FalseE l | TrueE l
  | VarE(l,_) | CrossE(l,_)
  | ArrayCE(l,_) | BinopE(l,_,_,_)
  | UnopE(l,_,_) | CastE(l,_,_)
  | CrossidxE(l,_,_) | ArrayidxE(l,_,_)
  | IteE(l,_,_,_) | ArrayLE(l,_,_)
  | SumLE(l,_,_) | AppE(l,_,_) -> l

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

