(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Ast_utils

type t

exception Impossible of string

val mempty: unit -> t

(* WARNING exn *)
val mappend: t -> t -> t

val add_alias: t -> string -> string -> t

(* add a new symbol to the env
 * returning: the new symbol string, the updated env *)
val add_symbol: t -> Varname.t -> string * t

val lookup: t -> Varname.t -> string

val get_unique_var: t -> string * t

val add_new_expr: t -> string option -> Ast.expr -> t

val clear_exprs: t -> (Ast.expr list * t)

val open_scope: t -> t

val close_scope: t -> t

val get_fns: t -> Ast.Fn.t list
