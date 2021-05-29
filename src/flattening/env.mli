(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Ast_utils

type t

val empty: unit -> t

(* add a new symbol to the env
 * returning: the new symbol string, the updated env *)
val add_symbol: t -> Varname.t -> string * t

val lookup: t -> Varname.t -> string

val get_unique_var: t -> string * t

val open_scope: t -> t

val close_scope: t -> t
