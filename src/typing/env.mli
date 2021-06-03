(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Ast_utils

type t = (Varname.t * type_expr)  list

val img_te: type_expr

val mempty: unit -> t

val mappend: t -> t -> t

val extend: t -> Varname.t -> type_expr -> t

val extend_img: t -> Varname.t -> t

val lookup: t -> Varname.t -> type_expr option
