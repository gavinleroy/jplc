(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

open Ast_utils

(* An interface function includes not only
 * the overall ArrowT type but also the
 * return and param list types separately
 * to make it easy to get the information needed *)
type interface_function =
  { name        : string
  ; arrow_type  : type_expr
  ; return_type : type_expr
  ; params      : type_expr list }

val get_time_info: interface_function
