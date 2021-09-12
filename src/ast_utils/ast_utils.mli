(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

type loc = Lexing.position

module type IDENTIFIER = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val (=) : t -> t -> bool
end

module Varname : IDENTIFIER
module Filename : IDENTIFIER

type rank = Int64.t

type type_expr =
  | Unit
  | IntT
  | BoolT
  | FloatT
  | ArrayT of type_expr * rank
  | CrossT of type_expr list
  | ArrowT of type_expr * type_expr list

val string_of_type: type_expr -> string

val ( = ): type_expr -> type_expr -> bool

val pp_type: Format.formatter -> type_expr -> unit

val pp_types: Format.formatter -> type_expr list -> unit

val type_to_s: type_expr -> string

val type_list_to_s: type_expr list -> string

val string_of_binop:
  [< `Lt
  | `Gt
  | `Cmp
  | `Lte
  | `Gte
  | `Neq
  | `Mul
  | `Div
  | `Mod
  | `Plus
  | `Minus ] -> string

val string_of_unop: [< `Bang | `Neg ] -> string

val pp_binop: Format.formatter ->
  [< `Lt
  | `Gt
  | `Cmp
  | `Lte
  | `Gte
  | `Neq
  | `Mul
  | `Div
  | `Mod
  | `Plus
  | `Minus ] -> unit

val pp_unop: Format.formatter -> [< `Bang | `Neg ] -> unit

val code_of_type: type_expr -> string
