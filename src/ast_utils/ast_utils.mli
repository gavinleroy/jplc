(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Base

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

val sexp_of_type: type_expr -> Sexp.t

val ( = ): type_expr -> type_expr -> bool

val type_to_s: type_expr -> string

val type_list_to_s: type_expr list -> string

val sexp_of_binop:
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
  | `Minus ] -> Sexp.t

val sexp_of_unop: [< `Bang | `Neg ] -> Sexp.t
