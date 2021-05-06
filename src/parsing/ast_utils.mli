(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Base

type loc = Lexing.position

type bin_op =
  | Lt | Gt
  | Cmp | Lte | Gte | Neq 
  | Or | Mul | Div | Mod
  | Plus | And | Minus

type un_op =
  | Bang | Neg

module type IDENTIFIER = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val (=) : t -> t -> bool
end

module Varname : IDENTIFIER
module Filename : IDENTIFIER

type rank = int

type type_expr =
  | IntT
  | BoolT
  | FloatT
  | ArrayT of type_expr * rank
  | CrossT of type_expr list

