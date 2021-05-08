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
  val ( = ) : t -> t -> bool
end

module String_id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let (=) = String.equal
end

module Varname : IDENTIFIER = String_id
module Filename : IDENTIFIER = String_id

type rank = Int64.t

type type_expr =
  | IntT
  | BoolT
  | FloatT
  | ArrayT of type_expr * rank
  | CrossT of type_expr list

