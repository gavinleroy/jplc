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
  | Unit
  | IntT
  | BoolT
  | FloatT
  | ArrayT of type_expr * rank
  | CrossT of type_expr list
  | ArrowT of type_expr * type_expr list

let rec sexp_of_type = function
  | Unit -> Sexp.Atom "UnitType"
  | IntT -> Sexp.Atom "IntType"
  | BoolT -> Sexp.Atom "BoolType"
  | FloatT -> Sexp.Atom "FloatType"
  | ArrayT (te, r) ->
    Sexp.(List[Atom "ArrayType"; sexp_of_type te; Int64.sexp_of_t r])
  | CrossT tes ->
    Sexp.(List[Atom "CrossType"; List.sexp_of_t sexp_of_type tes])
  | ArrowT (rt, ats) ->
    Sexp.(List[Atom "ArrowType"; sexp_of_type rt; List.sexp_of_t sexp_of_type ats])

(* XXX the catch all is troublesome *)
let rec ( = ) lhs rhs =
  match lhs, rhs with
  | Unit, Unit
  | IntT, IntT
  | BoolT, BoolT
  | FloatT, FloatT -> true
  | ArrayT(te1,r1), ArrayT(te2, r2) ->
    Int64.(=) r1 r2 && te1=te2
  | CrossT tes1, CrossT tes2 ->
    List.equal (=) tes1 tes2
  | ArrowT(t1,ts1), ArrowT(t2,ts2) ->
    t1=t2 && List.equal (=) ts1 ts2
  | _, _ -> false

let type_to_s t =
  Sexp.to_string (sexp_of_type t)

let type_list_to_s ts =
  Sexp.to_string (List.sexp_of_t sexp_of_type ts)

let sexp_of_binop o =
  Sexp.Atom (match o with
      | `Lt -> "<"
      | `Gt -> ">"
      | `Cmp -> "=="
      | `Lte -> "<="
      | `Gte -> ">="
      | `Neq -> "!="
      | `Mul -> "*"
      | `Div -> "/"
      | `Mod -> "%"
      | `Plus -> "+"
      | `Minus -> "-")

let sexp_of_unop o =
  Sexp.Atom
    (match o with
     | `Bang -> "!" | `Neg -> "-")
