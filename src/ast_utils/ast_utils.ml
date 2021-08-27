(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

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

let list_equal (f : 'a -> 'a -> bool) l r =
  List.map2 f l r
  |> (fun vs -> List.fold_left (&&) true vs)

let repeat (s : string) (n : int) =
  let rec helper (s1 : string) (n1 : int) =
    if n1 = 0 then
      s1
    else helper (s1 ^ s) (n1 - 1)
  in helper "" n

(* XXX the catch all is troublesome *)
let rec ( = ) lhs rhs =
  match lhs, rhs with
  | Unit, Unit
  | IntT, IntT
  | BoolT, BoolT
  | FloatT, FloatT -> true
  | ArrayT(te1,r1), ArrayT(te2, r2) ->
    Int64.equal r1 r2 && te1=te2
  | CrossT tes1, CrossT tes2 ->
    list_equal (=) tes1 tes2
  | ArrowT(t1,ts1), ArrowT(t2,ts2) ->
    t1=t2 && list_equal (=) ts1 ts2
  | _, _ -> false

let rec pp_types fmt tys =
  let open Format in
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_type fmt v
  in fprintf fmt "(%a)"
    f tys

and pp_type fmt ty =
  let open Format in
  match ty with
  | Unit -> fprintf fmt "unit-ty"
  | IntT -> fprintf fmt "int-ty"
  | BoolT -> fprintf fmt "bool-ty"
  | FloatT -> fprintf fmt "float-ty"
  | ArrayT (ty, rnk) ->
    fprintf fmt "(array-ty@ %a@ %Ld)"
      pp_type ty
      rnk
  | CrossT tys ->
    fprintf fmt "(cross-ty@ %a)"
      pp_types tys
  | ArrowT (ty, tys) ->
    fprintf fmt "(arrow-ty@ %a@ ->@ %a)"
      pp_types tys
      pp_type ty

let string_of_binop = function
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
  | `Minus -> "-"

and string_of_unop = function
  | `Bang -> "!"
  | `Neg -> "-"

let pp_binop fmt bo =
  let open Format in
  match bo with
  | `Lt -> fprintf fmt "<"
  | `Gt -> fprintf fmt ">"
  | `Cmp -> fprintf fmt "=="
  | `Lte -> fprintf fmt "<="
  | `Gte -> fprintf fmt ">="
  | `Neq -> fprintf fmt "!="
  | `Mul -> fprintf fmt "*"
  | `Div -> fprintf fmt "/"
  | `Mod -> fprintf fmt "%%"
  | `Plus -> fprintf fmt "+"
  | `Minus -> fprintf fmt "-"

let pp_unop fmt uo =
  let open Format in
  match uo with
  | `Bang -> fprintf fmt "!"
  | `Neg  -> fprintf fmt "-"

let  concat_with sep ls =
  let rec loop ls acc = match ls with
    | [] -> acc
    | [x] -> x :: acc
    | x :: y :: ls' ->
      loop (y :: ls') (sep :: x :: acc)
  in
  loop ls []
  |> (fun ls ->
      List.fold_right (^) ls "")
let rec code_of_type t =
  let open Printf in
  match t with
  | Unit -> "unit"
  | IntT -> "int"
  | BoolT -> "bool"
  | FloatT -> "float"
  | ArrayT (rt, i) ->
    let i = Int64.to_int i in
    sprintf "%s[%s]" (code_of_type rt) (repeat "," (i - 1))
  | CrossT (rts) ->
    sprintf "{ %s }" (List.map code_of_type rts
                      |> concat_with ", ")
  | ArrowT (rt, rts) ->
    sprintf "( %s )" (List.map code_of_type (rts @ [rt])
                      |> concat_with " -> ")

let type_to_s = code_of_type

let string_of_type = type_to_s

let type_list_to_s ts =
  List.map type_to_s ts
  |> (concat_with " ")
