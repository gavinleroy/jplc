(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core
open Printf
open Jir_lang
open Runtime

let jir_msg =
  "// JIR program dump, view at your own risk.
// ~Gavin :)"

(* type bin_op = Typing.Ast.bin_op
 * type un_op = Typing.Ast.un_op
 *
 * type bb_id = int
 *
 * (\* HACK FIXME TODO this is under deep construction *\)
 *
 * type constant =
 *   | INT of Int64.t
 *   | FLOAT of float
 *   | TRUE
 *   | FALSE
 *   | STATIC_STRING of string
 *
 * and rvalue =
 *   | UnopRV of un_op * lvalue
 *   | BinopRV of lvalue * bin_op * lvalue
 *   (\* | CrossRV of lvalue list
 *    * | ArrayRV of lvalue list *\)
 *   | ConstantRV of constant
 *
 * and lvalue =
 *   | UserBinding of string * int
 *   | Temp of int
 *
 * (\* | Project of lvalue * LVALUE.f        *\)
 *
 * and statement =
 *   (\* keep around the runtime type of the rvalue
 *    * for bindings to make things easier later (maybe) *\)
 *   | Bind of lvalue * Runtime.runtime_type * rvalue
 *
 * and terminator =
 *   | Goto of bb_id
 *   (\* | Panic of basic_block *\)
 *   | Iet of { cond : lvalue
 *            ; if_bb : bb_id
 *            ; else_bb : bb_id }
 *   | Return of lvalue
 *
 * and basic_block =
 *   | BB of { id : bb_id
 *           ; stmts : statement list
 *           ; term : terminator }
 *
 * and jir_fn =
 *   { name : string
 *   ; signature : Runtime.runtime_type list * Runtime.runtime_type
 *   ; bindings : (lvalue * Runtime.runtime_type) list
 *   ; body : basic_block Array.t }
 *
 * and jir =
 *   { main : jir_fn
 * ; prog : jir_fn list } *)

let concat_with sep vs =
  List.fold_left vs ~init:"" ~f:(fun acc v ->
      acc ^ sep ^ v)

let pp_binop = function
  | `Lt -> "<"
  | `Gt -> ">"
  | `Cmp -> "=="
  | `Lte -> "<="
  | `Gte -> ">="
  | `Neq -> "!="
  | `Mul -> "*"
  | `Div -> "/"
  | `Mod -> "%%"
  | `Plus -> "+"
  | `Minus -> "-"

let pp_unop = function
  | `Bang -> "!"
  | `Neg  -> "-"

let pp_const = function
  | INT i64 -> sprintf "%Ld" i64
  | FLOAT f -> sprintf "%f" f
  | TRUE -> "true"
  | FALSE -> "false"
  | STATIC_STRING str -> sprintf "\"%s\"" str

let pp_lvalue = function
  | UserBinding (str, i) -> sprintf "%s.%d" str i
  | Temp i -> sprintf "temp.%d" i

let pp_rvalue = function
  | UnopRV (uop, lv) ->
    sprintf "%s %s" (pp_unop uop) (pp_lvalue lv)
  | BinopRV (lvl, bop, lvr) ->
    sprintf "%s %s %s" (pp_lvalue lvl) (pp_binop bop) (pp_lvalue lvr)
  | VarRV lvl -> sprintf "%s" (pp_lvalue lvl)
  | ConstantRV const -> pp_const const

let pp_binding (lv, ty) =
  sprintf "%s : %s" (pp_lvalue lv) (code_of_type ty)

let pp_statement = function
  | Bind (lv, ty, rv) ->
    sprintf "%s : %s = %s;"
      (pp_lvalue lv)
      (code_of_type ty)
      (pp_rvalue rv)

let pp_terminator = function
  | Goto i -> sprintf "goto BB.%d;" i
  | Iet { cond; if_bb; else_bb } ->
    sprintf "if( %s | true -> BB.%d | BB.%d )"
      (pp_lvalue cond) if_bb else_bb
  | Return lv -> sprintf "return %s" (pp_lvalue lv)

let pp_bb = function
  | BB { id; stmts; term } ->
    sprintf "BB.%d {\n%s\n%s\n}" id
      (concat_with "\n" (List.map ~f:pp_statement stmts))
      (pp_terminator term)

let pp_fn { name
          ; signature
          ; bindings
          ; body } =
  (* the signature should /always/ be an arrow type *)
  let (rt, ps) = (function
      | ArrowRT (rt, ps) -> (rt, ps)
      | _ -> assert false) signature in
  sprintf
    "%s ( %s ) -> %s { \n  %s\n\n  %s\n\n}"
    name
    (concat_with ", " (List.map ~f:code_of_type (ps)))
    (rt |> code_of_type)
    (concat_with "\n" (List.map ~f:pp_binding bindings))
    (concat_with "\n" (Array.map ~f:pp_bb body |> Array.to_list))

let pp_jir { main; prog; } =
  concat_with "\n\n" (List.map ~f:pp_fn (main :: prog))

let string_of_jir = pp_jir
