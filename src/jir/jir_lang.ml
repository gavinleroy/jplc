(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core

type bin_op = Typing.Ast.bin_op
type un_op = Typing.Ast.un_op

type bb_id = int

(* HACK FIXME TODO this is under deep construction *)

type constant =
  | INT of Int64.t
  | FLOAT of float
  | TRUE
  | FALSE
  | STATIC_STRING of string

and rvalue =
  | UnopRV of un_op * lvalue
  | BinopRV of lvalue * bin_op * lvalue
  (* | CrossRV of lvalue list
   * | ArrayRV of lvalue list *)
  | ConstantRV of constant

and lvalue =
  | UserBinding of string * int
  | Temp of int

(* | Project of lvalue * LVALUE.f        *)

and statement =
  (* keep around the runtime type of the rvalue
   * for bindings to make things easier later (maybe) *)
  | Bind of lvalue * Runtime.runtime_type * rvalue

and terminator =
  | Goto of bb_id
  (* | Panic of basic_block *)
  | Iet of { cond : lvalue
           ; if_bb : bb_id
           ; else_bb : bb_id }
  | Return of lvalue

and basic_block =
  | BB of { id : bb_id
          ; stmts : statement list
          ; term : terminator }

and jir_fn =
  { name : string
  ; signature : Runtime.runtime_type list * Runtime.runtime_type
  ; bindings : (lvalue * Runtime.runtime_type) list
  ; body : basic_block Array.t }

and jir =
  { main : jir_fn
  ; prog : jir_fn list }

