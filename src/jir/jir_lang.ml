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
  | Binding of string
  | Temp of int

(* | Project of lvalue * LVALUE.f        *)

and statement =
  | Bind of lvalue * rvalue

and terminator =
  | Goto of bb_id
  (* | Panic of basic_block *)
  | Iet of { cond : lvalue
           ; if_bb : bb_id
           ; else_bb : bb_id }
  | Return

and basic_block =
  | BB of { id : bb_id
          ; stmts : statement list
          ; term : terminator }

and jir_fn =
  { signature : Runtime.runtime_type list * Runtime.runtime_type
  ; user_bindings : (string * Runtime.runtime_type) list
  ; temp_bindings : (int * Runtime.runtime_type) list
  ; body : basic_block list }

and jir =
  { main : jir_fn
  ; prog : jir_fn list }

