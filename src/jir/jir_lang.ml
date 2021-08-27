(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

type bin_op = Typing.Ast.bin_op
type un_op = Typing.Ast.un_op

type bb_id = int

(* HACK FIXME TODO this is under deep construction *)

type binding = (lvalue * Runtime.runtime_type)

and constant =
  | INT of Int64.t
  | FLOAT of float
  | TRUE
  | FALSE
  | STATIC_STRING of string

and rvalue =
  | UnopRV of un_op * lvalue
  | BinopRV of lvalue * bin_op * lvalue
  | VarRV of lvalue
  | CastRV of Runtime.runtime_type * lvalue
  | PhiRV of { ty : Runtime.runtime_type
             ; paths : (lvalue * bb_id) list }
  (* | CrossRV of lvalue list
   * | ArrayRV of lvalue list *)
  | ConstantRV of constant

and lvalue =
  | UserBinding of string * int
  | Temp of int
  | Symbol of string

(* | Project of lvalue * LVALUE.f        *)

and statement =
  (* keep around the runtime type of the rvalue
   * for bindings to make things easier later (maybe) *)
  | Bind of lvalue * Runtime.runtime_type * rvalue

and terminator =
  | Goto of bb_id
  (* | Panic of basic_block *)
  | Ite of { cond : lvalue
           ; if_bb : bb_id
           ; else_bb : bb_id
           ; merge_bb : bb_id }

  | Call of { fn_name : lvalue
            ; params : lvalue list
            (* where do you write the result of the call *)
            ; write_to : lvalue
            (* on success, which block do you jump to *)
            ; success_jump_to : bb_id
    (* TODO add block for jumping to on unwind *) }

  | Return of lvalue

and basic_block =
  | BB of { id : bb_id
          ; stmts : statement list
          ; term : terminator }

and jir_fn =
  { name : lvalue
  ; signature : Runtime.runtime_type
  ; bindings : binding list
  ; body : basic_block list }

and jir =
  { main : jir_fn
  ; globals : binding list
  ; prog : jir_fn list }
