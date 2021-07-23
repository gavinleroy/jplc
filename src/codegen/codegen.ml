(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

open Core
open Jir.Jir_lang

(* NOTE a few assumptions that are being made in the code generator:
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * + the variables are already in SSA format and there are no duplicate names
 *
 * + no functions have the same name. these should also have been desugared in the
 *   flattening phase.
 *
 * + the program is type safe
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

let ctx =
  Llvm.create_context ()

module Env = struct
  type t =
    { ll_b   : Llvm.llbuilder
    (* currently there is no support for modules/imports so all
     * code must be contained to one module *)
    ; ll_m   : Llvm.llmodule
    ; tbl    : (string, Llvm.llvalue) Hashtbl.t }
  let mempty () =
    { ll_b = Llvm.builder ctx
    ; ll_m = Llvm.create_module ctx "jplm"
    ; tbl = Hashtbl.create(module String) }
  let mappend _m1 _m2 =
    assert false
  let set_bldr_pos bb t =
    Llvm.position_at_end bb t.ll_b
  let add_llv_ f t : unit =
    let (_ : 'a) = f t.ll_b in ()
  (* let add_llv_partial_ vn f t =
   *   add_llv_ (f vn t.ll_b) t *)
  let store_llv vn llv t =
    Hashtbl.add_exn ~key:vn ~data:llv t.tbl
  let store_partial vn f t =
    store_llv vn (f vn t.ll_b) t
end

module State = Utils.Functional.State(Env)
module Monadic = Utils.Functional.Utils(State)

open State
open Monadic
open Env

let rnd_n = ref 0
let fresh_v () =
  incr rnd_n;
  Printf.sprintf "_x.%d" !rnd_n


let i64_t =
  Llvm.i64_type ctx

let f64_t =
  Llvm.double_type ctx

(* NOTE booleans are represented as 1 bit integers *)
let bool_t =
  Llvm.i1_type ctx

(* NOTE strings are represented as character pointers
 * in LLVM IR *)
let str_t =
  Llvm.i8_type ctx
  |> Llvm.pointer_type

let make_cross_t ts =
  Array.of_list ts
  |> Llvm.struct_type ctx

let make_arrow_t rett ps =
  Array.of_list ps
  |> Llvm.function_type rett

let rec llvm_t_of_runtime = function
  | Runtime.UnitRT -> assert false
  | Runtime.BoolRT -> bool_t
  | Runtime.IntRT -> i64_t
  | Runtime.FloatRT -> f64_t
  | Runtime.StringRT -> str_t
  | Runtime.ArrayRT (_base_t, _rank) ->
    assert false
  | Runtime.CrossRT (rts) ->
    List.map rts ~f:llvm_t_of_runtime
    |> make_cross_t
  | Runtime.ArrowRT (rt, ps) ->
    let rt = llvm_t_of_runtime rt in
    let ps = List.map ~f:llvm_t_of_runtime ps in
    make_arrow_t rt ps

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*            UTILITY FUNCTIONS         *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let get_var ?(stem = "t") (n : int) =
  Printf.sprintf "%%%s.%d" stem n

let lv_to_str = function
  | UserBinding (str,i) -> get_var ~stem:str i
  | Temp i -> get_var i

let get_llv vn = get >>= fun env ->
  return (Hashtbl.find_exn env.tbl vn)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*            CODE GENERATORS          *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let gen_code_of_constant = function
  | INT i ->
    return (Llvm.const_of_int64 i64_t i true)

  | FLOAT _f -> assert false
  | TRUE -> assert false
  | FALSE -> assert false
  | STATIC_STRING _s -> assert false

let gen_code_of_rvalue = function
  | UnopRV (__op, _v) ->
    assert false
  | BinopRV (_lhs, _bin_op, _rhs) ->
    assert false
  | ConstantRV c -> gen_code_of_constant c

let gen_code_of_term = function
  | Goto _id -> assert false
  | Iet _r (* { cond
            * ; if_bb
            * ; else_bb } *) -> assert false
  | Return l -> lv_to_str l |> get_llv
    >>= fun ptr_llv -> let fsh = fresh_v () in
    modify_ (Env.store_partial fsh (Llvm.build_load ptr_llv ))
    >> get_llv fsh
    >>= fun llv ->
    modify_ (Env.add_llv_ (Llvm.build_ret llv))


let gen_code_of_stmt = function
  | Bind (lv, _ty, rv) -> gen_code_of_rvalue rv
    >>= fun llv_rv -> lv_to_str lv |> get_llv
    >>= fun ptr_llv ->
    modify_ (Env.add_llv_ (Llvm.build_store llv_rv ptr_llv))

(* NOTE generating a code block will generate a
 * basic block at the END of the function fn_llval.
 * The name of the basic block will be BB.<id>. *)
let gen_code_of_bb _fn_llval = function
  | BB { id; stmts; term } ->
    ignore id; (* FIXME *)
    map_m_ stmts ~f:gen_code_of_stmt
    >> gen_code_of_term term

let gen_code_of_fn { name
                   ; signature
                   ; bindings
                   ; body } =
  let gen_code_of_binding (lv, ty) =
    let llvm_ty = llvm_t_of_runtime ty in
    let lv_str =  lv_to_str lv in
    modify_ (Env.store_partial lv_str (Llvm.build_alloca llvm_ty))
  in
  get >>= fun env ->
  let fn_type = llvm_t_of_runtime signature in
  let ll_f = Llvm.define_function name fn_type env.ll_m in
  (* set the entry block for the function *)
  modify_ (Env.set_bldr_pos (Llvm.entry_block ll_f))
  (* allocate space for each used variable *)
  >> map_m_ bindings ~f:gen_code_of_binding
  (* turn all the basic blocks into llvm *)
  >> map_m_ (Array.to_list body) ~f:(gen_code_of_bb ll_f)
(* >> map_m_ ~f:(gen_code_of_expr None) fn.body *)

let gen_code_of_prog { main; prog } =
  let ex =
    map_m prog ~f:gen_code_of_fn
    >> gen_code_of_fn main
  in
  exec_state  ex (Env.mempty ())
  |> fun env ->
  Llvm_analysis.assert_valid_module env.ll_m; (* TODO remove later *)
  Ok env.ll_m

(* function for generating the string dump of a module *)
let emit_llvm_module =
  Llvm.string_of_llmodule
