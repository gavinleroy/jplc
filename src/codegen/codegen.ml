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
    { ll_b : Llvm.llbuilder
    (* currently there is no support for modules/imports so all
     * code must be contained to one module *)
    ; ll_m : Llvm.llmodule
    ; tbl : (string, Llvm.llvalue) Hashtbl.t
    ; bbs : (string, Llvm.llbasicblock) Hashtbl.t }
  let mempty () =
    { ll_b = Llvm.builder ctx
    ; ll_m = Llvm.create_module ctx "jplm"
    ; tbl = Hashtbl.create(module String)
    ; bbs = Hashtbl.create(module String) }
  let mappend _m1 _m2 =
    assert false
  let set_bldr_pos bb t =
    Llvm.position_at_end bb t.ll_b
  let add_llv_ f t : unit =
    ignore (f t.ll_b : 'a)
  let store_llv vn llv t =
    Hashtbl.add_exn ~key:vn ~data:llv t.tbl
  let store_partial vn f t =
    store_llv vn (f vn t.ll_b) t
  let store_partial_global vn vl f t =
    store_llv vn (f vn vl t.ll_m) t
  let store_bb id bbl t =
    match Hashtbl.add t.bbs ~key:id ~data:bbl with
    | `Duplicate -> assert false
    | `Ok -> ()
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

let bb_tag id =
  Printf.sprintf "bb.%d" id

(* LLVM type declarations  *)

(* 32 bit integers are used solely for booleans at the moment *)
(* let i32_t =
 *   Llvm.i32_type ctx *)

let bool_t =
  Llvm.i1_type ctx

let i64_t =
  Llvm.i64_type ctx

let f64_t =
  Llvm.double_type ctx

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

let llvm_true_v =
  Llvm.const_int bool_t 1

let llvm_false_v =
  Llvm.const_int bool_t 0

let default_llv_of_type = function
  | Runtime.IntRT ->
    Llvm.const_of_int64 i64_t 0L true
  | Runtime.FloatRT ->
    Llvm.const_float f64_t 0.0
  | Runtime.BoolRT ->
    llvm_false_v
  (* NOTE not matching Arrow | Array | Cross | Unit | String | ETC ... *)
  | _ -> assert false


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*            UTILITY FUNCTIONS         *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let get_var ?(stem = "t") (n : int) =
  Printf.sprintf "_%s.%d" stem n

let lv_to_str = function
  | UserBinding (str,i) -> get_var ~stem:str i
  | Temp i -> get_var i
  | Symbol s -> s

let get_llv vn = get >>= fun env ->
  return (Hashtbl.find_exn env.tbl vn)

let get_llv_lv =
  get_llv <.> lv_to_str

let use_builder f = get
  >>= fun env ->
  return (f env.ll_b)

let get_bb_of_tag f tag = get
  >>= fun env ->
  match Hashtbl.find env.bbs tag with
  | Some bblv -> return bblv
  | None ->
    let lv = Llvm.append_block ctx tag f in
    modify_ (Env.store_bb tag lv)
    >> return lv


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*            CODE GENERATORS          *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let gen_code_of_constant c =
  return
    (`ConstRV
       (match c with
        | TRUE -> llvm_true_v

        | FALSE -> llvm_false_v

        | INT i ->
          Llvm.const_of_int64 i64_t i true

        | FLOAT f ->
          Llvm.const_float f64_t f

        | STATIC_STRING s ->
          Llvm.const_string ctx s))

let gen_code_of_rvalue = function

  | VarRV lv -> get_llv_lv lv
    >>= fun rv_ptr -> let temp = fresh_v () in
    modify_ (Env.store_partial temp (Llvm.build_load rv_ptr))
    >> get_llv temp
    >>= fun rv ->
    return (`ConstRV rv)

  | UnopRV (op, rhs) -> get_llv_lv rhs
    >>= fun ptr_llv -> let temp = fresh_v () in
    modify_ (Env.store_partial temp (Llvm.build_load ptr_llv))
    >> get_llv temp
    >>= fun llv ->
    return (`DynRV ((match op, Llvm.type_of llv |> Llvm.classify_type with
        | `Neg, Llvm.TypeKind.Integer -> Llvm.build_neg
        | `Neg, Llvm.TypeKind.Float -> Llvm.build_fneg
        | `Bang, Llvm.TypeKind.Integer -> Llvm.build_not
        | _, _ -> assert false) llv (fresh_v ())))

  | BinopRV (lhs, op, rhs) ->

    get_llv_lv lhs
    >>= fun ptr_lhs -> let templ = fresh_v () in
    modify_ (Env.store_partial templ (Llvm.build_load ptr_lhs))
    >> get_llv templ
    >>= fun llvl ->

    get_llv_lv rhs
    >>= fun ptr_rhs -> let tempr = fresh_v () in
    modify_ (Env.store_partial tempr (Llvm.build_load ptr_rhs))
    >> get_llv tempr
    >>= fun llvr ->

    return
      (`DynRV
         ((match Llvm.type_of llvl |> Llvm.classify_type
               , Llvm.type_of llvr |> Llvm.classify_type with

          | Llvm.TypeKind.Integer, Llvm.TypeKind.Integer ->
            (match op with
             | `Lt -> Llvm.build_icmp Llvm.Icmp.Slt
             | `Gt -> Llvm.build_icmp Llvm.Icmp.Sgt
             | `Cmp -> Llvm.build_icmp Llvm.Icmp.Eq
             | `Lte -> Llvm.build_icmp Llvm.Icmp.Sle
             | `Gte -> Llvm.build_icmp Llvm.Icmp.Sge
             | `Neq -> Llvm.build_icmp Llvm.Icmp.Ne
             | `Mul -> Llvm.build_mul
             | `Div -> Llvm.build_sdiv
             | `Mod -> Llvm.build_srem
             | `Plus -> Llvm.build_add
             | `Minus -> Llvm.build_sub)

          | Llvm.TypeKind.Float, Llvm.TypeKind.Float ->

            (match op with
             | `Lt -> Llvm.build_fcmp Llvm.Fcmp.Ult
             | `Gt -> Llvm.build_fcmp Llvm.Fcmp.Ugt
             | `Cmp -> Llvm.build_fcmp Llvm.Fcmp.Ueq
             | `Lte -> Llvm.build_fcmp Llvm.Fcmp.Ule
             | `Gte -> Llvm.build_fcmp Llvm.Fcmp.Uge
             | `Neq -> Llvm.build_fcmp Llvm.Fcmp.Une
             | `Mul -> Llvm.build_fmul
             | `Div -> Llvm.build_fdiv
             | `Mod -> Llvm.build_frem
             | `Plus -> Llvm.build_fadd
             | `Minus -> Llvm.build_fsub)

          | _, _ -> assert false) llvl llvr (fresh_v ())))

  | CastRV (ty, lv) -> get_llv_lv lv
    >>= fun ptrlv -> let temp = fresh_v () in
    modify_ ((Env.store_partial temp (Llvm.build_load ptrlv)))
    >> get_llv temp
    >>= fun llv ->
    let nt = llvm_t_of_runtime ty in
    return
      (`DynRV
         ((match Llvm.type_of llv
                 |> Llvm.classify_type, ty with
          | Llvm.TypeKind.Integer, Runtime.FloatRT ->
            Llvm.build_sitofp
          (* NOTE we need a double type 64 bits *)
          | Llvm.TypeKind.Double, Runtime.IntRT ->
            Llvm.build_fptosi
          | _, _ -> assert false) llv nt (fresh_v ())))

  | PhiRV { ty; paths } ->
    ignore ty; (* FIXME *)
    let rec loop ls ps =
      match ls with
      | [] -> return (List.rev ps)
      | (lv, bb_id) :: ls' ->
        use_builder Llvm.insertion_block
        >>= fun curr_bb ->
        let curr_f = Llvm.block_parent curr_bb in
        get_llv_lv lv >>= fun lv ->
        get_bb_of_tag curr_f (bb_tag bb_id)
        >>= fun bblv ->
        loop ls' ((lv, bblv) :: ps)
    in
    loop paths []
    >>= fun p_llvs ->
    use_builder (Llvm.build_phi p_llvs (fresh_v ()))
    >>= fun phi_ptr -> let phi_temp = fresh_v () in
    modify_ (Env.store_partial phi_temp (Llvm.build_load phi_ptr))
    >> get_llv phi_temp
    >>= fun phi ->
    return (`ConstRV phi)

  | ConstantRV c -> gen_code_of_constant c

let gen_code_of_term = function
  | Goto id ->
    (* potentially copy/pasted *)
    use_builder Llvm.insertion_block
    >>= fun curr_bb ->
    let curr_f = Llvm.block_parent curr_bb in
    get_bb_of_tag curr_f (bb_tag id)
    >>= fun bb ->
    modify_ (Env.add_llv_ (Llvm.build_br bb))

  | Ite { cond
        ; if_bb
        ; else_bb
        ; merge_bb } -> get_llv_lv cond
    >>= fun cnd_ptr -> let cnd_temp = fresh_v () in
    modify_ (Env.store_partial cnd_temp (Llvm.build_load cnd_ptr ))
    >> get_llv cnd_temp
    >>= fun cnd_llv ->
    let br_temp = fresh_v () in
    modify_ (Env.store_partial br_temp
               (Llvm.build_icmp Llvm.Icmp.Ne cnd_llv llvm_false_v))
    >> get_llv br_temp
    >>= fun br_llv ->
    use_builder Llvm.insertion_block
    >>= fun curr_bb -> let curr_f = Llvm.block_parent curr_bb in
    let (if_tag, else_tag, merge_tag) = bb_tag if_bb
                                      , bb_tag else_bb
                                      , bb_tag merge_bb in
    (* generate the future basic blocks so we can use them here *)
    get_bb_of_tag curr_f if_tag
    >>= fun if_bb -> get_bb_of_tag curr_f else_tag
    >>= fun else_bb -> get_bb_of_tag curr_f merge_tag
    >>
    (* FIXME below *)
    (* >>= fun merge_bb -> *)
    (* ignore the merge tag, we just needed to generate it *)
    (* begin *)
    (* Llvm.move_block_after curr_bb if_bb;
     * Llvm.move_block_after if_bb else_bb;
     * Llvm.move_block_after else_bb merge_bb; *)
    modify_ (Env.set_bldr_pos curr_bb)
    (* end *)
    >> modify_ (Env.add_llv_ (Llvm.build_cond_br br_llv if_bb else_bb))

  | Return l -> get_llv_lv l
    >>= fun ptr_llv -> let fsh = fresh_v () in
    modify_ (Env.store_partial fsh (Llvm.build_load ptr_llv ))
    >> get_llv fsh
    >>= fun llv ->
    modify_ (Env.add_llv_ (Llvm.build_ret llv))

  | Call { fn_name; params; write_to; success_jump_to } ->
    ignore fn_name;
    ignore params;
    ignore write_to;
    ignore success_jump_to;
    assert false

let gen_code_of_stmt = function
  | Bind (lv, _ty, rv) ->
    get_llv_lv lv
    >>= fun ptr_llv ->
    gen_code_of_rvalue rv
    >>= (function
        | `ConstRV llv_rv ->
          modify_
            (Env.add_llv_
               (Llvm.build_store llv_rv ptr_llv))
        | `DynRV f -> get >>= fun env ->
          modify_
            (Env.add_llv_
               (Llvm.build_store (f env.ll_b) ptr_llv)))

(* NOTE generating a code block will generate a
 * basic block at the END of the function fn_llval.
 * The name of the basic block will be BB.<id>. *)
let gen_code_of_bb fn_llval = function
  | BB { id; stmts; term } ->
    get_bb_of_tag fn_llval (bb_tag id)
    >>= fun bb ->
    modify_ (Env.set_bldr_pos bb)
    >> map_m_ stmts ~f:gen_code_of_stmt
    >> gen_code_of_term term
    >> return bb

let gen_code_of_fn { name
                   ; signature
                   ; bindings
                   ; body } =
  let gen_code_of_local_binding (lv, ty) =
    let llvm_ty = llvm_t_of_runtime ty in
    let lv_str =  lv_to_str lv in
    modify_ (Env.store_partial lv_str (Llvm.build_alloca llvm_ty))
  in
  get >>= fun env ->
  let fn_type = llvm_t_of_runtime signature in
  let ll_f = Llvm.define_function (lv_to_str name) fn_type env.ll_m in
  (* set the entry block for the function *)
  let entry_bb = Llvm.entry_block ll_f in
  modify_ (Env.set_bldr_pos entry_bb)
  (* allocate space for each used variable *)
  >> map_m_ bindings ~f:gen_code_of_local_binding
  (* the bindings form a basic block and we need
   * a terminator that goes to the next
   * basic block *)
  >> let (BB bb) = List.hd_exn body in
  get_bb_of_tag ll_f (bb_tag bb.id)
  >>= fun next_bb ->
  modify_ (Env.add_llv_ (Llvm.build_br next_bb))
  (* turn all the basic blocks into llvm *)
  >> map_m_ body ~f:(gen_code_of_bb ll_f)

let gen_code_of_global (lv, ty) =
  let lv_str =  lv_to_str lv in
  modify_ (Env.store_partial_global lv_str
             (* store the defaultl type in the global position *)
             (default_llv_of_type ty)
             Llvm.define_global)

let gen_code_of_prog { main; globals; prog } =
  let ex =
    map_m globals ~f:gen_code_of_global
    >> map_m prog ~f:gen_code_of_fn
    >> gen_code_of_fn main
  in
  exec_state ex (Env.mempty ())
  |> fun env ->
  (* TODO catch LLVM errors with this assertion *)
  Llvm_analysis.assert_valid_module env.ll_m;
  Ok env.ll_m

(* function for generating the string dump of a module *)
let emit_llvm_module = Llvm.string_of_llmodule
