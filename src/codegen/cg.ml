(* (\************************\)
 * (\*      Gavin Gray      *\)
 * (\*       06.2021        *\)
 * (\************************\)
 *
 * open Core
 * open Flattening.Ast
 *
 * (\* NOTE a few assumptions that are being made in the code generator:
 *  * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  *
 *  * + the variables are already in SSA format and there are no duplicate names
 *  *
 *  * + no functions have the same name. these should also have been desugared in the
 *  *   flattening phase.
 *  *
 *  * + the program is type safe
 *  * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * *\)
 *
 * let ctx =
 *   Llvm.create_context ()
 *
 * module Env = struct
 *   type t =
 *     { ll_b   : Llvm.llbuilder
 *     (\* currently there is no support for modules/imports so all
 *      * code must be contained to one module *\)
 *     ; ll_m   : Llvm.llmodule
 *     ; tbl    : (string, Llvm.llvalue) Hashtbl.t }
 *   let mempty () =
 *     { ll_b = Llvm.builder ctx
 *     ; ll_m = Llvm.create_module ctx "jplm"
 *     ; tbl = Hashtbl.create(module String) }
 *   let mappend _m1 _m2 =
 *     assert false
 *   let set_bldr_pos bb t =
 *     Llvm.position_at_end bb t.ll_b
 *   let add_llv_ f t : unit =
 *     let (_ : 'a) = f t.ll_b in ()
 *   (\* let add_llv_partial_ vn f t =
 *    *   add_llv_ (f vn t.ll_b) t *\)
 *   let store_llv vn llv t =
 *     Hashtbl.add_exn ~key:vn ~data:llv t.tbl
 *   let store_partial vn f t =
 *     store_llv vn (f vn t.ll_b) t
 * end
 *
 * module State = Utils.Functional.State(Env)
 * module Monadic = Utils.Functional.Utils(State)
 *
 * open State
 * open Monadic
 * open Env
 *
 * let i64_t =
 *   Llvm.i64_type ctx
 *
 * let f64_t =
 *   Llvm.double_type ctx
 *
 * (\* NOTE booleans are represented as 1 bit integers *\)
 * let bool_t =
 *   Llvm.i1_type ctx
 *
 * (\* NOTE strings are represented as character pointers
 *  * in LLVM IR *\)
 * let str_t =
 *   Llvm.i8_type ctx
 *   |> Llvm.pointer_type
 *
 * let make_cross_t ts =
 *   Array.of_list ts
 *   |> Llvm.struct_type ctx
 *
 * let make_arrow_t rett ps =
 *   Array.of_list ps
 *   |> Llvm.function_type rett
 *
 * let rec llvm_t_of_runtime = function
 *   | Runtime.UnitRT -> assert false
 *   | Runtime.BoolRT -> bool_t
 *   | Runtime.IntRT -> i64_t
 *   | Runtime.FloatRT -> f64_t
 *   | Runtime.StringRT -> str_t
 *   | Runtime.ArrayRT (_base_t, _rank) ->
 *     assert false
 *   | Runtime.CrossRT (rts) ->
 *     List.map rts ~f:llvm_t_of_runtime
 *     |> make_cross_t
 *   | Runtime.ArrowRT (rt, ps) ->
 *     let rt = llvm_t_of_runtime rt in
 *     let ps = List.map ~f:llvm_t_of_runtime ps in
 *     make_arrow_t rt ps
 *
 * (\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\)
 * (\*            UTILITY FUNCTIONS         *\)
 * (\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\)
 *
 * let get_llv vn = get >>= fun env ->
 *   return (Hashtbl.find_exn env.tbl vn)
 *
 * let unwrap_vn = function
 *   | Varname (_, vn) -> vn
 *
 * let get_llv_vn =
 *   get_llv <.> unwrap_vn
 *
 * let ( <^ ) (Varname (_, vn)) str =
 *   vn ^ str
 *
 * (\* let ( ^> ) str (Varname (_, vn)) =
 *  *   vn ^ str *\)
 *
 * (\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\)
 * (\*            CODE GENERATORS          *\)
 * (\*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\)
 *
 * (\* NOTE all variables are allocated on the **stack**
 *  * afterwards, the LLVM mem2reg optimization pass can be used
 *  * to get variables allocated as registers instead. This will make
 *  * things easier. Therefore, when referenceing a variable, you need
 *  * to load it from memory :)  *\)
 *
 * let rec gen_code_of_expr opt_vn expr =
 *   match opt_vn, expr with
 *   | Some _vn, TrueE -> assert false
 *   | Some _vn,FalseE -> assert false
 *
 *   (\* true means that the integer is signed *\)
 *   (\* | IntE i -> return (`Terminal (Llvm.const_of_int64 i64_t i true)) *\)
 *   | Some vn, IntE i ->
 *     modify_ (Env.store_partial vn (Llvm.build_alloca i64_t))
 *     >> get_llv vn
 *     >>= fun ptr_llv ->
 *     modify_ (Env.add_llv_ (Llvm.build_store (Llvm.const_of_int64 i64_t i true) ptr_llv))
 *   (\* | `AllocaTerminal (alloca_t, store_v) ->
 *    *   modify_ (Env.store_partial vn (Llvm.build_alloca alloca_t))
 *    *   >> get_llv vn
 *    *   >>= fun ptr_llv ->
 *    *   modify_ (Env.add_llv_ (Llvm.build_store store_v ptr_llv)) *\)
 *
 *   | Some vn, FloatE f ->
 *     modify_ (Env.store_partial vn (Llvm.build_alloca f64_t))
 *     >> get_llv vn
 *     >>= fun ptr_llv ->
 *     modify_ (Env.add_llv_ (Llvm.build_store (Llvm.const_float f64_t f) ptr_llv))
 *
 *   (\* strings can only be arrays of chars at this point *\)
 *   | Some _vn,StringE _str -> assert false
 *
 *   (\* NOTE tuples should /probably/ be stored on the `stack` i.e. not in a virtual register.
 *    * when passing a tuple to a function we will have to get a pointer to it anyways, might
 *    * as well start with the pointer and use GEP to index into the struct. *\)
 *   | Some _vn,CrossE (_t, _vns) ->
 *     (\* let cross_t = llvm_t_of_runtime t in *\)
 *     (\* map_m ~f:get_llv_vn vns
 *      * >>= fun llvs ->
 *      * return (`Terminal (Llvm.const_struct ctx (Array.of_list llvs))) *\)
 *     assert false
 *
 *   (\* NOTE this stores the tuple in a temporary register rather than on the stack *\)
 *   (\* map_m ~f:(get_llv <.> unwrap_vn) vns
 *    * >>= fun llvs ->
 *    * return (`Terminal (Llvm.const_struct ctx (Array.of_list llvs))) *\)
 *
 *   (\* an array construction expression e.g. `let x = [1, 2, 3];` *\)
 *   (\* NOTE arrays should be stored on the `stack` i.e. not in a virtual register. *\)
 *   | Some _vn,ArrayCE (_t, _vns) ->
 *     assert false
 *   (\* (match t with
 *    *   | Runtime.ArrayRT (base_t, _rank) ->
 *    *     let array_t = Llvm.array_type (llvm_t_of_runtime base_t) (List.length vns) in
 *    *     map_m vns ~f:get_llv_vn
 *    *     >>= fun llvs ->
 *    *     let ll_arr = Llvm.const_array array_t (Array.of_list llvs) in
 *    *     return (`Partial (Llvm.build_array_alloca array_t ll_arr))
 *    *   | _ -> assert false) *\)
 *
 *   | Some vn, IBinopE (_t, l, o, r) -> get_llv_vn l
 *     >>= fun ptr_l -> get_llv_vn r
 *     >>= fun ptr_r -> let (l, r) = l<^".load", r<^".load" in
 *     modify_ (Env.store_partial l (Llvm.build_load ptr_l))
 *     >> modify_ (Env.store_partial r (Llvm.build_load ptr_r))
 *     >> get_llv l
 *     >>= fun llv_l -> get_llv r
 *     >>= fun llv_r ->
 *     modify_ (Env.store_partial vn (Llvm.build_alloca i64_t))
 *     >> get_llv vn
 *     >>= fun ptr_llv -> let vn_temp = vn ^ ".ibinop" in
 *     modify_ (Env.store_partial vn_temp ((match o with
 *         | `Lt -> Llvm.build_icmp Llvm.Icmp.Slt
 *         | `Gt -> Llvm.build_icmp Llvm.Icmp.Sgt
 *         | `Cmp -> Llvm.build_icmp Llvm.Icmp.Eq
 *         | `Lte -> Llvm.build_icmp Llvm.Icmp.Sle
 *         | `Gte -> Llvm.build_icmp Llvm.Icmp.Sge
 *         | `Neq -> Llvm.build_icmp Llvm.Icmp.Ne
 *         | `Mul -> Llvm.build_mul
 *         | `Div -> Llvm.build_sdiv
 *         | `Mod -> Llvm.build_srem
 *         | `Plus -> Llvm.build_add
 *         | `Minus -> Llvm.build_sub) llv_l llv_r))
 *     >> get_llv vn_temp
 *     >>= fun temp_llv ->
 *     modify_ (Env.add_llv_ (Llvm.build_store temp_llv ptr_llv))
 *
 *   (\* NOTE the floating point operations use the /unordered/ version as either of the
 *    * operands /could/ be a QNAN *\)
 *   | Some vn, FBinopE (_t, l, o, r) -> get_llv_vn l
 *     >>= fun ptr_l -> get_llv_vn r
 *     >>= fun ptr_r -> let (l, r) = l <^ ".load", r <^ ".load" in
 *     modify_ (Env.store_partial l (Llvm.build_load ptr_l))
 *     >> modify_ (Env.store_partial r (Llvm.build_load ptr_r))
 *     >> get_llv l
 *     >>= fun llv_l -> get_llv r
 *     >>= fun llv_r ->
 *     modify_ (Env.store_partial vn (Llvm.build_alloca f64_t))
 *     >> get_llv vn
 *     >>= fun ptr_llv -> let vn_temp = vn ^ ".fbinop" in
 *
 *     modify_ (Env.store_partial vn_temp ((match o with
 *         | `Lt -> Llvm.build_fcmp Llvm.Fcmp.Ult
 *         | `Gt -> Llvm.build_fcmp Llvm.Fcmp.Ugt
 *         | `Cmp -> Llvm.build_fcmp Llvm.Fcmp.Ueq
 *         | `Lte -> Llvm.build_fcmp Llvm.Fcmp.Ule
 *         | `Gte -> Llvm.build_fcmp Llvm.Fcmp.Uge
 *         | `Neq -> Llvm.build_fcmp Llvm.Fcmp.Une
 *         | `Mul -> Llvm.build_fmul
 *         | `Div -> Llvm.build_fdiv
 *         | `Mod -> Llvm.build_frem
 *         | `Plus -> Llvm.build_fadd
 *         | `Minus -> Llvm.build_fsub) llv_l llv_r))
 *     >> get_llv vn_temp
 *     >>= fun temp_llv ->
 *     modify_ (Env.add_llv_ (Llvm.build_store temp_llv ptr_llv))
 *
 *   | Some vn, UnopE (_t, o, Varname (t', vn')) -> get_llv vn'
 *     >>= fun ptr_llv -> let vn_load = vn' ^ ".load" in
 *     modify_ (Env.store_partial vn_load (Llvm.build_load ptr_llv))
 *     >> get_llv vn_load
 *     >>= fun llv ->
 *     let ty = llvm_t_of_runtime t' in
 *     modify_ (Env.store_partial vn (Llvm.build_alloca ty))
 *     >> get_llv vn
 *     >>= fun ptr_llv -> let vn_temp = vn ^ ".unop" in
 *     modify_ (Env.store_partial vn_temp ((match o, t' with
 *         | `Neg, IntRT -> Llvm.build_neg
 *         | `Neg, FloatRT -> Llvm.build_fneg
 *         | `Bang, BoolRT -> Llvm.build_not
 *         | _, _ -> assert false) llv))
 *     >> get_llv vn_temp
 *     >>= fun temp_llv ->
 *     modify_ (Env.add_llv_ (Llvm.build_store temp_llv ptr_llv))
 *
 *   (\* get_llv vn
 *    * >>= fun llv -> return  *\)
 *
 *   | Some vn, CastE (t, Varname (t_expr, vn')) ->
 *     let rt_t = llvm_t_of_runtime t in
 *     modify_ (Env.store_partial vn (Llvm.build_alloca rt_t))
 *     >> get_llv vn'
 *     >>= fun ptr_llv -> let vn_tmp = vn ^ ".load" in
 *     modify_ (Env.store_partial vn_tmp (Llvm.build_load ptr_llv))
 *     >> get_llv vn_tmp
 *     >>= fun llv_tmp ->
 *     let vn_cast = vn ^ ".cast" in
 *     modify_ (Env.store_partial vn_cast ((match t_expr, t with
 *         | IntRT, FloatRT -> Llvm.build_sitofp
 *         | FloatRT, IntRT -> Llvm.build_fptosi
 *         | _ -> assert false) llv_tmp rt_t))
 *     >> get_llv vn_cast
 *     >>= fun llv ->
 *     modify_ (Env.add_llv_ (Llvm.build_store llv ptr_llv))
 *
 *   | Some _vn, CrossidxE (_t, _vn', _idx) ->
 *     assert false
 *   (\* get_llv_vn vn
 *    * >>= fun base_llv ->
 *    * return (`Terminal (Llvm.const_extractvalue base_llv [| idx |])) *\)
 *
 *   | Some _vn,ArrayidxE (_t, __vn, _vns) ->
 *     assert false
 *   (\* get_llv_vn vn
 *    * >>= fun base_llv -> map_m vns ~f:get_llv_vn
 *    * >>= fun idx_llvs -> get
 *    * >>= fun env ->
 *    * let arr_lv = Llvm.build_gep base_llv (Array.of_list idx_llvs) "arr_temparr" env.ll_b in
 *    * return (`Partial (Llvm.build_load arr_lv)) *\)
 *
 *   | None, IteE _ -> assert false
 *   | Some _vn,ArrayLE (_t, _lbs, _es) -> assert false
 *   | Some _vn,SumLE (_t, _vn', _vns) -> assert false
 *   | Some _vn,AppE (_t, _vn', _vns) -> assert false
 *
 *   (\* previously seen as Stmts *\)
 *
 *   | None, LetE (Varname (_t, vn), e) ->
 *     gen_code_of_expr (Some vn) e
 *     >>= fun ll_ev -> return ll_ev
 *
 *   | None, AssertE (_vn, _str) -> assert false
 *
 *   (\* NOTE we append ".ret" to the vn name here to keep SSA form
 *    * when building the return instruction *\)
 *   | None, ReturnE (Varname (_t, vn)) -> get_llv vn
 *     >>= fun ptr_llv ->
 *     let vn = vn ^ ".ret" in
 *     modify_ (Env.store_partial vn (Llvm.build_load ptr_llv))
 *     >> get_llv vn
 *     >>= fun llv ->
 *  modify_ (Env.add_llv_ (Llvm.build_ret llv))
 *     >> return ()
 *
 *   (\* previously seen as Cmds *\)
 *   | None, ShowE _vn -> assert false
 *
 *   (\* NOTE this means that a statement got a symbol to assign to or an
 *    * expression didn't get a value to assign to *\)
 *   | _,_ -> assert false
 *
 * let gen_code_of_fn (fn : Fn.t) = get >>= fun env ->
 *   let Varname (fn_type, fn_name) = fn.name in
 *   let fn_type = llvm_t_of_runtime fn_type in
 *   let ll_f = Llvm.define_function fn_name fn_type env.ll_m in
 *   modify_ (Env.set_bldr_pos (Llvm.entry_block ll_f))
 *   >> map_m_ ~f:(gen_code_of_expr None) fn.body
 *
 * let gen_code_of_prog p =
 *   exec_state (map_m p ~f:gen_code_of_fn) (Env.mempty ())
 *   |> fun env ->
 *   Llvm_analysis.assert_valid_module env.ll_m; (\* TODO remove later *\)
 *   Ok env.ll_m
 *
 * (\* function for generating the string dump of a module *\)
 * let emit_llvm_module =
 *   Llvm.string_of_llmodule *)
