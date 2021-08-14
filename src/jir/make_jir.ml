(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core
open Jir_lang
open Ast_utils

module TA = Typing.Ast

type varname = string

module State = Utils.Functional.State(Env)
module Monadic = Utils.Functional.Utils(State)
open State
open Monadic

(* ~~~~~~~~~~~~~~~~~ *)
(* UTILITY FUNCTIONS *)
(* ~~~~~~~~~~~~~~~~~ *)

let take_until xs ~f =
  let rec loop = function
    | [] -> []
    | y :: ys ->
      if f y then
        [y]
      else y :: loop ys
  in
  loop xs

let push_stmt s =
  modify (flip Env.add_stmt s)

let lookup vn =
  let s = Varname.to_string vn in
  get >>= fun e ->
  return <$> Env.lookup e s

(* get the var_count from the
 * environment and add one *)
let fresh_int () = get
  >>= fun env ->
  modify (fun e ->
      { e with var_count = e.var_count + 1 })
  >> return env.var_count

let fresh_temp () = fresh_int ()
  >>= fun i -> return (Temp i)

let add_new_bb () = get
  >>= fun env ->
  let (id, env') = Env.add_fresh_bb env in
  put env' >> return id

let set_bb bbid =
  modify <$> flip Env.set_bb bbid

let get_bb () = get
  >>= fun env ->
  Env.get_curr_bb env
  |> return

let rec rt_of_t = function
  | Ast_utils.Unit ->
    Runtime.UnitRT
  | Ast_utils.BoolT ->
    Runtime.BoolRT
  | Ast_utils.IntT ->
    Runtime.IntRT
  | Ast_utils.FloatT ->
    Runtime.FloatRT
  | Ast_utils.ArrayT(b,r) ->
    Runtime.ArrayRT (rt_of_t b, Int64.to_int_exn r)
  | Ast_utils.CrossT ls ->
    Runtime.CrossRT (List.map ~f:rt_of_t ls)
  | Ast_utils.ArrowT(r,ps) ->
    Runtime.ArrowRT (rt_of_t r, List.map ~f:rt_of_t ps)

(* ~~~~~~~~~~~~~~~~~~~ *)
(* FLATTENING FUNCTION *)
(* ~~~~~~~~~~~~~~~~~~~ *)

(* NOTE expressions turn into RVALUES
 * lv represents the LVALUE that the RVALUE
 * get's bound to. In the future this could change if
 * we have different kinds of statements. *)
let rec flatten_expr lv = function

  (* these all turn into STATEMENTS *)
  | TA.TrueE ->
    push_stmt (Bind (lv, Runtime.BoolRT, (ConstantRV TRUE)))

  | TA.FalseE ->
    push_stmt (Bind (lv, Runtime.BoolRT, (ConstantRV FALSE)))

  | TA.IntE i ->
    push_stmt (Bind (lv, Runtime.IntRT, (ConstantRV (INT i))))

  | TA.FloatE f ->
    push_stmt (Bind (lv, Runtime.FloatRT, (ConstantRV (FLOAT f))))

  | TA.VarE(t, vn) -> lookup vn
    >>= fun rhs_var ->
    push_stmt (Bind (lv, rt_of_t t, VarRV rhs_var))

  (* constant tuple and array construction *)
  | TA.CrossE(_t, _es) -> assert false
  | TA.ArrayCE(_t,_es) -> assert false

  (* bin/un-ops cannot cause a panic
   * because integers/floats wrap around *)
  | TA.BinopE(t, lhs, op, rhs) -> fresh_temp ()
    >>= fun tlhs -> fresh_temp ()
    >>= fun trhs ->
    flatten_expr tlhs lhs
    >> flatten_expr trhs rhs
    >> push_stmt (Bind (lv, rt_of_t t, (BinopRV (tlhs, op, trhs))))

  | TA.UnopE(t, op, expr) -> fresh_temp ()
    >>= fun tlv -> flatten_expr tlv expr
    >> push_stmt (Bind (lv, rt_of_t t, (UnopRV (op, tlv))))

  | TA.CastE(_t,expr, ct) -> fresh_temp ()
    >>= fun tlv -> flatten_expr tlv expr
    >> let ty = rt_of_t ct in
    push_stmt (Bind (lv, ty, (CastRV (ty, tlv))))

  | TA.CrossidxE(_t,_expr,_i) -> assert false

  (* array indexing can cause a PANIC when the index
   * is out of bounds. there needs to be a guard here *)
  | TA.ArrayidxE(_t,_expr,_es) -> assert false

  (* loop expressions need to expand into loops
   * using GOTO *)
  | TA.ArrayLE(_t,_bs,_expr) -> assert false
  | TA.SumLE(_t,_bs,_e) -> assert false

  (* AppE/ITE turn into terminators *)
  | TA.IteE(t, cnd, ie, ee) ->
    (* NOTE the `true_block` and the `false_block` *could*
     * have conditionals and re-written blocks. For this,
     * the PHI node needs to get the basic_blocks that
     * actually jump to it, so we need to follow the CFG
     * until a BB jumps to `exit_block`
     *
     * For this reason the true and false blocks are
     * shadowed in this env. *)

    (* generate a tag for each of the new basic_blocks *)
    get_bb () >>= fun curr_block ->
    (* gen new_lvs for each of the exprs *)
    fresh_temp () >>= fun cnd_lv ->
    fresh_temp () >>= fun true_lv ->
    fresh_temp () >>= fun false_lv ->
    (* generate the cnd *)
    flatten_expr cnd_lv cnd
    (* the block is done *)
    (* flatten the true block *)
    >> add_new_bb () >>= fun true_block ->
    set_bb true_block
    >> flatten_expr true_lv ie
    >> get_bb () >>= fun true_block' ->

    (* flatten the false block *)
    add_new_bb () >>= fun false_block ->
    set_bb false_block
    >> flatten_expr false_lv ee
    >> get_bb () >>= fun false_block' ->

    (* finish the basic blocks  *)
    add_new_bb () >>= fun exit_block ->
    set_bb curr_block
    >> modify (flip Env.add_term
                 (Ite { cond = cnd_lv
                      ; if_bb = true_block
                      ; else_bb = false_block
                      ; merge_bb = exit_block }))
    >> set_bb true_block'
    >> modify (flip Env.add_term (Goto exit_block))
    >> set_bb false_block'
    >> modify (flip Env.add_term (Goto exit_block))

    (* resume the exit block *)
    >> set_bb exit_block
    >> let ty = rt_of_t t in
    push_stmt
      (Bind
         (lv, ty
         , (PhiRV { ty = ty
                  ; paths = [(true_lv, true_block')
                            ; (false_lv, false_block')]})))

  | TA.AppE(_t, vn, es) ->
    let rec loop ps acc = match ps with
      | [] -> List.rev acc |> return
      | x :: xs -> fresh_temp () >>= fun newt ->
        flatten_expr newt x
        >> loop xs (newt :: acc)
    in
    loop es [] >>= fun param_lvs ->
    binding_of_vn vn >>= fun fn_lv ->
    add_new_bb () >>= fun next_bb ->
    modify (flip Env.add_term
              (Call { fn_name = fn_lv
                    ; params = param_lvs
                    ; write_to = lv
                    ; success_jump_to = next_bb }))
    (* start the next basic block *)
    >> set_bb next_bb

and flatten_arg = function
  | TA.VarA(_t, _vn) -> assert false
  | TA.ArraybindA(_te, _vn, _vns) -> assert false

and flatten_lvalue = function
  | TA.ArgLV(_te, _arg) -> assert false
  | TA.CrossbindLV(_te, _lvs) -> assert false

and flatten_bind = function
  | TA.ArgB (te, TA.VarA (_, vn)) ->
    let rt = rt_of_t te in
    binding_of_vn vn
    >>= fun lv ->
    modify (fun e -> Env.bind e lv rt)
  | TA.ArgB (_, _) ->
    assert false
  | TA.CrossbindB (te, bds) ->
    ignore te;
    ignore bds;
    assert false

and binding_of_vn vn = get
  >>= fun env -> modify Env.incr_var_count
  >> return (UserBinding (Varname.to_string vn, env.var_count))

and unify_arg_expr arg expr = match arg, expr with
  | TA.VarA (_, vn), expr ->
    binding_of_vn vn
    >>= fun lv ->
    flatten_expr lv expr

  | TA.ArraybindA (_, _base_vn, _vns), _expr ->
    assert false

and unify_lv_expr lv expr = match lv, expr with
  | TA.CrossbindLV (_, _lvs), _cross_e -> assert false
  | ArgLV (_, arg), expr -> unify_arg_expr arg expr

and flatten_stmt = function
  | TA.LetS(lv, expr) ->
    unify_lv_expr lv expr

  | TA.AssertS(_expr, _str) ->
    (* generate a temp var and flatten expr
     * with the temp. *)
    (* generate an if terminator which branches to a
     * panic when false and continue to next BB when
     * true *)
    (* TODO adding in a panic will require unwinding the LLVM stack,
     * this unwinding stack should be kept in the JIR. *)
    assert false

  | TA.ReturnS(_te, expr) -> fresh_int ()
    >>= fun id -> let temp_v = Temp id in
    flatten_expr temp_v expr
    >> modify (flip Env.add_term (Return temp_v))

and flatten_cmd = function
  | TA.ReadimgC(_fn, _arg) ->
    (* reading an image should expand
     * into a function call (for now) *)
    assert false

  | TA.WriteimgC(_expr, _fn) ->
    assert false

  (* TODO not supported at the type level yet *)
  | TA.ReadvidC(_fn, _arg) -> assert false
  | TA.WritevidC(_expr, _fn) -> assert false
  (********************************************)

  | TA.PrintC _str -> assert false

  (* expand into function calls *)
  | TA.ShowC _expr -> assert false
  | TA.TimeC _c -> assert false
  (* ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

  | TA.StmtC s -> flatten_stmt s

  | TA.FnC(te, vn, bs, _te', sts) ->
    let arrow_rt = rt_of_t te in
    get_bb () >>= fun curr_bb ->
    add_new_bb () >>= fun new_bb ->
    set_bb new_bb
    (* open a new scope *)
    >> modify Env.open_scope
    >> map_m_ bs ~f:flatten_bind
    >> map_m_ sts ~f:flatten_stmt
    (* finish_fn ... name fn_sig *)
    >> modify (fun e ->
        Env.finish_fn e (Varname.to_string vn) arrow_rt)
    (* close the new function scope *)
    >> modify Env.close_scope
    >> set_bb curr_bb

(* NOTE creating the JIR should never produce an `Error.t`
 * as the program was already successfully typed. *)
and jir_of_ty (p : TA.prog) : jir Or_error.t =
  exec_state (map_m p ~f:flatten_cmd) (Env.mempty ())
  |> fun env ->
  let (main_fn, glbls) = Env.make_main env in
  Ok { main = main_fn
     ; globals = glbls
     ; prog = env.fns }
