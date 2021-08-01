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
  Printf.printf "Looking for %s\n" s;
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
  | TA.IteE(_t,_cnd,_ie,_ee) ->
    (* stop (or pause) the current basic block COND *)
    (* generate the IF block *)
    (* generate the ELSE block *)
    (* push the blocks in the following order
     * COND ~> IF ~> ELSE *)
    assert false

  | TA.AppE(_t,_vn,_es) -> assert false

and flatten_arg = function
  | TA.VarA(_t, _vn) -> assert false
  | TA.ArraybindA(_te, _vn, _vns) -> assert false

and flatten_lvalue = function
  | TA.ArgLV(_te, _arg) -> assert false
  | TA.CrossbindLV(_te, _lvs) -> assert false

and binding_of_vn vn = get
  >>= fun env -> modify Env.incr_var_count
  >> return (UserBinding (Varname.to_string vn, env.var_count))

and unify_arg_expr arg expr = match arg, expr with
  | TA.VarA (_, vn), expr -> binding_of_vn vn
    >>= fun lv -> flatten_expr lv expr

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

  | TA.FnC(_te, _vn, _bs, _te', _ses) -> assert false

(* NOTE creating the JIR should never produce an `Error.t`
 * as the program was already successfully typed. *)
and jir_of_ty (p : TA.prog) : jir Or_error.t =
  let p = take_until p ~f:(function
      | TA.StmtC (TA.ReturnS _) -> true
      | _ -> false) in
  exec_state (map_m p ~f:flatten_cmd) (Env.mempty ())
  |> fun env ->
  Ok { main = Env.make_main env
     ; prog = [] }
