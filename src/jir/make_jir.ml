(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

(* FIXME remove *)
[@@@warning "-27"]

open Jir_tree
module AU = Ast_utils
module C = Jir_high
module TA = Typing.Ast

let rec jir_of_expr expr ctx = match expr with
  | TA.IntE i ->
    ctx (C.AtomL (IntLit i))
  | TA.FloatE f ->
    ctx (C.AtomL (FloatLit f))
  | TA.TrueE ->
    ctx (C.AtomL (BoolLit true))
  | TA.FalseE ->
    ctx (C.AtomL (BoolLit false))
  | TA.VarE (te, vn) ->
    ctx (C.AtomN (Symbol.of_string (AU.Varname.to_string vn)))
  | TA.CrossE (te, es)
    -> assert false
  | TA.ArrayCE (te, es)
    -> assert false
  | TA.BinopE (te, lhs, op, rhs)
    -> assert false
  | TA.UnopE (te, op, e)
    -> assert false
  | TA.CastE (te_from, e, te_to) (* TODO check this *)
    -> assert false
  | TA.CrossidxE (te, e, i)
    -> assert false
  | TA.ArrayidxE (te, e, es)
    -> assert false
  | TA.IteE (te, cnd, if_e, else_e)
    -> assert false
  | TA.ArrayLE (te, vnes, e)
    -> assert false
  | TA.SumLE (te, vnes, e)
    -> assert false
  | TA.AppE (te, vn, params)
    -> assert false

and jir_of_arg arg = match arg with
  | TA.VarA (te, vn)
    -> assert false
  | TA.ArraybindA (te, vn, vns)
    -> assert false

and jir_of_lvalue lvalue = match lvalue with
  | TA.ArgLV (te, a)
    -> assert false
  | TA.CrossbindLV (te, lvls)
    -> assert false

and jir_of_binding binding = match binding with
  | TA.ArgB (te, a)
    -> assert false
  | TA.CrossbindB (te, bs)
    -> assert false

and jir_of_stmt stmt = match stmt with
  | TA.LetS (lv, e)
    -> assert false
  | TA.AssertS (e, str)
    -> assert false
  | TA.ReturnS (te, e)
    -> assert false


and jir_of_cmds cmds ctx =

  let jir_of_cmd cmd body = match cmd with
    | TA.ReadimgC (fn, arg)
      -> assert false
    | TA.ReadvidC (fn, arg)
      -> assert false
    | TA.WriteimgC (expr, fn)
      -> assert false
    | TA.WritevidC (expr, fn)
      -> assert false
    | TA.PrintC str
      -> assert false
    | TA.ShowC expr
      -> assert false
    | TA.TimeC c
      -> assert false
    | TA.FnC (te, vn, bnds, ret_te, stmts)
      -> assert false
    | TA.StmtC stmt
      -> assert false
  in

  match cmds with
  | cmd :: cmds ->
    jir_of_cmd cmd (jir_of_cmds cmds)
  | [] ->
    assert false
(* C.Halt (C.AtomL 0) *)

let jir_of_ty (_p : TA.prog) : C.tree =
  C.Halt (C.AtomL (IntLit 0L))

(* exec_state (map_m p ~f:flatten_cmd) (Env.mempty ())
 * |> fun env ->
 * let (main_fn, glbls) = Env.make_main env in
 * Ok { main = main_fn
 *    ; globals = glbls
 *    ; prog = env.fns } *)

[@@@warning "+27"]
