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

let vn_to_symbol vn =
  AU.Varname.to_string vn
  |> Symbol.of_string

let rec jir_of_seq ?(ts = []) transformer vs ctx  =
  match vs with
  | [] -> ctx (List.rev ts)
  | hd :: tl ->
    transformer hd (fun v_i ->
        jir_of_seq ~ts:(v_i :: ts) transformer tl ctx)

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
    ctx (C.AtomN (vn_to_symbol vn))

  | TA.CrossE (te, es)
    -> assert false
  | TA.ArrayCE (te, es)
    -> assert false

  | TA.BinopE (te, lhs, `Lt, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Gt, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Cmp, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Lte, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Gte, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Neq, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Mul, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Div, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Mod, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Plus, rhs)
    -> assert false
  | TA.BinopE (te, lhs, `Minus, rhs)
    -> assert false

  | TA.UnopE (te, `Bang, e)
    -> assert false
  | TA.UnopE (te, `Neg, e)
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

  | TA.AppE (te, vn, params) ->
    let fname = vn_to_symbol vn in
    jir_of_seq jir_of_expr params (fun args ->
        let k = Symbol.fresh ~stem:"k" ()
        and r = Symbol.fresh ~stem:"atom" () in
        C.LetC { cnts = [ { name = k
                          ; args = [ r ]
                          ; body = ctx (C.AtomN r) } ];
                 body = C.AppF { lambda = C.AtomN(fname)
                               ; ret_c = k
                               ; args = args }})

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
  match cmds with
  | TA.ReadimgC (fn, arg) :: cmds
    -> assert false
  | TA.ReadvidC (fn, arg) :: cmds
    -> assert false
  | TA.WriteimgC (expr, fn) :: cmds
    -> assert false
  | TA.WritevidC (expr, fn) :: cmds
    -> assert false
  | TA.PrintC str :: cmds
    -> assert false
  | TA.ShowC expr :: cmds
    -> assert false
  | TA.TimeC c :: cmds
    -> assert false
  | TA.FnC (te, vn, bnds, ret_te, stmts) :: cmds
    -> assert false
  | TA.StmtC stmt :: cmds
    -> assert false
  | [] ->
    C.Halt (C.AtomL (IntLit 0L))

let jir_of_ty (p : TA.prog) : C.tree =
  jir_of_cmds p (fun _ -> C.Halt (C.AtomL (IntLit 0L)))

[@@@warning "+27"]
