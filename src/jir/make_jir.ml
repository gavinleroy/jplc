(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core

open Jir_lang

module TA = Typing.Ast

type varname = string

module State = Utils.Functional.State(Env)
module Monadic = Utils.Functional.Utils(State)
open State
open Monadic

(* ~~~~~~~~~~~~~~~~~ *)
(* UTILITY FUNCTIONS *)
(* ~~~~~~~~~~~~~~~~~ *)

let push_stmt s =
  modify (flip Env.add_stmt s)

(* get the var_count from the
 * environment and add one *)
let fresh_int () = get
  >>= fun env ->
  modify (fun e ->
      { e with var_count = e.var_count + 1 })
  >> return env.var_count

(* ~~~~~~~~~~~~~~~~~~~ *)
(* FLATTENING FUNCTION *)
(* ~~~~~~~~~~~~~~~~~~~ *)

(* NOTE expressions turn into RVALUES
 * lv represents the LVALUE that the RVALUE
 * get's bound to. In the future this could change if
 * we have different kinds of statements. *)
let flatten_expr lv = function

  (* these all turn into STATEMENTS *)
  | TA.TrueE -> push_stmt (Bind (lv, (ConstantRV TRUE)))
  | TA.FalseE -> push_stmt (Bind (lv, (ConstantRV FALSE)))
  | TA.IntE i -> push_stmt (Bind (lv, (ConstantRV (INT i))))
  | TA.FloatE f -> push_stmt (Bind (lv, (ConstantRV (FLOAT f))))
  (* In what scenario is this happening *)
  | TA.VarE(_t, _vn) -> assert false

  (* constant tuple and array construction *)
  | TA.CrossE(_t, _es) -> assert false
  | TA.ArrayCE(_t,_es) -> assert false

  (* bin/un-ops cannot cause a panic
   * because integers/floats wrap around *)
  | TA.BinopE(_t,_lhs,_op,_rhs) -> assert false
  | TA.UnopE(_t,_op,_expr) -> assert false

  | TA.CastE(_t,_expr,_ct) -> assert false
  | TA.CrossidxE(_t,_expr,_i) -> assert false

  (* array indexing can cause a PANIC when the index
   * is out of bounds. there needs to be a guard here *)
  | TA.ArrayidxE(_t,_expr,_es) -> assert false

  (* loop expressions need to expand into loops
   * using GOTO *)
  | TA.ArrayLE(_t,_bs,_expr) -> assert false
  | TA.SumLE(_t,_bs,_e) -> assert false

  (* AppE/ITE turn into terminators *)
  | TA.IteE(_t,_cnd,_ie,_ee) -> assert false
  | TA.AppE(_t,_vn,_es) -> assert false

let flatten_arg = function
  | TA.VarA(_t, _vn) -> assert false
  | TA.ArraybindA(_te, _vn, _vns) -> assert false

let flatten_lvalue = function
  | TA.ArgLV(_te, _arg) -> assert false
  | TA.CrossbindLV(_te, _lvs) -> assert false

let flatten_stmt = function
  | TA.LetS(_lv, _expr) ->
    assert false

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
    (* we want to push the return terminator to the
     * environment, which will in turn trigger creating
     * a new basic_block. *)
    >> assert false

let flatten_cmd = function
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
  | TA.StmtC _s -> assert false
  | TA.TimeC _c -> assert false
  (* ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

  | TA.FnC(_te, _vn, _bs, _te', _ses) -> assert false

let flatten_prog (_p : TA.prog) : jir Or_error.t =
  assert false
