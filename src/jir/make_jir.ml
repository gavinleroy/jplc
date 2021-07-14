(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core

open Jir_lang

module TA = Typing.Ast

type varname = string

(* ~~~~~~~~~~~~~~~~~ *)
(* UTILITY FUNCTIONS *)
(* ~~~~~~~~~~~~~~~~~ *)

(* ~~~~~~~~~~~~~~~~~~~ *)
(* FLATTENING FUNCTION *)
(* ~~~~~~~~~~~~~~~~~~~ *)

(* vn is the var_name that the expr will get bound to
 * when flattening a compiler intermediate is used *)

(* NOTE expressions turn into RVALUES *)
let flatten_expr _vn = function

  (* these all turn into STATEMENTS *)
  | TA.TrueE -> assert false
  | TA.FalseE -> assert false
  | TA.IntE _i -> assert false
  | TA.FloatE _f -> assert false
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
  | TA.ReturnS(_te, _expr) ->
    (* introduce a temp value to bind with the flattened expr
     *  and then return *)
    assert false

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
