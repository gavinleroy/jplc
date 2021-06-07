(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

open Ast_utils
open Ast

exception Todo

(* give a shortcut for the Parsing.Ast module *)
module TA = Typing.Ast

module State = Utils.Functional.State(Env)
module Monadic = Utils.Functional.Utils(State)
open State
open Monadic

(*********************)
(* UTILITY FUNCTIONS *)
(*********************)

(* let gen_new_var () =
 *   get >>= fun env ->
 *   let (t, env') = Env.get_unique_var env in
 *   put env' >> return t *)

(* let expect_vare_exn expr =
 *   match expr with
 *   | VarE _ -> return expr
 *   | _ -> raise (InvalidState
 *                   ("expected VarE but got " ^
 *                    (Sexp_ast.sexp_of_expr expr |> Sexp.to_string))) *)

let new_expr_mod ?(name = "") e =
  if String.is_empty name then
    fun s -> Env.add_new_expr s None e
  else fun s -> Env.add_new_expr s (Some name) e

(* type expr = *)
let flatten_expr = function
  | TA.IntE i -> get
    >>= fun env -> let (name, env) = Env.get_unique_var env in
    put env
    >> modify (new_expr_mod ~name (IntE i))
    >> return (Varname(IntT, name))
  | TA.FloatE _f -> raise Todo
  | TA.TrueE -> raise Todo
  | TA.FalseE -> raise Todo
  | TA.VarE(_t, _vn) -> raise Todo
  | TA.CrossE(_t, _es) -> raise Todo
  | TA.ArrayCE(_t,_es) -> raise Todo
  | TA.BinopE(_t,_lhs,_op,_rhs) -> raise Todo
  | TA.UnopE(_t,_op,_e) -> raise Todo
  | TA.CastE(_t,_e,_ct) -> raise Todo
  | TA.CrossidxE(_t,_e,_i) -> raise Todo
  | TA.ArrayidxE(_t,_e,_es) -> raise Todo
  | TA.IteE(_t,_cnd,_ie,_ee) -> raise Todo
  | TA.ArrayLE(_t,_bs,_e) -> raise Todo
  | TA.SumLE(_t,_bs,_e) -> raise Todo
  | TA.AppE(_t,_vn,_es) -> raise Todo

(* type arg =
 *   | VarA of type_expr * Varname.t
 *   | ArraybindA of type_expr * Varname.t * Varname.t list *)

(* type lvalue =
 *   | ArgLV of type_expr * arg
 *   | CrossbindLV of type_expr * lvalue list *)

(* type binding =
 *   | ArgB of type_expr * arg
 *   | CrossbindB of type_expr * binding list *)

(* let flatten_stmt = function
 *   | TA.LetS(_lv,_e) -> raise Todo
 *   | TA.AssertS(_e,_s) -> raise Todo
 *   | TA.ReturnS(_te,_e) -> raise Todo *)

let flatten_cmd = function
  | TA.ReadimgC(_fn, _arg) -> raise Todo
  | TA.ReadvidC(_fn, _arg) -> raise Todo
  | TA.WriteimgC(_expr, _fn) -> raise Todo
  | TA.WritevidC(_expr, _fn) -> raise Todo
  | TA.PrintC _str -> raise Todo

  | TA.ShowC expr -> flatten_expr expr
    >>= fun vn -> modify (new_expr_mod (ShowE vn))
    >> return []

  | TA.TimeC _c -> raise Todo
  | TA.FnC(_te, _vn, _bs, _te', _ses) -> raise Todo
  | TA.StmtC TA.ReturnS(_,expr) -> flatten_expr expr
    >>= fun vn -> modify (new_expr_mod (ReturnE vn))
    >> get >>= fun env ->
    let (body, env) = Env.clear_exprs env in
    put env >> return body
  | TA.StmtC _s -> raise Todo

let make_main (rb : expr list) : Fn.t =
  { name = Varname (ArrowT (IntT, []), "main")
  ; params = []
  ; body = rb }

let fix_ret_blocks rbs =
  List.filter ~f:(fun l -> List.length l > 0) rbs
  |> List.hd_exn (* NOTE should never throw *)
  |> List.rev

let flatten_prog (p : TA.prog) : prog Or_error.t =
  run_state (map_m p ~f:flatten_cmd) (Env.mempty ())
  |> fun (ret_blocks, env) ->
  let fns = Env.get_fns env in
  Ok (make_main (fix_ret_blocks ret_blocks) :: fns)
