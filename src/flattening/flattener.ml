(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast_utils
open Ast
open Env

module RT : Utils.Functional.MONOID = struct
  type t =
    { fns : (Fn.t, Fn.comparator_witness) Set.t
    ; exprs : (Expr.t, Expr.comparator_witness) Set.t
    ; env : Env.t }

  let mempty () =
    { fns = Set.empty(module Fn)
    ; exprs = Set.empty(module Expr)
    ; env=(Env.empty ()) }

  let mappend
      _ _
      (* { fns1; exprs1; env1; }
       * { fns2; exprs2; env2; } *)
    = mempty ()
end

(* val return: 'a -> 'a t
 * val join: 'a t t -> 'a t
 * val all: ('a t) list -> ('a list) t
 * val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
 * val ( >>| ): 'a t -> ('a -> 'b) -> 'b t *)

module ST = Utils.Functional.State( RT )
open Utils.Functional.Utils( ST )

(* give a shortcut for the Parsing.Ast module *)
module TA = Typing.Ast

(* type expr =
 *   | IntE of Int64.t
 *   | FloatE of float
 *   | TrueE
 *   | FalseE
 *   | VarE of type_expr * Varname.t
 *   | CrossE of type_expr * expr list
 *   | ArrayCE of type_expr * expr list
 *   | BinopE of type_expr * expr * bin_op * expr
 *   | UnopE of type_expr * un_op * expr
 *   | CastE of type_expr * expr
 *   | CrossidxE of type_expr * expr * int
 *   | ArrayidxE of type_expr * expr * expr list
 *   | IteE of type_expr * expr * expr * expr
 *   | ArrayLE of type_expr * (Varname.t * expr) list * expr
 *   | SumLE of type_expr * (Varname.t * expr) list * expr
 *   | AppE of type_expr * Varname.t * expr list *)

(* type arg =
 *   | VarA of type_expr * Varname.t
 *   | ArraybindA of type_expr * Varname.t * Varname.t list *)

(* type lvalue =
 *   | ArgLV of type_expr * arg
 *   | CrossbindLV of type_expr * lvalue list *)

(* type binding =
 *   | ArgB of type_expr * arg
 *   | CrossbindB of type_expr * binding list *)

(* type stmt =
 *   | LetS of lvalue * expr
 *   | AssertS of expr * string
 *   | ReturnS of type_expr * expr *)

let todo e = return (rt_empty e)
let flatten_cmd env = function
  (* TODO FIXME *)
  | TA.ReadimgC(_fn, _arg) -> todo env
  (* TODO FIXME *)
  | TA.ReadvidC(_fn, _arg) -> todo env
  (* TODO FIXME *)
  | TA.WriteimgC(_expr, _fn) -> todo env
  (* TODO FIXME *)
  | TA.WritevidC(_expr, _fn) -> todo env
  (* TODO FIXME *)
  | TA.PrintC _str -> todo env
  (* TODO FIXME *)


  | TA.ShowC _expr ->
    return (rt_empty env)


  (* TODO FIXME *)
  | TA.TimeC _c -> todo env
  (* TODO FIXME *)
  | TA.FnC(_te, _vn, _bs, _te', _ses) -> todo env
  (* TODO FIXME *)
  | TA.StmtC _s -> todo env

(* what are we trying to do?
 * Given a cmd/stmt/... we want to get back
 * a fn_cmds list and a expr list and an updated
 *  env
 *
 * 'a refers to the AST NODE type.  *)

(* p is a list of commands. In our new grammar, we need to return a list of *function commands*
 * where all non-FnCs go into MAIN *)
let flatten_prog (p : TA.prog) =
  ( foldM2 flatten_cmd (Env.empty ()) p
    >>= fun { fns; exprs; _; } ->
    return fns )
  |> Result.Ok
