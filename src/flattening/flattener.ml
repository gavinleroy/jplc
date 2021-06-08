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

let time_string =
  "time: "

let gen_new_var () =
  get >>= fun env ->
  let (t, env') = Env.get_unique_var env in
  put env' >> return t

let get_time_call_expr () =
  let info = Runtime.get_time_info in
  AppE (info.return_type, Varname (info.arrow_type, info.name), [])

let new_expr_mod ?(name = "") e =
  if String.is_empty name then
    fun s -> Env.add_new_expr s None e
  else fun s -> Env.add_new_expr s (Some name) e

let extend_env_expr e =
  gen_new_var ()
  >>= fun name -> modify (new_expr_mod ~name e)
  >> return name

let rec flatten_expr = function
  | TA.TrueE -> raise Todo
  | TA.FalseE -> raise Todo
  | TA.IntE i -> extend_env_expr (IntE i)
    >>= fun name -> return (Varname(IntT, name))
  | TA.FloatE f -> extend_env_expr (FloatE f)
    >>= fun name -> return (Varname(FloatT, name))
  | TA.VarE(t, vn) -> get >>= fun env ->
    return <.> (fun s -> Varname (t, s)) <$> Env.lookup env vn
  | TA.CrossE(t, es) -> map_m es ~f:flatten_expr
    >>= fun exprs' -> extend_env_expr (CrossE (t, exprs'))
    >>= fun name -> return (Varname (t, name))
  | TA.ArrayCE(t,es) -> map_m es ~f:flatten_expr
    >>= fun exprs' -> extend_env_expr (ArrayCE (t, exprs'))
    >>= fun name -> return (Varname (t, name))
  | TA.BinopE(t,lhs,op,rhs) -> flatten_expr lhs
    >>= fun lhsvn -> flatten_expr rhs
    >>= fun rhsvn -> extend_env_expr (BinopE (t, lhsvn, op, rhsvn))
    >>= fun name -> return (Varname (t,name))
  | TA.UnopE(t,op,expr) -> flatten_expr expr
    >>= fun vn -> extend_env_expr (UnopE (t, op, vn))
    >>= fun name -> return (Varname (t, name))
  | TA.CastE(_t,_e,_ct) -> raise Todo
  | TA.CrossidxE(_t,_e,_i) -> raise Todo
  | TA.ArrayidxE(_t,_e,_es) -> raise Todo
  | TA.IteE(_t,_cnd,_ie,_ee) -> raise Todo
  | TA.ArrayLE(_t,_bs,_e) -> raise Todo
  | TA.SumLE(_t,_bs,_e) -> raise Todo
  | TA.AppE(_t,_vn,_es) -> raise Todo

let flatten_arg = function
  | TA.VarA(_t,vn) -> `Single (Varname.to_string vn)
  | TA.ArraybindA(_te,vn,vns) ->
    let f = fun vn -> (Varname.to_string vn) in
    let dims = List.map vns ~f in
    let base = f vn in `Array (base, dims)

let rec flatten_lvalue = function
  | TA.ArgLV(_te,arg) -> flatten_arg arg
  | TA.CrossbindLV(_te,lvs) ->
    `Cross (List.map lvs ~f:flatten_lvalue)

(* type binding =
 *   | ArgB of type_expr * arg
 *   | CrossbindB of type_expr * binding list *)

let rec unify_lhs_rhs flv expr =
  match flv with
  | `Single vn -> flatten_expr expr
    >>= fun (Varname (_expr_t, expr_vn)) ->
    modify (fun s -> Env.add_alias s vn expr_vn)
    >> return []
  | `Array (_base, _dims) -> raise Todo
  | `Cross lvs ->
    map_m_ ~f:(fun (lv, expr) -> unify_lhs_rhs lv expr)
    <$> (match expr with
        | TA.CrossE(_te,exprs) -> List.zip_exn lvs exprs
        | _ -> raise Todo)
    >> return []

let flatten_stmt = function
  | TA.LetS(lv, expr) -> unify_lhs_rhs (flatten_lvalue lv) expr
  | TA.AssertS(expr,str) -> flatten_expr expr
    >>= fun vn -> modify (new_expr_mod (AssertE (vn, str)))
    >> return []
  | TA.ReturnS(_te,expr) -> flatten_expr expr
    >>= fun vn -> modify (new_expr_mod (ReturnE vn))
    >> get >>= fun env ->
    let (body, env) = Env.clear_exprs env in
    put env >> return body

let rec flatten_cmd = function
  (* these should be similar *)
  | TA.ReadimgC(_fn, _arg) -> raise Todo
  | TA.ReadvidC(_fn, _arg) -> raise Todo
  (* these should be similar *)
  | TA.WriteimgC(_expr, _fn) -> raise Todo
  | TA.WritevidC(_expr, _fn) -> raise Todo

  | TA.PrintC str -> modify (new_expr_mod (PrintE str))
    >> return []
  | TA.ShowC expr -> flatten_expr expr
    >>= fun vn -> modify (new_expr_mod (ShowE vn))
    >> return []
  | TA.StmtC s -> flatten_stmt s
  (* TODO FIXME clean up the comments *)
  | TA.TimeC c -> gen_new_var ()
    (* NOTE 1. put in a get_time command *)
    >>= fun time_var1 ->
    modify (get_time_call_expr () |> new_expr_mod ~name:time_var1)
    (* 2. flatten the c which will produce this return value *)
    >> flatten_cmd c
    >>= fun ret_val -> gen_new_var ()
    (* 3. put in a get_time command *)
    >>= fun time_var2 ->
    modify (get_time_call_expr () |> new_expr_mod ~name:time_var2)
    >> gen_new_var() >>= fun time_var3 ->
    (*  4. subtract the two times (second - first) *)
    let rt = Runtime.get_time_info.return_type in
    modify (new_expr_mod ~name:time_var3
              (BinopE (rt, Varname (rt, time_var1), Minus, Varname (rt, time_var2))))
    (* 5. print "Time: ..." *)
    >> modify (new_expr_mod (PrintE time_string))
    >> modify (new_expr_mod (ShowE (Varname (rt, time_var3))))
    >> return ret_val
  | TA.FnC(_te, _vn, _bs, _te', _ses) -> raise Todo

let make_main (rb : expr list) : Fn.t =
  { name = Varname (ArrowT (IntT, []), "main")
  ; params = []
  ; body = rb }

let fix_ret_blocks =
  List.rev
  <.> List.hd_exn
  <.> (List.filter ~f:(Option.is_some <.> List.hd))

let flatten_prog (p : TA.prog) : prog Or_error.t =
  run_state (map_m p ~f:flatten_cmd) (Env.mempty ())
  |> fun (ret_blocks, env) ->
  let fns = Env.get_fns env in
  Ok (make_main (fix_ret_blocks ret_blocks) :: fns)
