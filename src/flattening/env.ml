(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast
open Ast_utils

exception Impossible of string

type t =
  { fns       : (Fn.t, Fn.comparator_witness) Set.t
  (* NOTE if the string is NONE then
   * we just insert the expr directly into
   * the body (this would be an assert/return)
   * Otherwise, this is a let binding and
   * we expand it out *)
  ; exprs     : (string option * expr) list
  ; seen_syms : (string, int, String.comparator_witness) Map.t
  ; env       : (string * string) list list
  ; new_count : int }

(* TODO account for the provided runtime env *)
let mempty () =
  { seen_syms = Map.empty (module String)
  ; env       = [[]]
  ; new_count = 0
  ; fns       = Set.empty(module Fn)
  ; exprs     = [] }

let mappend _v1 _v2 =
  raise (Impossible "mappend for flattened Env unimplemented")

let add_alias e lhs rhs =
  let env' = match e.env with
    | scope :: ss -> ((lhs, rhs) :: scope) :: ss
    | [] -> [[(lhs, rhs)]] in
  { e with env = env' }

let add_symbol e vn =
  let vn = Varname.to_string vn in
  let m = Map.update e.seen_syms vn
      (* default return value is 0 for NONE case *)
      ~f:(fun vo -> (Option.value ~default:(-1) vo) + 1) in
  let c = Map.find_exn m vn in
  let newstr = Printf.sprintf "%s.%d" vn c in
  let env' = match e.env with
    | scope :: ss -> ((vn, newstr) :: scope) :: ss
    | [] -> [[(vn, newstr)]] in
  newstr, { e with seen_syms = m; env = env' }

(* NOTE should never fail if program is typechecked  *)
let lookup e vn =
  List.find_exn (List.join e.env)
    ~f:(fun tup ->
        String.( = ) (fst tup) (Varname.to_string vn))
  |> snd

let get_unique_var e =
  let newstr = Printf.sprintf "_t.%d" e.new_count in
  newstr, { e with new_count = e.new_count + 1 }

let add_new_expr
    (e : t) (name : string option) (expr : expr) : t =
  let exprs' = (name, expr) :: e.exprs in
  { e with exprs = exprs' }

let clear_exprs (e : t) : (expr list * t) =
  let exprs = List.map e.exprs
      ~f:(fun (so, e) ->
          match so with
          | Some s -> LetE(Varname (IntT, s), e)
          | None -> e) in
  exprs, { e with exprs = [] }

let open_scope v =
  let e = v.env in
  { v with env = [] :: e }

let close_scope v =
  let e = v.env in
  try { v with env = List.tl_exn e } with
  | _ -> v

let get_fns e =
  Set.to_list e.fns
