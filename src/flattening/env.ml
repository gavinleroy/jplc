(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast

type t =
  { fns       : (Fn.t, Fn.comparator_witness) Set.t
  (* NOTE if the string is NONE then
   * we just insert the expr directly into
   * the body (this would be an assert/return)
   * Otherwise, this is a let binding and
   * we expand it out *)
  ; exprs     : (string option * expr) list list
  ; env       : (string * string) list list
  ; new_count : int }

(* TODO account for the provided runtime env *)
let mempty () =
  { env       = [[]]
  ; new_count = 0
  ; fns       = Set.empty(module Fn)
  ; exprs     = [[]] }

let mappend _v1 _v2 =
  assert false

let append_to_hd v xs =
  match xs with
  | (x) :: xs' -> (v :: x) :: xs'
  | _ -> [[v]]

let add_alias e lhs rhs =
  let env' = match e.env with
    | scope :: ss -> ((lhs, rhs) :: scope) :: ss
    | [] -> [[(lhs, rhs)]] in
  { e with env = env' }

(* NOTE should never fail if program is typechecked  *)
let lookup e vn =
  List.find_exn (List.join e.env)
    ~f:(fun tup ->
        String.( = ) (fst tup) (Ast_utils.Varname.to_string vn))
  |> snd

let get_unique_var e =
  let newstr = Printf.sprintf "_t.%d" e.new_count in
  newstr, { e with new_count = e.new_count + 1 }

let add_new_expr
    (e : t) (name : string option) (expr : expr) : t =
  let exprs' = append_to_hd (name, expr)  e.exprs in
  { e with exprs = exprs' }

let clear_exprs (e : t) : (expr list * t) =
  let exprs = List.hd_exn e.exprs
              |> List.map
                ~f:(fun (so, e) ->
                    match so with
                    | Some s -> LetE(Varname (get_expr_type e, s), e)
                    | None -> e) in
  exprs, { e with exprs = List.tl_exn e.exprs }

let open_scope v =
  let e = v.env in
  let ex = v.exprs in
  { v with env = [] :: e
         ; exprs = ex }

let close_scope v =
  let e = v.env in
  let ex = v.exprs in
  try { v with env = List.tl_exn e
             ; exprs = List.tl_exn ex } with
  | _ -> v

let get_fns e =
  Set.to_list e.fns
