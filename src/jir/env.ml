(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core
open Jir_lang

(* TODO
 * for the invironment we will push all
 * bindings onto it. THen, when looking
 * something up,m if it was a temp binding
 * just return the value. If it was a user
 * binding, scan the list for the first on
 * with the base stem. *)

type t =
  { fns : jir_fn list
  ; bindings : (lvalue * Runtime.runtime_type) list list
  ; stmts : statement list list
  ; bbs : basic_block list list
  ; env : (lvalue * lvalue) list list
  ; var_count : int
  ; bb_count : int }

(* TODO account for the provided runtime env *)
let mempty () =
  { env = [[]]
  ; bindings = [[]]
  ; stmts = [[]]
  ; bbs = [[]]
  ; var_count = 0
  ; bb_count = 0
  ; fns = [] }

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
(* let lookup e vn =
 *   List.find_exn (List.join e.env)
 *     ~f:(fun tup ->
 *         String.( = ) (fst tup) (Ast_utils.Varname.to_string vn))
 *   |> snd *)

let fresh_var e =
  let newstr = Printf.sprintf "_%%%d" e.var_count in
  newstr, { e with var_count = e.var_count + 1 }

let incr_var_count e =
  { e with var_count = e.var_count + 1 }

let add_stmt e stmt =
  match stmt with
  | Bind (lv, ty, _) ->
    let nbs = append_to_hd (lv, ty) e.bindings in
    { e with stmts = append_to_hd stmt e.stmts
           ; bindings = nbs }

(* adding a terminator means that the basic
 * block is done, so we need to take the statements,
 * reverse them, then push it all into a block *)
let add_term e term =
  let stmts_r =  List.hd_exn e.stmts |> List.rev in
  let bb_id = e.bb_count + 1 in
  let bb = BB { id = bb_id
              ; stmts = stmts_r
              ; term = term } in
  { e with bb_count = bb_id
         ; stmts = List.tl_exn e.stmts
         ; bbs = append_to_hd bb e.bbs }

let tl_or ls ~default =
  match List.tl ls with
  | Some ls' -> ls'
  | None -> default

let finish_fn e name fn_sig =
  let body' = List.hd_exn e.bbs
              |> List.rev |> Array.of_list in
  let fn = { name = name
           ; signature = fn_sig
           ; bindings = List.hd_exn e.bindings
           ; body = body' } in
  { e with bbs = tl_or e.bbs ~default:[]
         (* reset as each function
          * should start with basic blocks
          * from 0 *)
         ; stmts = tl_or e.stmts ~default:[]
         ; bindings = tl_or e.bindings ~default:[]
         ; fns = fn :: e.fns }

let make_main e =
  let env = finish_fn e "main"
      (Runtime.ArrowRT (Runtime.IntRT, [])) in
  List.find_exn env.fns ~f:(fun fn ->
      String.equal fn.name "main")

(* let add_new_expr
 *     (e : t) (name : string option) (expr : expr) : t =
 *   let exprs' = append_to_hd (name, expr)  e.exprs in
 *   { e with exprs = exprs' }
 *
 * let clear_exprs (e : t) : (expr list * t) =
 *   let exprs = List.hd_exn e.exprs
 *               |> List.map
 *                 ~f:(fun (so, e) ->
 *                     match so with
 *                     | Some s -> LetE(Varname (get_expr_type e, s), e)
 *                     | None -> e) in
 *   exprs, { e with exprs = List.tl_exn e.exprs }
 *
 * let open_scope v =
 *   let e = v.env in
 *   let ex = v.exprs in
 *   { v with env = [] :: e
 *          ; exprs = ex }
 *
 * let close_scope v =
 *   let e = v.env in
 *   let ex = v.exprs in
 *   try { v with env = List.tl_exn e
 *              ; exprs = List.tl_exn ex } with
 *   | _ -> v *)

(* let get_fns e =
 *   Set.to_list e.fns *)
