(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core
open Jir_lang

type partial_bbs = (int, partial_bb, Int.comparator_witness) Map.t

(* TODO reintroduce the optional current BB tag
 * to use when pausing and resuming BBs *)

and t =
  { fns : jir_fn list
  ; bindings : (lvalue * Runtime.runtime_type) list list
  ; stmts : statement list list
  ; bbs : basic_block list list
  ; saved_bbs : partial_bbs
  ; env : (lvalue * lvalue) list list
  ; var_count : int
  ; bb_count : int }

(* a Partial Basic Block can be opened or stored and resumed at a later time *)
and partial_bb =
  { p_stmts : statement list
  ; p_env : (lvalue * lvalue) list
  ; tag : bb_id }



(* TODO account for the provided runtime env *)
let mempty () =
  { env = [[]]
  ; bindings = [[]]
  ; stmts = [[]]
  ; bbs = [[]]
  ; saved_bbs = Map.empty (module Int)
  ; var_count = 0
  ; bb_count = 0
  ; fns = [] }

let mappend _v1 _v2 =
  assert false

let append_to_hd v xs =
  match xs with
  | (x) :: xs' -> (v :: x) :: xs'
  | _ -> [[v]]

let tl_or ls ~default =
  match List.tl ls with
  | Some ls' -> ls'
  | None -> default

let add_alias e lhs rhs =
  let env' = match e.env with
    | scope :: ss -> ((lhs, rhs) :: scope) :: ss
    | [] -> [[(lhs, rhs)]] in
  { e with env = env' }

(* NOTE should never fail if program is typechecked  *)
let lookup e (vn : string) =
  List.find_exn (List.join e.bindings)
    ~f:(function
        | UserBinding (stem, _), _ ->
          String.( = ) stem vn
        | Temp _, _ -> false)
  (* get the LVALUE out of the tuple *)
  |> fst

let bind = append_to_hd

let fresh_var e =
  let newstr = Printf.sprintf "_%%%d" e.var_count in
  newstr, { e with var_count = e.var_count + 1 }

let incr_var_count e =
  { e with var_count = e.var_count + 1 }

let add_fresh_bb e =
  let bbid = e.bb_count in
  let new_bb_c = bbid + 1 in
  let new_saved_bbs = Map.add_exn e.saved_bbs
      ~key:bbid ~data:{ p_stmts = [] ; p_env = [] ; tag = bbid } in
  bbid, { e with  bb_count = new_bb_c
               ; saved_bbs = new_saved_bbs }

let pause_bb ?(cscope = false) e =
  let bbid = e.bb_count in
  let new_bb_c = bbid + 1 in
  let new_saved_bbs = Map.add_exn e.saved_bbs
      ~key:bbid ~data:{ p_stmts = List.hd_exn e.stmts
                      ; p_env = List.hd_exn e.env
                      ; tag = bbid } in
  bbid, { e with bb_count = new_bb_c
               ; saved_bbs = new_saved_bbs
               ; env = if cscope then
                     tl_or ~default:[] e.env
                   else e.env}

let resume_bb ?(oscope = false) e bbid =
  let { p_stmts; p_env; _ } = Map.find_exn e.saved_bbs bbid in
  let new_map = Map.remove e.saved_bbs bbid in
  { e with saved_bbs = new_map
         ; stmts = p_stmts :: e.stmts
         ; env = if oscope then
               p_env :: e.env
             else e.env }

let finish_bb cscope e tag term =
  let stmts_r =  List.hd_exn e.stmts |> List.rev in
  let bb = BB { id = tag
              ; stmts = stmts_r
              ; term = term } in
  { e with stmts = List.tl_exn e.stmts
         ; bbs = append_to_hd bb e.bbs
         ; env = if cscope then
               (tl_or ~default:[] e.env)
             else e.env}

let add_stmt e stmt =
  match stmt with
  | Bind (lv, ty, _) ->
    let nbs = bind (lv, ty) e.bindings in
    { e with stmts = append_to_hd stmt e.stmts
           ; bindings = nbs }

let add_term ?(cscope = false) _e term =
  let (bb_id, e') = assert false in
  finish_bb cscope e' bb_id term

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
