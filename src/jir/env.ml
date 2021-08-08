(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core
open Jir_lang

type partial_bbs = (int, partial_bb, Int.comparator_witness) Map.t

and t =
  (* the list of functions that represent
   * a JPL module *)
  { fns : jir_fn list
  (* the current list of bindings for a function
   * that will need space allocated in LLVM *)
  ; bindings : (lvalue * Runtime.runtime_type) list list
  (* the list of statements that will go into a
   * basic block when it is fully formed *)
  ; stmts : statement list list
  (* the current list of basic blocks *)
  ; bbs : basic_block list list
  (* the current Basic Block name if it exists *)
  ; curr_bb : bb_id option
  (* stored Basic Blocks that are unfinished *)
  ; saved_bbs : partial_bbs
  (* the environment that gives new aliases
   * for flattened variables *)
  ; env : (lvalue * lvalue) list list
  (* the total variables count *)
  ; var_count : int
  (* the total Basic Block count *)
  ; bb_count : int }

(* TODO notes
 * 1. What if we stored all basic blocks in a map rather than a list
 *    - you could bind to a finished or partial block and if
 *      an insertion attempt is made for the finished block just
 *      crash the program
 * 2. Adding a terminator to the list will still close out the
 *    basic block but it will no longer start another right away
 *    meaning that we can still access the name of the bb once it's
 *    finished.
 * 3. Some things will be a little more manual than they were before
 *    e.g. setting the current block *)

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
  ; curr_bb = None
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

let tl_or ?(default = []) ls =
  match List.tl ls with
  | Some ls' -> ls'
  | None -> default

let hd_or ?(default = []) ls =
  match List.hd ls with
  | Some f -> f
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

let get_bb_tags e =
  match e.curr_bb with
  | Some bbid -> bbid, e.bb_count
  | None -> e.bb_count, e.bb_count + 1

let add_fresh_bb e =
  let bbid = e.bb_count in
  let new_bb_c = bbid + 1 in
  let new_saved_bbs = Map.add_exn e.saved_bbs
      ~key:bbid ~data:{ p_stmts = [] ; p_env = [] ; tag = bbid } in
  bbid, { e with  bb_count = new_bb_c
               ; saved_bbs = new_saved_bbs }

let pause_bb ?(cscope = false) e =
  let (bbid, new_bb_c) = get_bb_tags e in
  let new_saved_bbs = Map.add_exn e.saved_bbs
      ~key:bbid ~data:{ p_stmts = List.hd_exn e.stmts
                      ; p_env = List.hd_exn e.env
                      ; tag = bbid } in
  bbid, { e with bb_count = new_bb_c
               ; saved_bbs = new_saved_bbs
               ; env = if cscope then
                     tl_or ~default:[] e.env
                   else e.env }

let resume_bb ?(oscope = false) e bbid =
  match e.curr_bb with
  | Some _ -> assert false
  | None ->
    let { p_stmts; p_env; _ } = Map.find_exn e.saved_bbs bbid in
    let new_map = Map.remove e.saved_bbs bbid in
    { e with saved_bbs = new_map
           ; stmts = p_stmts :: e.stmts
           ; curr_bb = Some bbid
           ; env = if oscope then
                 p_env :: e.env
               else e.env }

let finish_bb cscope e tag term =
  let stmts_r =  hd_or e.stmts |> List.rev in
  let bb = BB { id = tag
              ; stmts = stmts_r
              ; term = term } in
  { e with stmts = tl_or e.stmts
         ; bbs = append_to_hd bb e.bbs
         ; curr_bb = None
         ; env = if cscope then
               (tl_or ~default:[] e.env)
             else e.env}

let add_stmt e stmt =
  match stmt with
  | Bind (lv, ty, _) ->
    let nbs = bind (lv, ty) e.bindings in
    { e with stmts = append_to_hd stmt e.stmts
           ; bindings = nbs }

let add_term ?(cscope = false) e term =
  let (bbid, new_bb_c) = get_bb_tags e in
  finish_bb cscope
    { e with bb_count = new_bb_c }
    bbid term

let finish_fn e name fn_sig =
  let body' = List.hd_exn e.bbs
              |> List.rev in
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

(* Utility Functions *)

(* let jumps_to = function
 *   | Goto bb_id -> `BB bb_id
 *   | Ite { cond
 *         ; if_bb
 *         ; else_bb
 *         ; merge_bb } ->
 *     ignore cond;
 *     ignore merge_bb;
 *     `Many [if_bb; else_bb]
 *   | Return _lv -> `Return
 *
 * let jumps_to (BB { id; stmts; term }) =
 *   ignore id;
 *   ignore stmts;
 *   jumps_to term
 *
 * let rec crawl_bb bb (k : bb_id -> bb_id -> bool) (env : t) =
 *   (\* func to peel of BB layer and get the id *\)
 *   let
 *     bbid (BB b) = b.id
 *   in
 *   match jumps_to bb with
 *   | `BB id ->
 *     if k (bbid bb) id then
 *       Some (bbid bb)
 *     else
 *       crawl id k env
 *
 *   (\* we don't know who the caller is but the crawl has ended *\)
 *   | `Return ->
 *     None
 *
 *   | `Many ids ->
 *     let curr_id = bbid bb in
 *     List.fold_left ids ~init:None ~f:(fun acc v ->
 *         if k curr_id v then
 *           Some curr_id
 *         else
 *           (match acc, crawl v k env  with
 *            | Some _, None -> acc
 *            | None, Some v -> Some v
 *            | Some a, Some v ->
 *              (\* If crawling ended up in two separate places and
 *               * never merged this is an error *\)
 *              if a <> v then
 *                assert false
 *              else acc
 *            | _, _ -> None))
 *
 * and crawl tag k env =
 *   let get_bb id =
 *     let bbs = List.join env.bbs in
 *     List.find_exn bbs ~f:(fun (BB bb) -> bb.id = id)
 *   in
 *   crawl_bb (get_bb tag) k env *)
