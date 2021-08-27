(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Jir_lang

module IntMap = Map.Make(Int)

type 'a scoped_map = (IntMap.Key.t, (int * 'a) list, IntMap.Key.comparator_witness) Map.t

(* bindings are made in a certain scope (functional scope)
 * the environment will be help in a specific scope
 * the basic blocks will be  *)

and t =
  (* the list of functions that represent
   * a JPL module *)
  { fns : jir_fn list
  (* Map of Basic Blocks, either `Done or `Partial *)
  ; bbs : bb_variants scoped_map
  ; bindings : (lvalue * Runtime.runtime_type) scoped_map
  ; env : lvalue scoped_map
  (* the current Basic Block name if it exists *)
  ; curr_bb : bb_id option
  (* the total variables count *)
  ; var_count : int
  (* the total Basic Block count *)
  ; bb_count : int
  (* the number of scopes made *)
  ; scope_count : int }

(* a Partial Basic Block can be opened or stored and resumed at a later time *)
and partial_bb =
  { p_stmts : statement list
  ; p_env : (lvalue * lvalue) list
  ; tag : bb_id }

and bb_variants =
  [ `Done of basic_block
  | `Partial of partial_bb ]

(* TODO account for the provided runtime env *)
let rec mempty () =
  { env = IntMap.empty
  ; bindings = IntMap.empty
  ; bbs = IntMap.empty
  ; curr_bb = None
  ; var_count = 0
  ; bb_count = 0
  ; scope_count = 0
  ; fns = [] }
  |> add_fresh_bb
  |> (fun (bbid, e) ->
      set_bb e bbid)

and mappend _v1 _v2 =
  assert false

and hash_lvalue = function
  | Symbol str
  | UserBinding (str, _) ->
    String.hash str
  | Temp i ->
    Printf.sprintf "_t.%d" i
    |> String.hash

and open_scope e =
  { e with scope_count = e.scope_count + 1 }

and close_scope e =
  { e with scope_count = e.scope_count - 1 }

(* sort a list of scoped items by descending scope *)
and sort_scoped ?(desc = true) ls =
  List.stable_sort ls ~compare:(fun (a, _) (a', _) ->
      Int.compare a a')
  |> (if desc then List.rev else ident)

(* NOTE should never fail if program is typechecked  *)
and lookup e (vn : string) =
  String.hash vn
  |> Map.find_multi e.bindings
  |> sort_scoped
  |> List.hd_exn
  |> snd (* remove the scope int *)
  |> fst (* return the LVALUE *)


and fresh_var e =
  let newstr = Printf.sprintf "_%%%d" e.var_count in
  newstr, { e with var_count = e.var_count + 1 }

and incr_var_count e =
  { e with var_count = e.var_count + 1 }

and get_bb_tags e =
  e.bb_count, e.bb_count + 1
(* match e.curr_bb with
 * | Some bbid -> bbid, e.bb_count + 1
 * | None -> e.bb_count, e.bb_count + 1 *)

and add_fresh_bb e =
  let (bbid, new_bb_c) = get_bb_tags e in
  let data = (e.scope_count , `Partial { p_stmts = []
                                       ; p_env = []
                                       ; tag = bbid }) in
  let new_bbs = Map.add_multi e.bbs
      ~key:bbid
      ~data:data in
  bbid, { e with bb_count = new_bb_c; bbs = new_bbs }

and get_curr_bb e =
  match e.curr_bb with
  | Some bbid -> bbid
  | None ->
    Printf.printf "expected current bb but None was found";
    assert false

and get_partial_bb e id =
  match Map.find_multi e.bbs id with
  | [(_, `Partial pbb)] ->
    pbb
  | [(_, `Done _)] ->
    Printf.printf
      "looking for a partial bb but a finished one was found";
    assert false
  | _ :: _ ->
    Printf.printf
      "bbid was not unique when looking for a partial bb";
    assert false
  | _ ->
    Printf.printf
      "an error occurred when looking for partial bb";
    assert false


and set_bb ?(oscope = false) e bbid =
  ignore oscope;
  (* TODO REMOVE
   *  for safetly check that the BBID exists in our
   * environment and that it is partial *)
  ignore (get_partial_bb e bbid : partial_bb);
  { e with curr_bb = Some bbid }

and finish_bb cscope e bbid term =
  ignore cscope;
  (* we must be finishing the current basic block *)
  assert (bbid = Option.value_exn e.curr_bb);
  let { p_stmts
      ; p_env
      ; tag } = get_partial_bb e bbid in
  ignore p_env;
  assert (bbid = tag);
  let finished_bb = BB { id = bbid
                       ; stmts = List.rev p_stmts
                       ; term = term } in
  let without = Map.remove_multi e.bbs bbid in
  let with_bb = Map.add_multi without ~key:bbid
      ~data:(e.scope_count, `Done finished_bb) in
  { e with bbs = with_bb }

and bind e lv rt =
  let bindings' = Map.add_multi e.bindings
      ~key:(hash_lvalue lv)
      ~data:(e.scope_count, (lv, rt))
  in { e with bindings = bindings' }

and add_stmt e stmt =
  let bbid = get_curr_bb e in
  let e_w_bindings' = (match stmt with
      | Bind (lv, rt, _rv) ->
        bind e lv rt)
  in
  let new_map = Map.update e.bbs bbid ~f:(function
      | Some [(sc, `Partial pbb)] ->
        [(sc, `Partial
            { pbb with
              p_stmts = stmt :: pbb.p_stmts })]
      (* problems *)
      | None ->
        Printf.printf "inserting a stmt into a BB that doesn't exist";
        assert false
      | Some [(_sc, `Done _bb)] ->
        Printf.printf "Done bb found for BB.%d" bbid;
        assert false
      | Some (_ :: _) ->
        Printf.printf "multiple entries in bb hash!";
        assert false
      | Some [] ->
        Printf.printf "empty list found!";
        assert false)
  in { e_w_bindings' with bbs = new_map }

and add_term ?(cscope = false) e term =
  finish_bb cscope e (get_curr_bb e) term

(* TODO clean up this function and
 *  potentially make it faster *)
and finish_fn e name fn_sig =
  let body' = Map.filter e.bbs ~f:(function
      | [(sc, `Done _)] ->
        sc = e.scope_count
      | _ -> false)
              |> Map.to_alist
              |> List.map ~f:(fun t ->
                  snd t |> List.hd_exn |> snd)
              |> List.filter ~f:(function
                  | `Done _ -> true
                  | `Partial _ -> false)
              |> List.map ~f:(function
                  | `Done bb -> bb
                  | `Partial _ -> assert false)
  in
  let bindings' = Map.filter e.bindings ~f:(function
      | [(sc, _)] ->
        sc = e.scope_count
      | _ -> false)
                  |> Map.to_alist
                  |> List.map ~f:(fun t ->
                      snd t |> List.hd_exn |> snd)
  in
  let fn = { name = name
           ; signature = fn_sig
           ; bindings = bindings'
           ; body = body' } in
  (* TODO increment the current scope (or bring back a saved one) *)
  (* FIXME HACK PROBLEM
   * because function calls are not supported right now
   * we don't need to worry about preparing for a new function
   * but that will change SOON *)
  { e with (* filter the BBS *)
    (* reset as each function
     * should start with basic blocks
     * from 0 *)
    (* filter the bindings *)
    fns = fn :: e.fns }

and make_main e =
  let match_name lv f = match lv, f.name with
    | Symbol l, Symbol r -> String.(=) l r
    | UserBinding (sl, il), UserBinding (sr, ir) ->
      String.(=) sl sr && il = ir
    | Temp l, Temp r -> l = r
    | _, _ -> false
  in
  let env = finish_fn e (Symbol "main")
      (Runtime.ArrowRT (Runtime.IntRT, [])) in
  let mn = List.find_exn env.fns
      ~f:(match_name (Symbol "main")) in

  (* separate the global from the local bindings *)
  let match_userbnd (lv, _) = match lv with
    | UserBinding (t, i) ->
      not (List.exists e.fns
             ~f:(match_name (UserBinding (t, i))))
    | Symbol _ | Temp _ -> false
  in

  let globals = List.filter mn.bindings
      ~f:match_userbnd in

  let locals = List.filter mn.bindings
      ~f:(fun (lv, _) -> match lv with
          | UserBinding _
          | Symbol _ -> false
          | _ -> true) in

  { mn with bindings = locals }, globals

and get_prog e =
  List.filter e.fns ~f:(fun f ->
      match f.name with
      | Symbol "main" -> false
      | __ -> true)
