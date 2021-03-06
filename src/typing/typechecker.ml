(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Ast_utils
open Utils
open Parsing.Ast

(* some shorthand module names *)
module F = Utils.Functional
module TA = Ast
module ErrorStateT = F.StateT(Env)(F.Or_error)
module Monadic = F.Utils(ErrorStateT)
(* Open the StateT module and Utils module for map_m and map_m_ *)
open ErrorStateT (* NOTE this is a (StateT Env.t Ether a)  *)
open Monadic

(*************************************)
(********* UTILITY FUNCITONS *********)
(*************************************)

let cerr_msg ~pos ~t ~msg =
  lift (Err.cerr_msg ~pos ~t ~msg)

(* expect type `te` to be of type `expr` *)
let expect pos (f : 'a -> type_expr) (exp : type_expr) (te : 'a) =
  let t = f te in
  if not (exp = t) then
    cerr_msg ~pos:pos ~t:"type"
      ~msg:(Printf.sprintf "expected type %s but got %s"
              (type_to_s exp) (type_to_s t))
  else return te

let expect_equal pos f lhs rhs =
  let tel = f lhs in
  expect pos f tel rhs

(* expect type `te`  to be one of the given exps *)
let expect_or pos (f : 'a -> type_expr) (exps : type_expr list) (te : 'a) : 'a t =
  let t = f te in
  if List.exists (fun t' -> t = t') exps then
    return te
  else
    cerr_msg ~pos:pos ~t:"type"
      ~msg:(Printf.sprintf "expected one of the following types: %s but got %s"
              (type_list_to_s exps) (type_to_s t))

let expect_e p te e =
  expect p TA.extract_expr_type te e

let expect_or_e pos ts e =
  expect_or pos TA.extract_expr_type ts e

(* lookup Varname.t in the environment and return an error if not bound *)
let lookup_err (p: loc) (vn: Varname.t) : type_expr ErrorStateT.t =
  get >>= fun env ->
  match Env.lookup env vn with
  | None -> Err.cerr_msg ~pos:p ~t:"type"
              ~msg:(Printf.sprintf "unbounded symbol '%s'" (Varname.to_string vn)) |> lift
  | Some te -> return te

let unify_arg arg t =
  let modifier = fun vn t ->
    fun s -> Env.extend s vn t in
  match arg with
  | VarA(_,vn) -> modify (modifier vn t)
    >>| fun () -> TA.VarA(t,vn)
  | ArraybindA (l,vn,vns) ->
    (match t with
     | ArrayT(_,r) -> let vnsl = Int64.of_int (List.length vns) in
       if Int64.equal vnsl r then
         map_m_ vns ~f:(fun v ->
             modify (modifier v IntT))
         >> modify (modifier vn t)
         >> return (TA.ArraybindA (t, vn, vns))
       else cerr_msg ~pos:l ~t:"type"
           ~msg:(Printf.sprintf "binding a rank %d array with %d dimensions"
                   (Int64.to_int r) (Int64.to_int vnsl))
     | _ -> cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "expected ArrayT but got %s"
                      (type_to_s t)))

let rec unify_lvalue lv t =
  match lv with
  | ArgLV(_,a) -> unify_arg a t
    >>| fun a' -> TA.ArgLV(t,a')
  | CrossbindLV(l,lvs) ->
    (match t with
     | CrossT tes ->
       (match list_zip lvs tes with
        | `Ok lvstes ->
          map_m lvstes ~f:(fun (lv, te) -> unify_lvalue lv te)
          >>| fun lvs' -> TA.CrossbindLV(t, lvs')
        | `Unequal_lengths -> cerr_msg ~pos:l ~t:"type"
                                ~msg:(Printf.sprintf
                                        "expected tuple of %d elements but got %d"
                                        (List.length lvs) (List.length tes)))
     | _ -> cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf
                      "expected tuple right-hand-side but got %s"
                      (type_to_s t)))

(*****************************************)
(********* TYPECHECKER FUNCITONS *********)
(*****************************************)

let rec type_expr (e : expr) : TA.expr ErrorStateT.t =
  (* case statement for typechecking exprs *)
  let type_expr_expect_int = fun e -> type_expr e
    >>= expect_e (extract_expr_pos e) IntT in
  (* given an environment and a loop bind (vn * expr)
   * typecheck all exprs and expect and IntT then
   * bind all of the types to the Varnames *)
  let type_loopbind bs =
    let exs = List.map snd bs in
    map_m exs ~f:type_expr_expect_int
    >>= fun es' ->
    let tes = List.map TA.extract_expr_type es' in
    map_m_ ~f:(uncurry unify_arg)
      (List.map2 (fun (a,_) t ->
           VarA(Lexing.dummy_pos, a), t) bs tes)
    >> return (List.map2 (fun (a,_) e' ->
        a,e') bs es') in
  match e with
  | IntE(_,i) -> return (TA.IntE i)
  | FloatE(_,f) -> return (TA.FloatE f)
  | TrueE _ -> return TA.TrueE
  | FalseE _ -> return TA.FalseE
  | VarE(l,vn) -> lookup_err l vn
    >>= fun t -> return (TA.VarE(t,vn))
  | CrossE(_,es) ->
    map_m es ~f:type_expr
    >>= fun es' ->
    let t = CrossT (List.map TA.extract_expr_type es') in
    return (TA.CrossE(t, es'))
  | ArrayCE(l,es) ->
    map_m es ~f:type_expr
    >>= fun es' ->
    let tes = List.map TA.extract_expr_type es' in
    if not (all_equal tes ~equal:( = )) then
      cerr_msg ~pos:l ~t:"type" ~msg:"array types must all be equal"
    else let arrt = ArrayT(
        list_hd tes |> Option.value ~default:Unit
      , Int64.of_int 1) in
      return (TA.ArrayCE(arrt,es'))



  | BinopE(l,lhs,op,rhs) ->
    let type_boolop = fun ts ort ->
      type_expr lhs
      >>= expect_or_e l ts
      >>= fun lhs' -> type_expr rhs
      >>= expect_equal l TA.extract_expr_type lhs'
      >>| fun rhs' ->
      TA.BinopE(
        Option.value ort ~default:(TA.extract_expr_type rhs')
      , lhs', op, rhs')
    in
    (match op with
     | `Lt | `Gt | `Lte | `Gte | `Cmp | `Neq ->
       type_boolop [IntT; FloatT] (Some BoolT)
     | `Mul | `Div | `Mod | `Plus | `Minus ->
       type_boolop [IntT; FloatT] None)




  | UnopE(l,op,e) ->
    let type_unop = fun ts -> type_expr e
      >>= expect_or_e l ts
      >>| fun e' ->
      TA.UnopE(TA.extract_expr_type e', op, e')
    in
    (match op with
     | `Bang -> type_unop [BoolT]
     | `Neg -> type_unop [IntT; FloatT])
  | CastE(l,e,t) ->
    type_expr e >>= fun e' ->
    let et = TA.extract_expr_type e' in
    (match et,t with
     (* | BoolT, IntT | IntT, BoolT (* TODO remove this comment for casting booleans *)
      * | BoolT, FloatT | FloatT, BoolT *)
     | IntT, FloatT | FloatT, IntT -> return (TA.CastE(et, e',t))
     | _,_ ->
       cerr_msg ~pos:l ~t:"type"
         ~msg:(Printf.sprintf
                 "cannot convert expression of type %s to type %s"
                 (type_to_s et) (type_to_s t)))
  | CrossidxE(l,e,i) ->
    type_expr e >>= fun e' ->
    let t = TA.extract_expr_type e' in
    (match t with
     | CrossT tes ->
       (try let iint = (Int64.to_int i) in
          (try let ti = List.nth tes iint in
             return (TA.CrossidxE(ti,e',iint))
           with Failure _ ->
             cerr_msg ~pos:l ~t:"type"
               ~msg:(Printf.sprintf "index %d out of tuple range" iint))
        with _ -> cerr_msg ~pos:l ~t:"type"
                    ~msg:(Printf.sprintf "tuple sizes greater than %d unsupported" Int.max_int))
     | _ -> cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "expected base type of CrossT but got %s"
                      (type_to_s t)))
  | ArrayidxE(l,base,idxs) ->
    (* typecheck the base and make sure it is ArrayT *)
    type_expr base >>= fun base' ->
    let tb = TA.extract_expr_type base' in
    (match tb with
     | ArrayT(baset,r) ->
       if Int64.equal r (List.length idxs |> Int64.of_int) then
         map_m idxs ~f:type_expr_expect_int
         >>| fun idxs' ->
         TA.ArrayidxE(baset, base', idxs')
       else cerr_msg ~pos:l ~t:"type"
           ~msg:(Printf.sprintf
                   "incorrect number of indexes provided for array of rank %d"
                   (Int64.to_int r))
     | _ -> cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "expected base type of ArrayT but got %s"
                      (type_to_s tb)))
  | IteE(l,cnd,ie,ee) -> type_expr cnd
    >>= expect_e l BoolT >>= fun cnd' ->
    type_expr ie >>= fun ie' ->
    type_expr ee >>= expect_equal l TA.extract_expr_type ie'
    >>| fun ee' -> let et = TA.extract_expr_type ie' in
    TA.IteE(et,cnd',ie',ee')
  | ArrayLE(_,bs,e) -> get
    >>= fun env -> type_loopbind bs
    >>= fun bs' -> type_expr e
    >>= fun e' -> put env (* replace the environment with the original *)
    >> let t = TA.extract_expr_type e' in
    let arrt = ArrayT(t, List.length bs' |> Int64.of_int) in
    return (TA.ArrayLE(arrt,bs',e'))
  | SumLE(l,bs,e) -> get
    >>= fun env -> type_loopbind bs
    >>= fun bs' -> type_expr e
    >>= expect_or_e l [IntT; FloatT]
    >>= fun e'-> put env
    >> let t = TA.extract_expr_type e' in
    return (TA.SumLE(t,bs',e'))
  | AppE(l,vn,ps) -> lookup_err l vn
    >>= fun arrowt -> (match arrowt with
        | ArrowT(rt,pst) -> map_m ps ~f:type_expr
          >>= fun ps' -> let pst' = List.map TA.extract_expr_type ps' in
          if not (list_equal ( = ) pst pst') then
            cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "expected parameter types of %s but got %s"
                      (type_list_to_s pst) (type_list_to_s pst'))
          else return (TA.AppE(rt, vn, ps'))
        | ot -> cerr_msg ~pos:l ~t:"type"
                  ~msg:(Printf.sprintf "expected type of ArrowT but got %s"
                          (type_to_s ot)))

let type_stmt = function
  | LetS(_,lv,e) -> type_expr e
    >>= fun e' -> unify_lvalue lv (TA.extract_expr_type e')
    >>| fun lv' -> TA.LetS(lv', e')
  | AssertS(l,e,s) -> type_expr e
    >>= expect_e l BoolT
    >>| fun e' -> TA.AssertS(e', s)
  | ReturnS(_,e) -> type_expr e
    >>| fun e' -> TA.ReturnS(TA.extract_expr_type e',e')

let rec type_binding = function
  | ArgB(_,a,te) -> unify_arg a te
    >>| fun a' -> TA.ArgB(te,a')
  | CrossbindB(_,bs) -> map_m bs ~f:type_binding
    >>| fun bs' ->
    let t = CrossT (List.map TA.extract_binding_type bs') in
    TA.CrossbindB(t, bs')

let rec type_cmd = function
  | ReadimgC (_,fn, arg) -> unify_arg arg Env.img_te
    >>= fun arg' -> return (TA.ReadimgC (fn, arg'))
  | ReadvidC (l,_, _arg) ->
    cerr_msg ~pos:l ~t:"type" ~msg:"video currently unsupported"
  | WriteimgC (l,e,fn) -> type_expr e
    >>= expect_e l Env.img_te
    >>| fun e' -> TA.WriteimgC(e', fn)
  | WritevidC (l,_,_) ->
    cerr_msg ~pos:l ~t:"type" ~msg:"video currently unsupported"
  | PrintC (_,s) -> return (TA.PrintC s)
  | ShowC (_,e) -> type_expr e
    >>| fun e' -> TA.ShowC e'
  | TimeC (_,c) -> type_cmd c
    >>| fun c' -> TA.TimeC c'
  | FnC (l,vn,bs,te,ss) ->
    get >>= fun env -> map_m bs ~f:type_binding
    >>= fun bs' ->
    let fntype = ArrowT(te, List.map TA.extract_binding_type bs') in
    modify (fun s -> Env.extend s vn fntype)
    >> map_m ss ~f:type_stmt
    >>= fun ss' -> get_retstmt_type l ss'
    >>= fun rt -> expect l ident te rt
    >> put env >> modify (fun s -> Env.extend s vn fntype)
    >> return (TA.FnC (fntype, vn, bs', te, ss'))
  | StmtC (_,s) -> type_stmt s
    >>| fun s' -> TA.StmtC s'
(* given a list of statements find the one that is a RetStmt and return its type *)
and get_retstmt_type l = fun stmts ->
  try (match
         (List.find (fun s ->
              match s with | TA.ReturnS _ -> true | _ -> false)
             stmts)
       with
       | TA.ReturnS (rt,_) ->
         return rt
       | _ -> assert false)
  with Not_found ->
    cerr_msg ~pos:l ~t:"type" ~msg:"functions must return a value"

let get_retstmt_type_c l = fun cmds ->
  try (match
         (List.find (fun s ->
              match s with | (TA.StmtC TA.ReturnS _) ->
                true | _ ->
                false) cmds) with
      | (TA.StmtC TA.ReturnS (rt,_)) ->
        return rt
      | _ -> assert false)
  with Not_found ->
    cerr_msg ~pos:l ~t:"type" ~msg:"functions must return a value"

let type_prog (p : prog) : (TA.prog, string) Result.t =
  let dp = Lexing.dummy_pos in
  (* make sure that the top-level has a return *)
  let p = match (List.rev p |> list_hd) with
    | Some (StmtC(_, ReturnS(_, _))) -> p
    | Some _ -> p @ [StmtC(dp, ReturnS(dp, IntE(dp, Int64.zero)))]
    | None -> [StmtC(dp, ReturnS(dp, IntE(dp, Int64.zero)))]
  in
  eval_state_t (map_m p ~f:type_cmd
                >>= fun program -> get_retstmt_type_c dp program
                >>= expect dp ident IntT
                >> return program) (Env.mempty ())
