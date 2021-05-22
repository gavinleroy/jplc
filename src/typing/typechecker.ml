(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast_utils
open Utils
open Parsing.Ast
open Result

module TA = Ast

(*************************************)
(********* UTILITY FUNCITONS *********)
(*************************************)

let type_to_s t =
  Sexp.to_string (sexp_of_type t)

let type_list_to_s ts =
  Sexp.to_string (List.sexp_of_t sexp_of_type ts)

let foldMN
    (r1 : 'd Or_error.t -> 'e Or_error.t)
    (r2 : 'd Or_error.t -> 'f Or_error.t)
    (unwrap : (Env.t -> 'a -> 'd Or_error.t) -> 'a -> 'd -> 'd Or_error.t)
    (f : Env.t -> 'a -> 'd Or_error.t)
    (env : Env.t) (xs : 'a list) =
  let rec foldM' prev xs =
    match xs with
    | [] -> [r1 prev], r2 prev
    | y :: ys ->
      foldM' (prev >>= unwrap f y) ys
      |> fun (vs', env') ->
      ((r1 prev) :: vs'), env' in
  match xs with
  | [] -> return ([], env)
  | x :: xs ->
    foldM' (f env x) xs
    |> fun (vs, env') ->
    all vs >>= fun vs' ->
    env' >>| fun env'' -> vs', env''

let foldM3 f env xs =
  foldMN (fun a -> a >>| snd3) (fun a -> a >>| trd3)
    (fun g v (_,_,e) -> g e v) f env xs

let foldM2 f env xs =
  foldMN (fun a -> a >>| fst) (fun a -> a >>| snd)
    (fun g v (_,e) -> g e v) f env xs

let all_equal' x xs ~equal =
  List.fold_left xs ~init:true
    ~f:(fun acc v ->
        acc && equal x v)

(* return boolean indicating if all list elements are equal
 * according to the function `equal` *)
let all_equal xs ~equal =
  match xs with
  | [] | [_] -> true
  | x :: xs' -> all_equal' x xs' ~equal:equal

(* expect type `te` to be of type `expr` *)
let expect pos (exp : type_expr) (te : type_expr * 'a * Env.t) =
  let t = fst3 te in
  if not (exp=t) then
    Err.cerr_msg ~pos:pos ~t:"type"
      ~msg:(Printf.sprintf "expected type %s but got %s"
              (type_to_s exp) (type_to_s t))
  else return te

(* expect type `te`  to be one of the given exps *)
let expect_or pos (exps : type_expr list) (te : type_expr * 'a * Env.t) =
  let t = fst3 te in
  match List.find exps ~f:(fun t' -> t=t') with
  | Some _ -> return te
  | None -> Err.cerr_msg ~pos:pos ~t:"type"
              ~msg:(Printf.sprintf "expected one of the following types: %s but got %s"
                      (type_list_to_s exps) (type_to_s t))

(* lookup Varname.t in the environment and return an error if not bound *)
let lookup_err env p vn =
  match Env.lookup env vn with
  | None -> Err.cerr_msg ~pos:p ~t:"type"
              ~msg:(Printf.sprintf "unbounded symbol '%s'" (Varname.to_string vn))
  | Some te -> return te

let unify_arg arg t env =
  match arg with
  | VarA(_,vn) -> return (TA.VarA(t,vn), Env.extend env vn t)
  | ArraybindA (l,vn,vns) ->
    (match t with
     | ArrayT(_,r) ->
       let vnsl = Int64.of_int (List.length vns) in
       if Int64.( = ) vnsl r
       then foldM2 (fun ev v -> return (v, Env.extend ev v IntT)) env vns
         >>| fun (_, env') -> (TA.ArraybindA (t, vn, vns), Env.extend env' vn t)
       else Err.cerr_msg ~pos:l ~t:"type"
           ~msg:(Printf.sprintf
                   "binding a rank %d array with %d dimensions"
                   (Int64.to_int_exn r) (Int64.to_int_exn vnsl))
     | _ -> Err.cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "expected ArrayT but got %s"
                      (type_to_s t)))

let rec unify_lvalue lv t env =
  match lv with
  | ArgLV(_,a) -> unify_arg a t env
    >>| fun (a', env') -> TA.ArgLV(t, a'), env'
  | CrossbindLV(l,lvs) ->
    (match t with
     | CrossT tes ->
       (match List.zip lvs tes with
        | Ok lvstes ->
          foldM2 (fun env (lv, te) ->
              unify_lvalue lv te env) env lvstes
          >>| fun (lvs', env') ->
          TA.CrossbindLV(t, lvs'), env'
        | Unequal_lengths ->
          Err.cerr_msg ~pos:l ~t:"type"
            ~msg:(Printf.sprintf
                    "expected tuple of %d elements but got %d"
                    (List.length lvs) (List.length tes)))
     | _ -> Err.cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf
                      "expected tuple right-hand-side but got %s"
                      (type_to_s t)))

(*****************************************)
(********* TYPECHECKER FUNCITONS *********)
(*****************************************)

let rec type_expr env e =
  (* given an environment and a loop bind (vn * expr)
   * typecheck all exprs and expect and IntT then
   * bind all of the types to the Varnames *)
  let type_loopbind env bs =
    let exs = List.map ~f:snd bs in
    foldM3 (fun ev e ->
        type_expr ev e
        >>= expect (extract_expr_pos e) IntT) env exs
    >>= fun (es', _) ->
    let tes = List.map ~f:TA.extract_expr_type es' in
    foldM2 (fun env (lv, te) -> unify_arg lv te env) env
      (List.map2_exn bs tes ~f:(fun (a,_) t ->
           VarA(Lexing.dummy_pos, a), t))
    >>| fun (_, env') ->
    (List.map2_exn bs es' ~f:(fun (a,_) e' ->
         a,e')), env' in
  (* case statement for typechecking exprs *)
  match e with
  | IntE(_,i) -> return (IntT, TA.IntE i, env)
  | FloatE(_,f) -> return (FloatT, TA.FloatE f, env)
  | TrueE _ -> return (BoolT, TA.TrueE, env)
  | FalseE _ -> return (BoolT, TA.FalseE, env)
  | VarE(l,vn) -> lookup_err env l vn
    >>| fun t -> t, TA.VarE(t, vn), env
  | CrossE(_,es) ->
    foldM3 type_expr env es
    >>| fun (es', env') ->
    let t = CrossT (List.map es' ~f:TA.extract_expr_type) in
    t, TA.CrossE(t,es'), env'
  | ArrayCE(l,es) ->
    foldM3 type_expr env es
    >>= fun (es', env') ->
    let tes = List.map es' ~f:TA.extract_expr_type in
    if not (all_equal tes ~equal:( = )) then
      Err.cerr_msg ~pos:l ~t:"type" ~msg:"array types must all be equal"
    else let arrt = ArrayT(
        (match List.hd tes with
         | None -> Unit
         | Some y -> y), Int64.of_int 1) in
      return (arrt, TA.ArrayCE(arrt,es'), env')
  | BinopE(l,lhs,op,rhs) ->
    let type_boolop = fun ts ort -> type_expr env lhs
      >>= expect_or l ts >>= fun (tl,lhs',env') -> type_expr env' rhs
      >>= expect l tl >>| fun (tr,rhs',env'') ->
      let tt = (match ort with | Some t -> t | None -> tr) in
      tt, TA.BinopE(tt,lhs',op,rhs'), env'' in
    (match op with
     | Lt | Gt | Lte | Gte | Cmp | Neq ->
       type_boolop [IntT; FloatT] (Some BoolT)
     | Mul | Div | Mod | Plus | Minus ->
       type_boolop [IntT; FloatT] None
     | Or | And -> type_boolop [BoolT] None)
  | UnopE(l,op,e) ->
    let type_unop = fun ts -> type_expr env e
      >>= expect_or l ts >>| fun (t, e', env') ->
      t, TA.UnopE(t, op, e'), env' in
    (match op with
     | Bang -> type_unop [BoolT]
     | Neg -> type_unop [IntT; FloatT])
  | CastE(l,e,t) ->
    type_expr env e
    >>= fun (et,e',env') ->
    (match et,t with
     | IntT, FloatT
     | FloatT, IntT -> return (t, TA.CastE(et, e'), env')
     | _,_ ->
       Err.cerr_msg ~pos:l ~t:"type"
         ~msg:(Printf.sprintf
                 "cannot convert expression of type %s to type %s"
                 (type_to_s et) (type_to_s t)))
  | CrossidxE(l,e,i) ->
    type_expr env e
    >>= fun (t,e',_) ->
    (match t with
     | CrossT tes ->
       (try let iint = (Int64.to_int_exn i) in
          match List.nth tes iint with
          | Some ti ->
            return (ti, TA.CrossidxE(ti,e',iint), env)
          | None ->
            Err.cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "index %d out of tuple range" iint)
        with _ -> Err.cerr_msg ~pos:l ~t:"type"
                    ~msg:(Printf.sprintf "tuple sizes greater than %d unsupported" Int.max_value))
     | _ -> Err.cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "expected base type of CrossT but got %s"
                      (type_to_s t)))
  | ArrayidxE(l,base,idxs) ->
    (* typecheck the base and make sure it is ArrayT *)
    type_expr env base
    >>= fun (tb,base',_) ->
    (match tb with
     | ArrayT(baset,r) ->
       if Int64.( = ) r (List.length idxs |> Int64.of_int) then
         foldM3 (fun ev e -> type_expr ev e
                  >>= expect (extract_expr_pos e) IntT) env idxs
         >>= fun (idxs',_) ->
         return (baset, TA.ArrayidxE(baset, base', idxs'), env)
       else Err.cerr_msg ~pos:l ~t:"type"
           ~msg:(Printf.sprintf
                   "incorrect number of indexes provided for array of rank %d"
                   (Int64.to_int_exn r))
     | _ -> Err.cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "expected base type of ArrayT but got %s"
                      (type_to_s tb)))
  | IteE(l,cnd,ie,ee) -> type_expr env cnd
    >>= expect l BoolT >>= fun (_,cnd',env') ->
    type_expr env' ie >>= fun (it,ie',env'') ->
    type_expr env'' ee >>= expect l it
    >>| fun (et,ee',env''') ->
    et, TA.IteE(et,cnd',ie',ee'), env'''
  | ArrayLE(_,bs,e) -> type_loopbind env bs
    >>= fun (bs', env') -> type_expr env' e
    >>| fun (t,e',_) ->
    let arrt = ArrayT(t, List.length bs' |> Int64.of_int) in
    arrt, TA.ArrayLE(arrt,bs',e'), env
  | SumLE(l,bs,e) -> type_loopbind env bs
    >>= fun (bs', env') -> type_expr env' e
    >>= expect_or l [IntT; FloatT]
    >>| fun (t,e',_) -> t, TA.SumLE(t,bs',e'), env
  | AppE(l,vn,ps) ->
    lookup_err env l vn
    >>= fun arrowt ->
    (match arrowt with
     | ArrowT(rt,pst) -> foldM3 type_expr env ps
       >>= fun (ps', _) ->
       let pst' = List.map ps' ~f:TA.extract_expr_type in
       if not (List.equal ( = ) pst pst') then
         Err.cerr_msg ~pos:l ~t:"type"
           ~msg:(Printf.sprintf "expected parameter types of %s but got %s"
                   (type_list_to_s pst) (type_list_to_s pst'))
       else return (rt, TA.AppE(rt, vn, ps'), env)
     | ot -> Err.cerr_msg ~pos:l ~t:"type"
               ~msg:(Printf.sprintf "expected type of ArrowT but got %s"
                       (type_to_s ot)))

let type_stmt env = function
  | LetS(_,lv,e) -> type_expr env e
    >>= fun (t,e',env') -> unify_lvalue lv t env'
    >>| fun (lv', env'') -> Unit, TA.LetS(lv', e'), env''
  | AssertS(l,e,s) -> type_expr env e
    >>= expect l BoolT
    >>| fun (_, e', env') -> Unit, TA.AssertS(e', s), env'
  | ReturnS(_,e) -> type_expr env e
    >>| fun (t, e', env') -> t, TA.ReturnS(t,e'), env'

let rec type_binding env = function
  | ArgB(_,a,te) -> unify_arg a te env
    >>| fun (a',env') -> te,TA.ArgB(te,a'),env'
  | CrossbindB(_,bs) ->
    foldM3 type_binding env bs
    >>| fun (bs', env') ->
    let t = CrossT (List.map bs' ~f:TA.extract_binding_type) in
    t, TA.CrossbindB(t, bs'), env'

let rec type_cmd env = function
  | ReadimgC (_,fn, VarA(_,vn)) ->
    return (Unit, TA.ReadimgC (fn, TA.VarA(Env.img_te, vn)), Env.extend_img env vn)
  | ReadvidC (l,_, VarA(_,_)) ->
    Err.cerr_msg ~pos:l ~t:"type" ~msg:"video currently unsupported"
  | ReadimgC (l,_, ArraybindA _) ->
    Err.cerr_msg ~pos:l ~t:"type" ~msg:"cannot pattern match within 'read' command"
  | ReadvidC (l,_, ArraybindA _) ->
    Err.cerr_msg ~pos:l ~t:"type" ~msg:"cannot pattern match within 'read' command"
  | WriteimgC (l,e,fn) -> type_expr env e
    >>= expect l Env.img_te
    >>| fun (_, e', env') -> Unit, TA.WriteimgC(e', fn), env'
  | WritevidC (l,_,_) ->
    Err.cerr_msg ~pos:l ~t:"type" ~msg:"video currently unsupported"
  | PrintC (_,s) -> return (Unit, TA.PrintC s, env)
  | ShowC (_,e) -> type_expr env e
    >>| fun (_, e', env') -> Unit, TA.ShowC e', env'
  | TimeC (_,c) -> type_cmd env c
    >>| fun (_, c', env') -> Unit, TA.TimeC c', env'
  | FnC (l,vn,bs,te,ss) ->
    let get_retstmt_type = fun stmts ->
      match (List.find stmts ~f:(fun s ->
          match s with | TA.ReturnS _ -> true | _ -> false)) with
      | Some (TA.ReturnS (rt,_)) -> return rt
      | _ -> Err.cerr_msg ~pos:l ~t:"type" ~msg:"functions must return a value" in
    foldM3 type_binding env bs
    >>= fun (bs', env') -> foldM3 type_stmt env' ss
    >>= fun (ss', _) -> get_retstmt_type ss'
    >>= fun rt -> expect l te (rt,0,env')
    >>| fun _ ->
    let fntype = ArrowT(rt, List.map bs' ~f:TA.extract_binding_type) in
    fntype, TA.FnC (fntype, vn, bs', te, ss'), Env.extend env vn fntype
  | StmtC (_,s) -> type_stmt env s
    >>| fun (_, s', env') -> Unit, TA.StmtC s', env'

let type_prog (p : prog) =
  foldM3 type_cmd (Env.empty ()) p
  >>| fst
