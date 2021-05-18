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

let unwrap3 f v t =
  let _, _, e = t in f e v

let ret2 a =
  a >>| snd3

let ret3 a =
  a >>| trd3

let foldM f env xs =
  let rec foldM' prev xs =
    match xs with
    | [] -> [ret2 prev], ret3 prev
    | y :: ys ->
      foldM' (prev >>= unwrap3 f y) ys
      |> fun (vs', env') ->
      ((ret2 prev) :: vs'), env' in
  match xs with
  | [] -> return ([], env)
  | x :: xs ->
    foldM' (f env x) xs |> fun (vs, env') ->
    all vs >>= fun vs' ->
    env' >>| fun env'' -> vs', env''

let extract_binding_type = function
  | TA.ArgB(t,_) -> t
  | TA.CrossbindB(t,_) -> t

let expect pos (exp : type_expr) (te : type_expr * 'a * Env.t) =
  let t = fst3 te in
  if not (exp=t) then
    Err.cerr_msg ~pos:pos ~t:"type"
      ~msg:(Printf.sprintf "expected type %s but got %s"
              (Sexp.to_string (sexp_of_type exp))
              (Sexp.to_string (sexp_of_type t)))
  else return te

let expect_or pos (exps : type_expr list) (te : type_expr * 'a * Env.t) =
  let t = fst3 te in
  match List.find exps ~f:(fun t' -> t=t') with
  | Some _ -> return te
  | None -> Err.cerr_msg ~pos:pos ~t:"type"
              ~msg:(Printf.sprintf "expected one of the following types: %s but got %s"
                      (Sexp.to_string (List.sexp_of_t sexp_of_type exps))
                      (Sexp.to_string (sexp_of_type t)))

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
       then foldM (fun ev v -> return (0,v, Env.extend ev v IntT)) env vns
         >>| fun (_, env') -> (TA.ArraybindA (t, vn, vns), Env.extend env' vn t)
       else Err.cerr_msg ~pos:l ~t:"type"
           ~msg:(Printf.sprintf
                   "binding a rank %d array with %d dimensions"
                   (Int64.to_int_exn r) (Int64.to_int_exn vnsl))
     | _ -> Err.cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf "expected ArrayT but got %s"
                      (Sexp.to_string (sexp_of_type t))))

let unify_lvalue lv t env =
  match lv with
  | ArgLV(_,a) -> unify_arg a t env
    >>| fun (a', env') -> TA.ArgLV(t, a'), env'
  | CrossbindLV(l,lvs) ->
    (match t with
     | CrossT tes ->
       let tl, lvl = (List.length tes), (List.length lvs) in
       if Int.( = ) tl lvl then
         Error (Error.of_string "TODO")
       else Err.cerr_msg ~pos:l ~t:"type"
           ~msg:(Printf.sprintf
                   "expected tuple of %d elements but got %d" lvl tl)
     | _ -> Err.cerr_msg ~pos:l ~t:"type"
              ~msg:(Printf.sprintf
                      "expected tuple right-hand-side but got %s"
                      (Sexp.to_string (sexp_of_type t))))

(*****************************************)
(********* TYPECHECKER FUNCITONS *********)
(*****************************************)

let rec type_expr env = function
  | IntE(_,i) -> return (IntT, TA.IntE i, env)
  | FloatE(_,f) -> return (FloatT, TA.FloatE f, env)
  | TrueE _ -> return (BoolT, TA.TrueE, env)
  | FalseE _ -> return (BoolT, TA.FalseE, env)
  | VarE(l,vn) -> lookup_err env l vn
    >>| fun t -> t, TA.VarE(t, vn), env
  | CrossE(_,_) -> Error (Error.of_string "TODO")
  | ArrayCE(_,_) -> Error (Error.of_string "TODO")

  | BinopE(l,lhs,op,rhs) ->
    let type_boolop = fun ts ort -> type_expr env lhs
      >>= expect_or l ts >>= fun (tl,lhs',env') -> type_expr env' rhs
      >>= expect l tl >>| fun (tr,rhs',env'') ->
      (match ort with | Some t -> t | None -> tr),
      TA.BinopE(tr,lhs',op,rhs'), env'' in
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
                 (Sexp.to_string (sexp_of_type et))
                 (Sexp.to_string (sexp_of_type t))))
  | CrossidxE(_,_,_) -> Error (Error.of_string "TODO")
  | ArrayidxE(_,_,_) -> Error (Error.of_string "TODO")
  | IteE(l,cnd,ie,ee) -> type_expr env cnd
    >>= expect l BoolT >>= fun (_,cnd',env') ->
    type_expr env' ie >>= fun (it,ie',env'') ->
    type_expr env'' ee >>= expect l it
    >>| fun (et,ee',env''') ->
    et, TA.IteE(et,cnd',ie',ee'), env'''
  | ArrayLE(_,_,_) -> Error (Error.of_string "TODO")
  | SumLE(_,_,_) -> Error (Error.of_string "TODO")
  | AppE(_,_,_) -> Error (Error.of_string "TODO")


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
    foldM type_binding env bs
    >>| fun (bs', env') ->
    let t = CrossT (List.map bs' ~f:extract_binding_type) in
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
    foldM type_binding env bs
    >>= fun (bs', env') -> foldM type_stmt env' ss
    >>= fun (ss', _) -> get_retstmt_type ss'
    >>= fun rt -> expect l te (rt,0,env')
    >>| fun _ ->
    let fntype = ArrowT(rt, List.map bs' ~f:extract_binding_type) in
    fntype, TA.FnC (fntype, vn, bs', te, ss'), Env.extend env vn fntype
  | StmtC (_,s) -> type_stmt env s
    >>| fun (_, s', env') -> Unit, TA.StmtC s', env'

let type_prog (p : prog) =
  foldM type_cmd (Env.empty ()) p
  >>| fst
