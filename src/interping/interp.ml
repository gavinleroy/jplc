(************************)
(*      Gavin Gray      *)
(*       09.2021        *)
(************************)

open Typing.Ast
open Ast_utils

(* NOTE this indicates a type checking error *)
exception Unbound_symbol

(* type rets =
 *   [> `List of rets list
 *   | `Int of int
 *   | `Float of float
 *   | `Bool of bool ] *)

let empty_env _ = raise Unbound_symbol

let empty_f_env = empty_env

let bind env sym v =
  fun y ->
  if Varname.(=) sym y then
    v
  else env y

(* expect type functions
 * ~ the program should already be type safe *)

let exp_int = function
  | `Int i -> i
  | _ -> assert false

let exp_float = function
  | `Float f -> f
  | _ -> assert false

let exp_bool = function
  | `Bool b -> b
  | _ -> assert false

let exp_list = function
  | `List l -> l
  | _ -> assert false

let bind_with_type env ty sym v =
  bind env sym (match ty with
      | Unit -> assert false
      | IntT -> exp_int v
      | BoolT -> exp_bool v
      | FloatT -> exp_float v
      | ArrayT (_base_t, _r) -> assert false
      | CrossT (_ts) -> assert false
      | ArrowT (_rt, _ts) -> assert false)

let rec interp_expr e env fenv k =
  match e with
  | IntE i ->
    (* FIXME integers need to stay 64 bits *)
    let i = Int64.to_int i in
    k env fenv (`Int i)
  | FloatE f ->
    k env fenv (`Float f)
  | TrueE ->
    k env fenv (`Bool true)
  | FalseE ->
    k env fenv (`Bool false)
  | VarE (_,vn) ->
    k env fenv (`Int (env vn))
  | CrossE (_,_) ->
    assert false
  | ArrayCE (_,_) ->
    assert false
  | BinopE (_te, lhs, o, rhs) ->
    interp_expr lhs env fenv (fun env fenv lv ->
        interp_expr rhs env fenv (fun env fenv rv ->
            k env fenv (match lv, rv, o with
                | `Int il, `Int ir, `Lt -> `Bool (il < ir)
                | `Int il, `Int ir, `Gt -> `Bool (il > ir)
                | `Int il, `Int ir, `Cmp -> `Bool (Int.equal il ir)
                | `Int il, `Int ir, `Lte -> `Bool (il <= ir)
                | `Int il, `Int ir, `Gte -> `Bool (il >= ir)
                | `Int il, `Int ir, `Neq -> `Bool (il <> ir)
                | `Int il, `Int ir, `Mul -> `Int (il * ir)
                | `Int il, `Int ir, `Div -> `Int (il / ir)
                | `Int il, `Int ir, `Mod -> `Int (Int.rem il ir)
                | `Int il, `Int ir, `Plus -> `Int (il + ir)
                | `Int il, `Int ir, `Minus -> `Int (il - ir)

                | `Float fl, `Float fr, `Lt -> `Bool (fl < fr)
                | `Float fl, `Float fr, `Gt -> `Bool (fl > fr)
                | `Float fl, `Float fr, `Cmp -> `Bool (Float.equal fl fr)
                | `Float fl, `Float fr, `Lte -> `Bool (fl <= fr)
                | `Float fl, `Float fr, `Gte -> `Bool (fl >= fr)
                | `Float fl, `Float fr, `Neq -> `Bool (fl <> fr)
                | `Float fl, `Float fr, `Mul -> `Float (fl *. fr)
                | `Float fl, `Float fr, `Div -> `Float (fl /. fr)
                | `Float fl, `Float fr, `Mod -> `Float (Float.rem fl fr)
                | `Float fl, `Float fr, `Plus -> `Float (fl +. fr)
                | `Float fl, `Float fr, `Minus -> `Float (fl -. fr)
                (* NOTE indicates a bad typechecker *)
                | _, _, _ -> assert false)))
  | UnopE (_,_,_) ->
    assert false
  | CastE (t, e, t') ->
    interp_expr e env fenv (fun _ _ v ->
        k env fenv (match t, t' with
            | IntT, FloatT ->
              `Float (Int.to_float (exp_int v))
            | FloatT, IntT ->
              `Int (Float.to_int (exp_float v))
            | _, _ -> assert false))

  | CrossidxE (_,_,_) ->
    assert false
  | ArrayidxE (_,_,_) ->
    assert false
  | IteE (_t, cnd, ie, ee) ->
    interp_expr cnd env fenv (fun env fenv v ->
        interp_expr (if (exp_bool v) then
                       ie
                     else ee) env fenv k)
  | ArrayLE (_,_,_) ->
    assert false
  | SumLE (_,_,_) ->
    assert false

  | AppE (_t , fn, es) ->
    interp_expr_list [] es env fenv
      (fun _env _fenv v ->
         k env fenv (fenv fn v))

and interp_expr_list vs es env fenv k =
  match es with
  | [] -> k env fenv (`List (List.rev vs))
  | e :: es' ->
    interp_expr e env fenv (fun env fenv v ->
        interp_expr_list (v :: vs) es' env fenv k)

(* and interp_binding = function
 *   | ArgB (_,_) ->
 *     assert false
 *   | CrossbindB (_,_) ->
 *     assert false *)

(* and interp_arg = function
 *   | VarA (_,_) ->
 *     assert false
 *   | ArraybindA (_,_,_) ->
 *     assert false *)

(* and interp_lvalue = function
 *   | ArgLV (_, _) ->
 *     assert false
 *   | CrossbindLV (_, _) ->
 *     assert false *)

and interp_fn_body ss env fenv k =
  match ss with
  (* this shouldn't happen *)
  | [] -> assert false
  (* last command /should be a return stmt/ *)
  | [ x ] ->
    interp_stmt x env fenv k
  | s :: ss' ->
    interp_stmt s env fenv (fun env _fenv _v ->
        interp_fn_body ss' env fenv k)

and interp_stmt s env fenv k =
  match s with
  | LetS (ArgLV(_t, VarA(__t, _vn)), _e) ->
    assert false
  | LetS (ArgLV(_t, ArraybindA(__t, _vn, _vns)), _e) ->
    assert false
  (* | LetS (CrossbindLV(_t, _lvs), _e) ->
   *   assert false *)
  | LetS (_lv, _e) ->
    assert false
  | AssertS (_, _) ->
    assert false
  | ReturnS(_, e) ->
    interp_expr e env fenv k

(* NOTE all commands have type UNIT except the function command *)
and interp_cmd c env fenv k =
  match c with
  | ReadimgC (_, _) ->
    assert false
  | ReadvidC (_, _) ->
    assert false
  | WriteimgC (_,_) ->
    assert false
  | WritevidC (_, _) ->
    assert false
  | PrintC _ ->
    assert false

  (* FIXME *)
  | ShowC e ->
    interp_expr e env fenv (fun _ _ v ->
        Printf.printf "SHOW : %d\n" (exp_int v);
        k env fenv (`Int 0))

  | TimeC _ ->
    assert false
  | StmtC s ->
    interp_stmt s env fenv k

  (* temporary test FnC
   * FIXME this will not allow for recursive definitions *)
  | FnC (_t, name, bs, _rt, ss) ->
    let rec repeat n f =
      if Int.equal n 0 then
        f
      else fun x -> f (repeat (n - 1) f x)
    in

    let rec bind_list env bs vs =
      match bs, vs with
      | [], [] -> env
      | b :: bs', v :: vs' ->
        (match b with
         | ArgB (_, a) ->
           (match a with
            | VarA (t, vn) ->
              bind_list
                (bind_with_type env t vn v)
                bs' vs'
            | ArraybindA (_t, _vn, _vns) ->
              assert false)
         | CrossbindB (_t, _bs) ->
           assert false)
      (* NOTE this indicates a typechecker error *)
      | _, _ -> assert false
    in

    let rec func x =
      (let body cf x (* NOTE x is a list of values*) =
         interp_fn_body ss
           (* bind all arguments to the environment *)
           (bind_list env bs x)
           (bind fenv name cf)
           initial_k
       in repeat 1 body (fun y -> func y) x)
    in k env (bind fenv name func) (`Int 0 (* dummy value *))

and interp_cmd_list (cs : cmd list) env fenv k =
  match cs with
  (* this shouldn't happen *)
  | [] -> assert false
  (* last command /should be a return stmt/ *)
  | [ x ] ->
    interp_cmd x env fenv k
  | c :: cs' ->
    interp_cmd c env fenv (fun env fenv v ->
        ignore v; (* we ignore the return value of all commands except:
                   * ~ the last return StmtCommand *)
        interp_cmd_list cs' env fenv k)

and initial_k = (fun _ _ x -> x)

and interp_prog (p : prog) =
  Ok (interp_cmd_list p empty_env empty_f_env initial_k
      |> exp_int (* |> Runnative.run *))
(* let string_of_code c =
 *   let () = Codelib.print_code Format.str_formatter c in
 *   Format.flush_str_formatter ()
 * in
 * interp_cmd_list p empty_env empty_f_env initial_k
 * |> (fun c ->
 *     Printf.printf "~~~ running code ~~~ \n%s\n" (string_of_code c);
 *     Ok (Runnative.run c)) *)
