(************************)
(*      Gavin Gray      *)
(*       09.2021        *)
(************************)

open Typing.Ast
open Ast_utils

type 'a ret_cps_t =
  | IntIT of int
  | FloatIT of float
  | BoolIT of bool
  | ListIT of 'a ret_cps_t list
  | ArrayIT of 'a Array.t

let dummy_value =
  IntIT 0

(* NOTE this indicates a type checking error *)
exception Unbound_symbol

(* type rets =
 *   [> ListIT of rets list
 *   | IntIT of int
 *   | FloatIT of float
 *   | BoolIT of bool ] *)

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
  | IntIT i -> (i : int)
  | _ -> assert false

let exp_float = function
  | FloatIT f -> (f : float)
  | _ -> assert false

let exp_bool = function
  | BoolIT b -> (b : bool)
  | _ -> assert false

let exp_list = function
  | ListIT l -> l
  | _ -> assert false

(* let bind_with_type (env : Varname.t -> 'a) ty sym v : (Varname.t -> 'a) =
 *   match ty, v with
 *   | Unit, _ -> assert false
 *   | IntT, IntIT i -> bind env sym i
 *   | FloatT, FloatIT f -> bind env sym f
 *   | BoolT, BoolIT b -> bind env sym b
 *   | ArrayT (_base_t, _r), _ -> assert false
 *   | CrossT (_ts), _ -> assert false
 *   | ArrowT (_rt, _ts), _ -> assert false
 *   | _ -> assert false *)

let rec interp_expr e env fenv k =
  match e with
  | IntE i ->
    (* FIXME HACK : integers need to stay 64 bits *)
    let i = Int64.to_int i in
    k env fenv (IntIT i)
  | FloatE f ->
    k env fenv (FloatIT f)
  | TrueE ->
    k env fenv (BoolIT true)
  | FalseE ->
    k env fenv (BoolIT false)
  | VarE (_,vn) ->
    k env fenv (env vn)
  | CrossE (_,_) ->
    assert false
  | ArrayCE (_,_) ->
    assert false
  | BinopE (_te, lhs, o, rhs) ->
    interp_expr lhs env fenv (fun env fenv lv ->
        interp_expr rhs env fenv (fun env fenv rv ->
            k env fenv (match lv, rv, o with
                | IntIT il, IntIT ir, `Lt -> BoolIT (il < ir)
                | IntIT il, IntIT ir, `Gt -> BoolIT (il > ir)
                | IntIT il, IntIT ir, `Cmp -> BoolIT (Int.equal il ir)
                | IntIT il, IntIT ir, `Lte -> BoolIT (il <= ir)
                | IntIT il, IntIT ir, `Gte -> BoolIT (il >= ir)
                | IntIT il, IntIT ir, `Neq -> BoolIT (il <> ir)
                | IntIT il, IntIT ir, `Mul -> IntIT (il * ir)
                | IntIT il, IntIT ir, `Div -> IntIT (il / ir)
                | IntIT il, IntIT ir, `Mod -> IntIT (Int.rem il ir)
                | IntIT il, IntIT ir, `Plus -> IntIT (il + ir)
                | IntIT il, IntIT ir, `Minus -> IntIT (il - ir)

                | FloatIT fl, FloatIT fr, `Lt -> BoolIT (fl < fr)
                | FloatIT fl, FloatIT fr, `Gt -> BoolIT (fl > fr)
                | FloatIT fl, FloatIT fr, `Cmp -> BoolIT (Float.equal fl fr)
                | FloatIT fl, FloatIT fr, `Lte -> BoolIT (fl <= fr)
                | FloatIT fl, FloatIT fr, `Gte -> BoolIT (fl >= fr)
                | FloatIT fl, FloatIT fr, `Neq -> BoolIT (fl <> fr)
                | FloatIT fl, FloatIT fr, `Mul -> FloatIT (fl *. fr)
                | FloatIT fl, FloatIT fr, `Div -> FloatIT (fl /. fr)
                | FloatIT fl, FloatIT fr, `Mod -> FloatIT (Float.rem fl fr)
                | FloatIT fl, FloatIT fr, `Plus -> FloatIT (fl +. fr)
                | FloatIT fl, FloatIT fr, `Minus -> FloatIT (fl -. fr)
                (* NOTE indicates a bad typechecker *)
                | _, _, _ -> assert false)))
  | UnopE (_t, o, e) ->
    interp_expr e env fenv (fun env fenv v ->
        k env fenv (match o, v with
            | `Bang, BoolIT b -> BoolIT (not b)
            | `Neg, IntIT i -> IntIT (- i)
            | `Neg, FloatIT f -> FloatIT (-. f)
            | _, _ -> assert false))
  | CastE (t, e, t') ->
    interp_expr e env fenv (fun _ _ v ->
        k env fenv (match t, t' with
            | IntT, FloatT ->
              FloatIT (Int.to_float (exp_int v))
            | FloatT, IntT ->
              IntIT (Float.to_int (exp_float v))
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
    interp_expr_list es env fenv
      (fun _env _fenv v ->
         k env fenv (fenv fn v))

and interp_expr_list es env fenv k =
  (* NOTE we don't pass the new environments into
   * the loop because they should not change when
   * 'interping' expressions. *)
  let rec loop vs es =
    match es with
    | [] -> k env fenv (ListIT (List.rev vs))
    | e :: es' ->
      interp_expr e env fenv (fun _env _fenv v ->
          loop (v :: vs) es')
  in loop [] es

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
  | LetS (ArgLV(_t, VarA(__t, vn)), e) ->
    interp_expr e env fenv (fun env fenv v ->
        k (bind env vn v) fenv dummy_value)
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
  | TimeC _ ->
    assert false
  (* FIXME use the runtime library for show *)
  | ShowC e ->
    interp_expr e env fenv (fun _ _ v ->
        Printf.printf "SHOW : %d\n" (exp_int v);
        k env fenv dummy_value)
  | StmtC s ->
    interp_stmt s env fenv k
  | FnC (_t, name, bs, _rt, ss) ->
    let rec repeat n f =
      if Int.equal n 0 then
        f
      else fun x -> f (repeat (n - 1) f x)
    in
    let bind_list env bs v =
      let rec loop env bs vs =
        match bs, vs with
        | [], [] -> env
        | b :: bs', v :: vs' ->
          (match b with
           | ArgB (_, a) ->
             (match a with
              | VarA (_, vn) ->
                loop (bind env vn v) bs' vs'
              | ArraybindA (_t, _vn, _vns) ->
                assert false)
           | CrossbindB (_t, _bs) ->
             assert false)
        (* NOTE this indicates a typechecker error *)
        | _, _ -> assert false
      in loop env bs (exp_list v)
    in
    let rec func x =
      let body cf x
      (* NOTE x is a list of values*) =
        interp_fn_body ss
          (* bind all arguments to the environment *)
          (bind_list env bs x)
          (bind fenv name cf)
          initial_k
      in repeat 1 body (fun y -> func y) x
    in k env (bind fenv name func) dummy_value

and interp_cmd_list cs env fenv k =
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
      |> exp_int)

(* let string_of_code c =
 *   let () = Codelib.print_code Format.str_formatter c in
 *   Format.flush_str_formatter ()
 * in
 * interp_cmd_list p empty_env empty_f_env initial_k
 * |> (fun c ->
 *     Printf.printf "~~~ running code ~~~ \n%s\n" (string_of_code c);
 *     Ok (Runnative.run c)) *)
