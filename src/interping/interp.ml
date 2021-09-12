(************************)
(*      Gavin Gray      *)
(*       09.2021        *)
(************************)

open Typing.Ast
open Ast_utils
open Ty

(* let string_of_code c = (\* TODO remove me *\)
 *   let () = Codelib.print_code Format.str_formatter c in
 *   Format.flush_str_formatter () *)

let dummy_value = UnitIT

exception Typechecking_error

(* simple environment *)

let empty_env s =
  Printf.printf "Couldn't find symbol '%s'!\n" (Varname.to_string s);
  raise Typechecking_error

let empty_f_env = empty_env

let bind env sym v =
  fun y ->
  if Varname.(=) sym y then
    v
  else env y

let exp_int = function
  | IntIT i -> i
  | _ -> raise Typechecking_error

let exp_float = function
  | FloatIT f -> f
  | _ -> raise Typechecking_error

let exp_bool = function
  | BoolIT b -> b
  | _ -> raise Typechecking_error

let exp_list = function
  | ListIT l -> l
  | _ -> raise Typechecking_error

let exp_array = function
  | ArrayIT a -> a
  | _ -> raise Typechecking_error

let exp_tuple = function
  | TupleIT t -> t
  | _ -> raise Typechecking_error

let tuple_idx tup i =
  tup.(i)

let rec interp_list f ls env fenv k =
  match ls with
  (* this shouldn't happen *)
  | [] -> raise Typechecking_error
  (* last command /should be a return stmt/ *)
  | [ x ] ->
    f x env fenv k
  | s :: ss' ->
    f s env fenv (fun env fenv _v ->
        interp_list f ss' env fenv k)

let initial_k =
  fun _ _ x -> x

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
  | CrossE (_t, es) ->
    interp_expr_list es env fenv (fun env fenv vs ->
        k env fenv (TupleIT (exp_list vs
                             |> Array.of_list)))
  | ArrayCE (_t, es) ->
    interp_expr_list es env fenv (fun env fenv v ->
        k env fenv (ArrayIT (exp_list v
                             |> Array.of_list)))
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
                | _, _, _ -> raise Typechecking_error)))
  | UnopE (_t, o, e) ->
    interp_expr e env fenv (fun env fenv v ->
        k env fenv (match o, v with
            | `Bang, BoolIT b -> BoolIT (not b)
            | `Neg, IntIT i -> IntIT (- i)
            | `Neg, FloatIT f -> FloatIT (-. f)
            | _, _ -> raise Typechecking_error))
  | CastE (t, e, t') ->
    interp_expr e env fenv (fun _ _ v ->
        k env fenv (match t, t' with
            | IntT, FloatT ->
              FloatIT (Int.to_float (exp_int v))
            | FloatT, IntT ->
              IntIT (Float.to_int (exp_float v))
            | _, _ -> raise Typechecking_error))
  | CrossidxE (_t, e, idx) ->
    interp_expr e env fenv (fun env fenv v ->
        k env fenv (exp_tuple v
                    |> fun t -> tuple_idx t idx))
  | ArrayidxE (_t, e, es) ->
    interp_expr e env fenv (fun env fenv v ->
        interp_expr_list es env fenv (fun env fenv vs ->
            let rec make_loop acc = function
              | [] -> acc
              | c :: cs ->
                make_loop (exp_array acc
                           |> fun a -> a.(exp_int c)) cs
            in k env fenv (make_loop v (exp_list vs))))
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

and bind_list env bs vs =
  List.fold_left2 unify_binding env vs bs

and unify_binding env v = function
  | ArgB (_, a) ->
    unify_arg env v a
  | CrossbindB (_t, bs) ->
    let cross_vs = exp_tuple v
                   |> Array.to_list in
    bind_list env bs cross_vs

and unify_arg env v = function
  | VarA (_, vn) ->
    bind env vn v
  | ArraybindA (_t, _vn, _vns) ->
    assert false

and unify_lvalue env v = function
  | ArgLV (_t, a) ->
    unify_arg env v a
  | CrossbindLV (_t, lvs) ->
    List.fold_left2 unify_lvalue env (exp_tuple v
                                      |> Array.to_list) lvs

and interp_stmts ss env fenv k =
  interp_list interp_stmt ss env fenv k

and interp_cmds  cs env fenv k =
  interp_list interp_cmd cs env fenv k

and interp_stmt s env fenv k =
  match s with
  | LetS (lv, e) ->
    interp_expr e env fenv (fun env fenv v ->
        k (unify_lvalue env v lv) fenv dummy_value)
  | AssertS (e, s) ->
    interp_expr e env fenv (fun env fenv v ->
        if (exp_bool v) then
          k env fenv dummy_value
        else
          begin
            Printf.printf "~~~ Assertion Failure ~~~\n%s\n\n" s;
            (* FIXME this and other false assertions should be changed into proper errors *)
            assert false
          end)
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
  | ShowC e ->
    interp_expr e env fenv (fun _ _ v ->
        begin
          (* NOTE using the runtime 'show' method would be more
           * work than writing an OCaml module *)
          Show.show v;
          k env fenv dummy_value
        end)
  | StmtC s ->
    interp_stmt s env fenv k
  | FnC (_t, name, bs, _rt, ss) ->
    let rec repeat n f =
      if Int.equal n 0 then
        f
      else fun x -> f (repeat (n - 1) f x)
    in
    let rec func x =
      let body cf x
      (* NOTE x is a list of values*) =
        interp_stmts ss
          (* bind all arguments to the environment *)
          (bind_list env bs (exp_list x))
          (bind fenv name cf)
          initial_k
      in repeat 1 body (fun y -> func y) x
    in k env (bind fenv name func) dummy_value

and interp_prog (p : prog) =
  Ok (interp_cmds p empty_env empty_f_env initial_k
      |> exp_int)
