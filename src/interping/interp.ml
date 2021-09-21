(************************)
(*      Gavin Gray      *)
(*       09.2021        *)
(************************)

open Typing.Ast
open Ast_utils
open Ty

(* NOTE after typechecking, we don't need all of the exhaustive
 * patter matching. On Match_failure, we print out that there is
 * an error in the typechecker *)
[@@@ warning "-8"] (* HACK this seems excessive *)

let string_of_code c = (* TODO remove me *)
  let () = Codelib.print_code Format.str_formatter c in
  Format.flush_str_formatter ()

let dummy_value = UnitIT

exception Typechecking_error

exception Jpl_assert_fail of string

(* simple environment *)

let empty_env s =
  Printf.printf "Couldn't find symbol '%s'!\n" (Varname.to_string s);
  raise Typechecking_error

let empty_f_env = empty_env

let bind env sym v = fun y ->
  if Varname.(=) sym y then
    v
  else env y

let exp_int (IntIT i) = i

let exp_float (FloatIT f) = f

let exp_bool (BoolIT b) = b

let exp_list (ListIT l) = l

let exp_array (ArrayIT a) = a

let exp_tuple (TupleIT t) = t

let tuple_idx tup i =
  tup.(i)

(* utility functions *)

let list_drop c ls =
  let rec loop c = function
    | [] -> []
    | v :: ls ->
      if c > 0 then
        (loop (c - 1) ls)
      else v :: ls
  in loop c ls

let list_take c ls =
  let rec loop c = function
    | [] -> []
    | v :: ls ->
      if c > 0 then
        v :: (loop (c - 1) ls)
      else []
  in loop c ls

let list_chunk chunk ls =
  let rec loop = function
    | [] -> []
    | ls ->
      list_take chunk ls :: (loop (list_drop chunk ls))
  in loop ls

let add_intit (IntIT a) (IntIT b) =
  IntIT (a + b)

let ident_k = fun _ _ v -> v

(* conversion functions *)

(* TODO FIXME HACK
 * These conversions to and from the CArray will make image
 * reading/writing much slower than it needs to be *)

let cstruct_from_array = function
  | ArrayIT arr ->
    let rows = Array.length arr in
    let l = List.concat_map (function
        | ArrayIT arr ->
          let ll = List.concat_map (function
              | TupleIT [| FloatIT f0; FloatIT f1; FloatIT f2; FloatIT f3|] ->
                [f0; f1; f2; f3]
              | _ -> assert false) (Array.to_list arr) in
          ll
        | _ -> assert false) (Array.to_list arr) in
    let data = Ctypes.CArray.of_list Ctypes.double l in
    let cols = (Ctypes.CArray.length data) / (4 *  (* because float4 *) rows) in
    let p = Ctypes.make Runtime.Lib.pict in
    begin
      Ctypes.setf p Runtime.Lib.rows (Int64.of_int rows);
      Ctypes.setf p Runtime.Lib.cols (Int64.of_int cols);
      Ctypes.setf p Runtime.Lib.data (Ctypes.CArray.start data);
      p
    end
  | _ -> assert false

let array_from_cstruct cs =
  let open Runtime.Lib in
  let open Ctypes in
  let wrap v = ArrayIT (Array.of_list v) in
  let (r, c) = getf cs rows |> Signed.Int64.to_int
             , getf cs cols |> Signed.Int64.to_int in
  let d = getf cs data |> (fun p -> CArray.from_ptr p (r * c * 4)) in
  CArray.to_list d
  |> list_chunk (c * 4)
  |> List.map (fun l ->
      list_chunk 4 l
      |> List.map (function
          | [f0; f1; f2; f3] ->
            TupleIT [| FloatIT f0; FloatIT f1; FloatIT f2; FloatIT f3|]
          | _ -> assert false)
      |> wrap)
  |> wrap

(* interp functions *)

let rec interp_list f ls env fenv k =
  match ls with
  (* last command /should be a return stmt/ *)
  | [ x ] ->
    f x env fenv k
  | s :: ss' ->
    f s env fenv (fun env fenv _v ->
        interp_list f ss' env fenv k)

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
    interp_expr_list es env fenv (fun env fenv (ListIT vs) ->
        k env fenv (TupleIT (vs |> Array.of_list)))
  | ArrayCE (ArrayT(base_t, _), es) ->
    interp_expr_list es env fenv (fun env fenv (ListIT vs) ->
        k env fenv (ArrayIT (vs |> Array.of_list)))
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
                | FloatIT fl, FloatIT fr, `Minus -> FloatIT (fl -. fr))))
  | UnopE (_t, o, e) ->
    interp_expr e env fenv (fun env fenv v ->
        k env fenv (match o, v with
            | `Bang, BoolIT b -> BoolIT (not b)
            | `Neg, IntIT i -> IntIT (- i)
            | `Neg, FloatIT f -> FloatIT (-. f)))
  | CastE (t, e, t') ->
    interp_expr e env fenv (fun _ _ v ->
        k env fenv (match t, t', v with
            | IntT, FloatT, IntIT ic ->
              FloatIT (Int.to_float ic)
            | FloatT, IntT, FloatIT fc ->
              IntIT (Float.to_int fc)))
  | CrossidxE (t, e, idx) ->
    interp_expr e env fenv (fun env fenv (TupleIT v) ->
        k env fenv (tuple_idx v idx))
  | ArrayidxE (_t, e, es) ->
    interp_expr e env fenv (fun env fenv v ->
        interp_expr_list es env fenv (fun env fenv (ListIT vs) ->
            let rec make_loop acc = function
              | [] -> acc
              | (IntIT c) :: cs ->
                let (ArrayIT acc) = acc in
                make_loop acc.(c) cs
            in k env fenv (make_loop v vs)))
  | IteE (_t, cnd, ie, ee) ->
    interp_expr cnd env fenv (fun env fenv (BoolIT cnd) ->
        interp_expr (if cnd then
                       ie
                     else ee) env fenv k)
  | ArrayLE (_t, lbs, e) ->
    let es = List.map snd lbs in
    interp_expr_list es env fenv (fun env_o fenv (ListIT vs) ->
        let bs = List.map2 (fun (a, _) c -> (a, c))  lbs vs in
        let rec loop env = function
          | [] ->
            interp_expr e env fenv ident_k
          | ( vn, (IntIT maxn) ) :: vs' ->
            ArrayIT (Array.init maxn (fun i ->
                loop (bind env vn (IntIT i)) vs'))
        in
        k env fenv (loop env_o bs))
  (* could definitely be optimized
   * or written in cps *)
  | SumLE (_t, lbs, e) ->
    let es = List.map snd lbs in
    interp_expr_list es env fenv (fun env_o fenv (ListIT vs) ->
        let bs = List.map2 (fun (a, _) c -> (a, c))  lbs vs in
        let rec loop env = function
          | [] ->
            interp_expr e env fenv ident_k
          | ( vn, (IntIT maxn) ) :: vs' ->
            Array.init maxn (fun i ->
                loop (bind env vn (IntIT i)) vs')
            |> Array.fold_left add_intit (IntIT 0)
        in
        k env fenv (loop env_o bs))
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
    | [] ->
      let vs = List.rev vs in
      k env fenv (ListIT vs)
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
    interp_expr e env fenv (fun env fenv (BoolIT cnd) ->
        if cnd then
          k env fenv dummy_value
        else
          raise (Jpl_assert_fail (Printf.sprintf "Failed Assertion> %s" s)))
  | ReturnS(_, e) ->
    interp_expr e env fenv k

(* NOTE all commands have type UNIT except the function command *)
and interp_cmd c env fenv k =
  match c with
  | ReadimgC (fn, arg) ->
    Runtime.Lib.read_image fn
    |> array_from_cstruct
    |> (fun p -> unify_arg env p arg)
    |> (fun env -> k env fenv dummy_value)
  | WriteimgC (e, fn) ->
    interp_expr e env fenv (fun env fenv v ->
        cstruct_from_array v
        |> (fun cs ->
            begin
              Runtime.Lib.write_image cs fn;
              k env fenv dummy_value
            end))
  (* currently unsupported *)
  | ReadvidC (_, _) ->
    assert false
  | WritevidC (_, _) ->
    assert false
  (*************************)
  | PrintC str ->
    begin
      (* NOTE print_endline is much faster than
       * running a FFI *)
      print_endline str;
      k env fenv dummy_value
    end
  | TimeC c ->
    let t0 = Runtime.Lib.get_time () in
    interp_cmd c env fenv (fun env fenv v ->
        begin
          ignore v; (* NOTE ignore the return value of commands *)
          let t1 = Runtime.Lib.get_time () in
          Printf.printf "time: %f\n" (t1 -. t0);
          k env fenv dummy_value
        end)
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
      let body cf (ListIT x) =
        interp_stmts ss
          (* bind all arguments to the environment *)
          (bind_list env bs x)
          (bind fenv name cf)
          (fun _ _ x -> x)
      in repeat 1 body (fun y -> func y) x
    in k env (bind fenv name func) dummy_value

and interp_prog (p : prog) =
  try
    Ok (interp_cmds p empty_env empty_f_env ident_k
        |> exp_int)
  with
  | Jpl_assert_fail msg ->
    Error msg
  | Division_by_zero ->
    Error "division by zero"
  | Invalid_argument m ->
    Error m
  | Match_failure (s, i, j) ->
    begin
      Printf.printf "MATCH ERROR => \"%s\" [ %d : %d ]\n" s i j;
      raise Typechecking_error
    end

(* go back to exhaustive pattern matching checks *)
[@@@ warning "+8"] (* HACK this seems excessive *)
