(************************)
(*      Gavin Gray      *)
(*       09.2021        *)
(************************)

open Typing.Ast
open Ast_utils

(* NOTE this indicates a type checking error *)
exception Unbound_symbol

let empty_env _ = raise Unbound_symbol

let empty_f_env = empty_env

let bind env sym v =
  fun y ->
  if String.equal sym y then
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
    k env fenv (`Int ((Varname.to_string vn |> env)))
  | CrossE (_,_) ->
    assert false
  | ArrayCE (_,_) ->
    assert false
  | BinopE (_,_,_,_) ->
    assert false
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
  | IteE (_,_,_,_) ->
    assert false
  | ArrayLE (_,_,_) ->
    assert false
  | SumLE (_,_,_) ->
    assert false

  (* test appE *)
  | AppE (_t , fn, [e]) ->
    interp_expr e env fenv (fun _env fenv v ->
        let f = Varname.to_string fn
                |> fenv in
        k env fenv (f v))

  (* real appE *)
  | AppE (_,_,_) ->
    assert false


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

  (* temporary test FnC
   * FIXME this will not allow for recursive definitions *)
  | FnC (_t, vn, [ArgB(__t, VarA(IntT, vna))], _rt, ss) ->
    let name = Varname.to_string vn in
    let arg = Varname.to_string vna in
    let func a =
      interp_fn_body ss (bind env arg (exp_int a))  fenv initial_k
    in k env (bind fenv name func) (`Int 0 (* dummy value *))

  | FnC (_t, _vn, _bs, _rt, _ss) ->
    assert false
  | StmtC s ->
    interp_stmt s env fenv k

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
