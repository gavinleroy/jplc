(************************)
(*      Gavin Gray      *)
(*       09.2021        *)
(************************)

open Typing.Ast
(* open Ast_utils *)

(* continuation
 *   val K: value -> interpable -> env -> fenv -> value *)

exception Unbound_symbol

let empty_env _ = raise Unbound_symbol

let empty_f_env = empty_env

(* let bind env sym v =
 *   fun y -> if
 *     String.(sym = y) then
 *       v
 *     else env y *)

(* unwrapping *)

(* let expect_int = function
 *   | `IntV i -> Ok i
 *   | _ -> Error "Expected int" *)

(* interp_loopbind (_, _) =
   assert false *)

(* and interp_arg = function
 *   | VarA (_,_) ->
 *     assert false
 *   | ArraybindA (_,_,_) ->
 *     assert false *)

(* and interp_binding = function
 *   | ArgB (_,_) ->
 *     assert false
 *   | CrossbindB (_,_) ->
 *     assert false *)

(* expect type functions
 * ~ the program should already be type safe *)

let exp_int = function
  | `Int i -> i
  | _ -> assert false

let exp_float = function
  | `Float f -> f
  | _ -> assert false

let rec interp_expr e env fenv k =
  ignore env; ignore fenv;
  match e with
  | IntE i ->
    (* FIXME integers need to stay 64 bits *)
    let i = Int64.to_int i in
    k (`Int .<i>.)
  | FloatE f ->
    k (`Float .<f>.)
  | TrueE ->
    k (`Bool .<true>.)
  | FalseE ->
    k (`Bool .<false>.)
  | VarE (_,_) ->
    assert false
  | CrossE (_,_) ->
    assert false
  | ArrayCE (_,_) ->
    assert false
  | BinopE (_,_,_,_) ->
    assert false
  | UnopE (_,_,_) ->
    assert false

  | CastE (t, e, t') ->
    interp_expr e env fenv (fun v ->
        k (match t, t' with
            | IntT, FloatT ->
              `Float .<(Int.to_float .~(exp_int v))>.
            | FloatT, IntT ->
              `Int .<(Float.to_int .~(exp_float v))>.
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
  | AppE (_,_,_) ->
    assert false

(* and interp_lvalue = function
 *   | ArgLV (_, _) ->
 *     assert false
 *   | CrossbindLV (_, _) ->
 *     assert false *)

and interp_stmt s env fenv k =
  match s with
  | LetS (_, _) ->
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
  | ShowC _ ->
    assert false
  | TimeC _ ->
    assert false
  | FnC (_,_,_,_,_) ->
    assert false
  | StmtC s ->
    interp_stmt s env fenv k

and interp_cmd_list cs env fenv k =
  match cs with
  (* this shouldn't happen *)
  | [] -> assert false
  (* last command /should be a return stmt/ *)
  | [ x ] ->
    interp_cmd x env fenv k
  | c :: cs' ->
    interp_cmd c env fenv (fun v ->
        ignore v; (* we ignore the return value of all commands except:
                   * ~ the last return StmtCommand *)
        interp_cmd_list cs' env fenv k)

and initial_k = (fun x -> x)

and interp_prog (p : prog) =
  Ok (interp_cmd_list p empty_env empty_f_env initial_k
      |> exp_int |> Runnative.run)
(* let string_of_code c =
 *   let () = Codelib.print_code Format.str_formatter c in
 *   Format.flush_str_formatter ()
 * in
 * interp_cmd_list p empty_env empty_f_env initial_k
 * |> (fun c ->
 *     Printf.printf "~~~ running code ~~~ \n%s\n" (string_of_code c);
 *     Ok (Runnative.run c)) *)
