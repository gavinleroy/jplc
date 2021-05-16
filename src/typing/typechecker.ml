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

let unwrap
    (f : 'b -> 'a -> ('d * 'c * 'b) Or_error.t)
    (v : 'a)
    (t : 'd * 'c * 'b) =
  let _, _, e = t in
  f e v

let foldM (f : 'b -> 'a -> ('d * 'c * 'b) Or_error.t) env xs =
  let ret = fun a -> a >>= fun (_,v,_) -> Ok v in
  let rec foldM' prev xs =
    match xs with
    | [] -> [ret prev]
    | y :: ys ->
      (ret prev) :: (foldM' (prev >>= unwrap f y) ys) in
  match xs with
  | [] -> Ok []
  | x :: xs ->
    all (foldM' (f env x) xs)

let expect pos (exp : type_expr) (te : type_expr * 'a * Env.t) =
  let t = fst3 te in
  if not (exp=t) then
    Err.cerr_msg ~pos:pos ~t:"type"
      ~msg:(Printf.sprintf "expected type %s but got %s"
              (Sexp.to_string (sexp_of_type exp))
              (Sexp.to_string (sexp_of_type t)))
  else Ok te
let type_expr _env _e =
  Error (Error.of_string "unimplemented type expr")

let type_stmt _env _s =
  Error (Error.of_string "unimplemented type stmt")

let rec type_cmd env = function
  | ReadimgC (_,fn, VarA(_,vn)) ->
    Ok (Unit, TA.ReadimgC (fn, TA.VarA(Env.img_te, vn)), Env.extend_img env vn)
  | ReadvidC (_,_, VarA(_,_)) ->
    Error (Error.of_string "video unsupported")
  | ReadimgC (_,_, ArraybindA _)
  | ReadvidC (_,_, ArraybindA _) ->
    Error (Error.of_string "cannot pattern match in read command")
  | WriteimgC (l,e,fn) ->
    type_expr env e
    >>= expect l Env.img_te
    >>= fun (_, e', env') -> Ok (Unit, TA.WriteimgC(e', fn), env')
  | WritevidC (_,_,_) ->
    Error (Error.of_string "video unsupported")
  | PrintC (_,s) -> Ok (Unit, PrintC s, env)
  | ShowC (_,e) ->
    type_expr env e
    >>= fun (_, e', env') -> Ok (Unit, TA.ShowC e', env')
  | TimeC (_,c) ->
    type_cmd env c
    >>= fun (_, c', env') -> Ok (Unit, TA.TimeC c', env')
  (* | FnC (l,vn,bs,te,ss) -> *)
  | FnC (_,_,_,_,_) ->
    (* 1. extend env with the args *)
    (* 2. typecheck all of the statements (foldM over list)  *)
    (* 3. make sure return type is present and matches expected *)
    (* 4. extend env with arrowT. *)
    Error (Error.of_string "TODO")
  | StmtC (_,s) ->
    type_stmt env s
    >>= fun (_, s', env') -> Ok (Unit, TA.StmtC s', env')

let type_prog (p : prog) =
  foldM type_cmd (Env.empty ()) p
