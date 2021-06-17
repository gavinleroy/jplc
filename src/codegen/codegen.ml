(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

open Core
open Flattening.Ast

(* NOTE a few assumptions that are being made in the code generator:
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * + the variables are already in SSA format and there are no duplicate names
 *
 * + no functions have the same name. these should also have been desugared in the
 *   flattening phase.
 *
 * + the program is safely typed
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*)

(* GLOBAL VARIABLES THAT NEED TO GO AWAY FIXME *)
let ctx =
  Llvm.create_context ()

module Env = struct
  type t =
    { ll_b   : Llvm.llbuilder
    (* currently there is no support for modules/imports so all
     * code must be contained to one module *)
    ; ll_m   : Llvm.llmodule
    ; tbl    : (string, Llvm.llvalue) Hashtbl.t }
  let mempty () =
    { ll_b = Llvm.builder ctx
    ; ll_m = Llvm.create_module ctx "jplm"
    ; tbl = Hashtbl.create(module String) }
  let mappend _m1 _m2 =
    assert false
  let set_bldr_pos bb t =
    Llvm.position_at_end bb t.ll_b
  (* let add_llv f t =
   *   f t.ll_b *)
  let add_llv_ f t : unit =
    let (_ : 'a) = f t.ll_b in ()
  let store_llv vn llv t =
    Hashtbl.add_exn ~key:vn ~data:llv t.tbl
end

module State = Utils.Functional.State(Env)
module Monadic = Utils.Functional.Utils(State)
open State
open Monadic
open Env

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(* DEFINITIONS OF LLVM IR TYPES IN JPL *)
(* the following is from RUNTIME.ml
 *~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *  type runtime_type =
 *   | UnitRT
 *   | StringRT
 *   | ArrayRT of runtime_type * int
 *   | CrossRT of runtime_type list
 *   | ArrowRT of runtime_type * runtime_type list *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let i64_t =
  Llvm.i64_type ctx

let f64_t =
  Llvm.double_type ctx

(* NOTE booleans are represented as 1 bit integers *)
let bool_t =
  Llvm.i1_type ctx

(* NOTE strings are represented as character pointers
 * in LLVM IR *)
let str_t =
  Llvm.i8_type ctx
  |> Llvm.pointer_type

let make_cross_t ts =
  Array.of_list ts
  |> Llvm.struct_type ctx

(* a static array is one defined in JPL such as
 * let a = [1, 2, 3]
 * ! This is where we KNOW the size at runtime. *)
(* let make_static_array_t base_t len =
 *   Llvm.array_type base_t len *)

let make_arrow_t rett ps =
  Array.of_list ps
  |> Llvm.function_type rett

let rec llvm_t_of_runtime = function
  | Runtime.UnitRT -> assert false
  | Runtime.BoolRT -> bool_t
  | Runtime.IntRT -> i64_t
  | Runtime.FloatRT -> f64_t
  | Runtime.StringRT -> str_t
  | Runtime.ArrayRT (_base_t, _rank) -> assert false
  | Runtime.CrossRT (rts) ->
    List.map rts ~f:llvm_t_of_runtime
    |> make_cross_t
  | Runtime.ArrowRT (rt, ps) ->
    let rt = llvm_t_of_runtime rt in
    let ps = List.map ~f:llvm_t_of_runtime ps in
    make_arrow_t rt ps

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*            UTILITY FUNCTIONS         *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let get_llv vn = get >>= fun env ->
  return (Hashtbl.find_exn env.tbl vn)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*            CODE GENERATORS          *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let rec gen_code_of_expr = function
  | TrueE -> assert false
  | FalseE -> assert false

  (* true means that the integer is signed *)
  | IntE i -> return (Llvm.const_of_int64 i64_t i true)
  | FloatE f -> return (Llvm.const_float f64_t f)
  | StringE _str -> assert false
  | CrossE (_t, _vns) -> assert false
  | ArrayCE (_t, _vns) -> assert false
  | BinopE (_t, Varname (_t', _l), o, Varname (_t'', _r)) ->
    (* get_llv l >>= fun llvl -> get_llv r >>= fun llvr -> *)
    (match o with
     | Ast_utils.Lt -> assert false
     | Ast_utils.Gt -> assert false
     | Ast_utils.Cmp -> assert false
     | Ast_utils.Lte -> assert false
     | Ast_utils.Gte -> assert false
     | Ast_utils.Neq -> assert false
     | Ast_utils.Mul -> assert false
     | Ast_utils.Div -> assert false
     | Ast_utils.Mod -> assert false
     | Ast_utils.Plus -> assert false
     | Ast_utils.Minus -> assert false

     (* maybe we should get rid of these?? *)
     | Ast_utils.And -> assert false
     | Ast_utils.Or -> assert false)

  | UnopE (_t, _o, _vn) -> assert false
  (* type un_op =
   *   | Bang | Neg *)
  | CastE (_t, _vn) -> assert false
  | CrossidxE (_t, _vn, _idx) -> assert false
  | ArrayidxE (_t, _vn, _vns) -> assert false
  | IteE _ -> assert false
  | ArrayLE (_t, _lbs, _es) -> assert false
  | SumLE (_t, _vn, _vns) -> assert false
  | AppE (_t, _vn, _vns) -> assert false





  (* previously seen as Stmts *)
  | LetE (Varname (_t, vn), e) -> gen_code_of_expr e
    >>= fun ll_ev -> modify_ (Env.store_llv vn ll_ev)
    >> return ll_ev






  | AssertE (_vn, _str) -> assert false





  | ReturnE (Varname (_t, vn)) -> get_llv vn
    >>= fun ll_v -> modify_ (Env.add_llv_ (Llvm.build_ret ll_v))
    >> return ll_v






  (* previously seen as Cmds *)
  | ReadimgE (_str, _vn) -> assert false
  | ReadvidE (_str, _vn) -> assert false
  | WriteimgE (_vn, _str) -> assert false
  | WritevidE (_vn, _str) -> assert false
  | PrintE _str -> assert false
  | ShowE _vn -> assert false

let gen_code_of_fn (fn : Fn.t) = get >>= fun env ->
  let Varname (fn_type, fn_name) = fn.name in
  let fn_type = llvm_t_of_runtime fn_type in
  let ll_f = Llvm.define_function fn_name fn_type env.ll_m in
  modify_ (Env.set_bldr_pos (Llvm.entry_block ll_f))
  >>
  (* let (_ : unit) = List.iter ~f:(fun s -> print_endline (Sexp.to_string (sexp_of_expr s))) fn.body in *)
  map_m_ ~f:gen_code_of_expr fn.body

let gen_code_of_prog p =
  exec_state (map_m p ~f:gen_code_of_fn) (Env.mempty ())
  |> fun env ->
  (* Llvm_analysis.assert_valid_module jpl_module; (\* TODO remove later *\) *)
  Ok env.ll_m

(* function for generating the string dump of a module *)
let emit_llvm_module =
  Llvm.string_of_llmodule
