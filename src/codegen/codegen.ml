(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

open Core

(* NOTE a few assumptions that are being made in the code generator:
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * + the variables are already in SSA format and there are no duplicate names
 *
 * + no functions have the same name. these should also have been desugared in the
 *   flattening phase.
 *
 * + the program is safely typed *)

let ctx =
  Llvm.global_context ()

(* currently there is no support for modules/imports so all
 * code must be contained to one module *)
let jpl_module =
  Llvm.create_module ctx "jpl-module"

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
  | BoolRT -> bool_t
  | IntRT -> i64_t
  | FloatRT -> f64_t
  | Runtime.StringRT -> str_t
  | Runtime.ArrayRT (_base_t, _rank) -> assert false
  | Runtime.CrossRT (rts) -> List.map rts ~f:llvm_t_of_runtime
                             |> make_cross_t
  | ArrowRT (_rt, _ps) -> assert false

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(*         END TYPE SECTION             *)
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

let gen_code_of_fn _fn =
  assert false

let gen_code_of_prog _p =
  let ll_ctx = Llvm.global_context () in
  let ll_m = Llvm.create_module ll_ctx "jplm" in
  let fty = make_arrow_t i64_t [] in
  let f = Llvm.define_function "main" fty ll_m in
  let llbuilder = Llvm.builder_at_end ll_ctx (Llvm.entry_block f) in
  let (_ : Llvm.llvalue) = Llvm.build_ret (Llvm.const_int i64_t 0) llbuilder in
  Ok ll_m

(* function for generating the string dump of a module *)
let emit_llvm_module =
  Llvm.string_of_llmodule
