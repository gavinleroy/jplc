(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

open Core

let emit_llvm_module =
  Llvm.string_of_llmodule


let gen_code_of_prog _p =
  let ll_ctx = Llvm.global_context () in
  let ll_m = Llvm.create_module ll_ctx "jplm" in
  let i64_t = Llvm.i64_type ll_ctx in
  let fty = Llvm.function_type i64_t [| |] in
  let f = Llvm.define_function "main" fty ll_m in
  let llbuilder = Llvm.builder_at_end ll_ctx (Llvm.entry_block f) in
  let (_ : Llvm.llvalue) = Llvm.build_ret (Llvm.const_int i64_t 0) llbuilder in
  Ok ll_m
