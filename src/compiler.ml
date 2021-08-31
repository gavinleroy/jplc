(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Utils

(* this function is a HACK it would be better to know
 * when we bailed early or whether a read error occured.
 * maybe having some Result.t with three option
 * Ok | Error | Bail
 * or something similar *)
let maybe_exit ok_exit pp_f ast =
  if ok_exit then
    begin
      pp_f Format.std_formatter ast;
      print_newline ();
      flush stdout;
      Error ""
    end
  else Ok ast

let compile_prog
    ?(emit_parse = false)
    ?(emit_type = false)
    ?(interp_module = false)
    ?(emit_jir = false)
    ?(emit_llvm = false)
    (* the filename without the extension *)
    file_stem
    lexbuf =
  (* FIXME *)
  ignore file_stem;
  ignore interp_module;
  ignore emit_jir;
  ignore emit_llvm;
  (*********)
  try let open Functional.Or_error in
    Parsing.Lex_parse.parse_prog lexbuf
    >>= maybe_exit emit_parse Parsing.Pp.pp_prog
    >>= Typing.Typechecker.type_prog
    >>= maybe_exit emit_type Typing.Pp.pp_prog
    (* >>= Jir.Make_jir.jir_of_ty
     * >>= maybe_exit skip_codegen  Jir.Pp.pp_jir
     * >>= Codegen.gen_code_of_prog
     * >>= maybe_exit skip_assembler print_endline Codegen.emit_llvm_module *)
    |> function
    | Ok _llvm_module ->
      (* output the LLVM Module to a file <filename>.ll *)
      (* run a script that will take an LLVM Module and compile it with clang -O2 *)
      ANSITerminal.(printf [yellow; Bold] "WARNING: unimplemented codegen %b\n%!" emit_llvm)
    | Error msg -> (* print the error message to the console *)
      ANSITerminal.(eprintf [red; Bold] "%s\n%!" msg)

  with e ->
    let msg = Printexc.to_string e
    and stack = Printexc.get_backtrace () in
    ANSITerminal.(eprintf [magenta; Bold]
                    "an error occured within jplc please file a bug report\n\n%s\n%s"
                    msg stack)
