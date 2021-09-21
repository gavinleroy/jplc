(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Utils

let print_error_msg m =
  (* HACK fixme later *)
  ANSITerminal.(eprintf [red; Bold]
                  "~~~~~~ ERROR ~~~~~~
> %s
~~~~~~~~~~~~~~~~~~~\n%!" m)

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

(* fork and either continue compilation or
 * continue with interpreting the program and
 * end there *)
let interp_or_compile
    (do_interp : bool)
    (compile_k :  Typing.Ast.prog -> unit)
    (interp_k : Typing.Ast.prog -> unit)
  = function
    | Ok p ->
      if do_interp then
        interp_k p
      else compile_k p
    | Error m -> print_error_msg m

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
  ignore emit_jir;
  ignore emit_llvm;
  (*********)

  let compilation_cont (_ : Typing.Ast.prog) =
    ANSITerminal.(printf [yellow; Bold]
                    "WARNING: unimplemented codegen %b\n%!" emit_llvm)
    (* >>= Jir.Make_jir.jir_of_ty
     * >>= maybe_exit skip_codegen  Jir.Pp.pp_jir
     * >>= Codegen.gen_code_of_prog
     * >>= maybe_exit skip_assembler print_endline Codegen.emit_llvm_module *)
    (* |> *)
    (* function
     * | Ok _llvm_module -> *)
    (* output the LLVM Module to a file <filename>.ll *)
    (* run a script that will take an LLVM Module and compile it with clang -O2 *)
    (*   ANSITerminal.(printf [yellow; Bold]
     *                   "WARNING: unimplemented codegen %b\n%!" emit_llvm)
     * | Error msg -> print_error_msg msg *)
  in

  let interp_cont (prog : Typing.Ast.prog) =
    match Interping.Interp.interp_prog prog with
    | Ok i ->
      exit i
    | Error m ->
      print_error_msg m
  in

  try let open Functional.Or_error in
    Parsing.Lex_parse.parse_prog lexbuf
    >>= maybe_exit emit_parse Parsing.Pp.pp_prog
    >>= Typing.Typechecker.type_prog
    >>= maybe_exit emit_type Typing.Pp.pp_prog
    |> interp_or_compile
      interp_module
      compilation_cont
      interp_cont


  with e ->
    let msg = Printexc.to_string e
    and stack = Printexc.get_backtrace () in
    ANSITerminal.(eprintf [magenta; Bold]
                    "an error occured within jplc please file a bug report\n\n%s\n%s"
                    msg stack)
