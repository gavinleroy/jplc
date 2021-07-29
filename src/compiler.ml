(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Utils

(* this function is a HACK it would be better to know
 * when we bailed early or whether a read error occured.
 * maybe having some Result.t with three option
 * Ok | Error | Bail
 * or something similar *)
let maybe_exit ok_exit pp cnv ast =
  if ok_exit then
    (cnv ast |> pp; Error (Error.of_string ""))
  else Ok ast

let compile_prog
    ?(skip_typecheck = false)
    ?(skip_flatten = false)
    ?(skip_codegen = false)
    ?(skip_assembler = false)
    lexbuf =
  try let open Result in
    Parsing.Lex_parse.parse_prog lexbuf
    >>= maybe_exit skip_typecheck Pp.print_sexp Parsing.Sexp_ast.sexp_of_prog
    >>= Typing.Typechecker.type_prog
    >>= maybe_exit skip_flatten Pp.print_sexp Typing.Sexp_ast.sexp_of_prog
    >>= Jir.Make_jir.jir_of_ty
    >>= maybe_exit skip_codegen (Printf.printf "%s%!") Jir.Pp_jir.stdout_of_jir
    >>= Codegen.gen_code_of_prog
    >>= maybe_exit skip_assembler print_endline Codegen.emit_llvm_module
    |> function
    | Ok _llvm_module ->
      (* output the LLVM Module to a file <filename>.ll *)
      (* run a script that will take an LLVM Module and compile it with clang -O2 *)
      ANSITerminal.(printf [yellow; Bold] "WARNING: unimplemented codegen %b\n%!" skip_assembler)
    | Error msg -> (* print the error message to the console *)
      Error.to_string_hum msg
      |> ANSITerminal.(eprintf [red; Bold] "%s\n%!")

  with e -> let msg = Exn.to_string e
    and stack = Printexc.get_backtrace () in
    ANSITerminal.(eprintf [magenta; Bold]
                    "an error occured within jplc please file a bug report\n\n%s\n%s"
                    msg stack)
