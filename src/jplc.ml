(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Cmdliner

let list_last ls ~default =
  try List.rev ls |> List.hd
  with Failure _ -> default

let get_file_extension filename =
  String.split_on_char '.' filename
  |> list_last ~default:""

let rec remove_last_elem_list = function
  | [] | [_] -> []
  | x :: xs -> x :: remove_last_elem_list xs

let get_output_file filename =
  String.split_on_char '.' filename
  |> fun split_filename ->
  remove_last_elem_list split_filename
  (* remove file ending *)
  |> fun filename_without_ending ->
  String.concat "." (filename_without_ending @ ["ir"])

(* check whether or not a given 'filename' is a real file and
 * defined with a JPL extension. *)
let is_jpl_file filename =
  let error_not_file fn =
    Printf.printf "'%s' is not a jpl file" fn;
    exit 1 in
  if Sys.file_exists filename then
    if String.equal (get_file_extension filename) "jpl" then
      ()
    else error_not_file filename
  else error_not_file filename

(* command line optional arguments *)

let emit_parsed =
  let doc = "Emit the parsed AST and skip typechecking." in
  Arg.(value & flag & info ["p"; "emit-parsed"] ~doc)

let emit_typed =
  let doc = "Emit the typed AST and skip JIR conversion." in
  Arg.(value & flag & info ["t"; "emit-typed"] ~doc)

let emit_jir =
  let doc = "Emit the JIR and skip code generation." in
  Arg.(value & flag & info ["j"; "emit-jir"] ~doc)

let emit_llvm =
  let doc = "Emit the LLVM IR and assembly." in
  Arg.(value & flag & info ["l"; "emit-llvm"] ~doc)

let interp_prog =
  let doc = "Interpret the JPL program skipping code generation." in
  Arg.(value & flag & info ["i"; "interp"] ~doc)

(* command line required arguments *)

let filename =
  let doc = "$(docv) containing the JPL source to be compiled." in
  Arg.(value & pos 0 string "" & info [] ~docv:"FILENAME" ~doc)

(* program info *)

let info =
  let doc = "Compile JPL programs!" in
  let man =  [ `S Manpage.s_bugs;
               `P "Report bugs at github.com/gavinleroy/jplc." ]
  in
  Term.info "jplc" ~version:"0.0.1" ~doc ~exits:Term.default_exits ~man

let run_jplc' filename emit_parse emit_type interp_prog emit_jir emit_llvm =
  is_jpl_file filename;
  open_in filename
  |> (fun file_ic ->
      let lexbuf = Lexing.from_channel file_ic in
      Compiler.compile_prog
        ~emit_parse
        ~emit_type
        (* NOTE we can only interpet one module at a time
         * but we will keep the naming convention
         * 'interp_prog' in case support for cross-module
         * evaluation is supported later. *)
        ~interp_module:interp_prog
        ~emit_jir
        ~emit_llvm
        (* the file_stem turns into the module name *)
        "temp-jpl-module"
        lexbuf)

let run_jplc = Term.( const run_jplc'
                      $ filename
                      $ emit_parsed
                      $ emit_typed
                      $ interp_prog
                      $ emit_jir
                      $ emit_llvm )

let () =
  Term.exit @@ Term.eval (run_jplc, info)
