(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let get_file_extension filename =
  String.split_on_chars filename ~on:['.']
  |> List.last |> Option.value ~default:""

let rec remove_last_elem_list = function
  | []      -> []
  | [_]     -> []
  | x :: xs -> x :: remove_last_elem_list xs

let get_output_file filename =
  String.split_on_chars filename ~on:['.']
  |> fun split_filename ->
  remove_last_elem_list split_filename
  (* remove file ending *)
  |> fun filename_without_ending ->
  String.concat ~sep:"." (filename_without_ending @ ["ir"])

let jpl_file =
  let error_not_file filename =
    Printf.printf "'%s' is not a jpl file" filename;
    exit 1 in
  Command.Spec.Arg_type.create (fun filename ->
      match Sys.is_file filename with
      | `Yes ->
        if String.equal (get_file_extension filename) "jpl"
        then filename
        else error_not_file filename
      | `No | `Unknown -> error_not_file filename)

let command =
  Command.basic ~summary:"compile jpl programs"
    ~readme:(fun () -> "~~ TODO ~~")
    Command.Let_syntax.(
      let%map_open
        filename = anon (maybe_with_default "-" ("filename" %: jpl_file))
      and skip_typecheck = flag "-p" no_arg ~doc:" dump parsed AST and skip typechecking"
      and skip_flatten = flag "-t" no_arg ~doc:" dump typed AST and skip flattening"
      and skip_codegen = flag "-f" no_arg ~doc:" dump flattened AST and skip codegen"
      (* and skip_assembler = flag "-s" no_arg ~doc:" dump assembly code" *)
      in
      fun () ->
        In_channel.with_file filename ~f:(fun file_ic ->
            let lexbuf = Lexing.from_channel file_ic in
            Compiler.compile_prog
              ~skip_typecheck
              ~skip_flatten
              ~skip_codegen
              lexbuf))

let () =
  Command.run ~version:"1.0" ~build_info:"JPLC" command
