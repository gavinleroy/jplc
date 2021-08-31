(************************)
(*      Gavin Gray      *)
(*       09.2021        *)
(************************)

let interp_prog (p : Typing.Ast.prog) : unit =
  ignore p;
  let c = .<1 + 2>. in
  Printf.printf "value : %d\n" (Runnative.run c)
