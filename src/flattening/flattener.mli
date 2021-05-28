(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

(* NOTE flatten_prog returns an AST Or_error.t
 * However, there should *never* be a flattening error
 * because all invalid programs should have been stopped by now. *)
val flatten_prog: Typing.Ast.prog -> Ast.prog Or_error.t
