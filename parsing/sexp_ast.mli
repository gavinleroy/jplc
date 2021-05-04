(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

val sexp_of_prog: Ast.prog -> Sexp.t

val sexp_of_cmd: Ast.cmd -> Sexp.t

val sexp_of_stmt: Ast.stmt -> Sexp.t

val sexp_of_arg: Ast.arg -> Sexp.t

val sexp_of_expr: Ast.expr -> Sexp.t

val sexp_of_binding: Ast.binding -> Sexp.t

val sexp_of_lvalue: Ast.lvalue -> Sexp.t

val sexp_of_type: Ast_utils.type_expr -> Sexp.t

