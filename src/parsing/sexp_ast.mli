(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast

val sexp_of_prog: prog -> Sexp.t

val sexp_of_cmd: cmd -> Sexp.t

val sexp_of_stmt: stmt -> Sexp.t

val sexp_of_arg: arg -> Sexp.t

val sexp_of_expr: expr -> Sexp.t

val sexp_of_binding: binding -> Sexp.t

val sexp_of_lvalue: lvalue -> Sexp.t
