(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast_utils

type t = (Varname.t * type_expr)  list

let img_te =
  ArrayT ((CrossT [FloatT; FloatT; FloatT; FloatT;]) (* base type *)
         , (Int64.of_int 2)) (* rank 2 *)

let empty () =
  (* TODO put the library and default env *)
  []

let extend e vn te = (vn, te) :: e

let extend_img e vn =
  (vn, img_te) :: e

let rec lookup (e : t) vn =
  match e with
  | [] -> None
  | (vn', te) :: e' ->
    if Varname.(=) vn' vn
    then Some te
    else lookup e' vn
