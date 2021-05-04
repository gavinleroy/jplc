(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast
open Ast_utils

let rec sexp_of_type t =
  match t with
  | IntT -> Sexp.Atom "IntType"
  | BoolT -> Sexp.Atom "BoolType"
  | FloatT -> Sexp.Atom "FloatType"
  | Float3T -> Sexp.Atom "Float3Type"
  | Float4T -> Sexp.Atom "Float4Type"
  | ArrayT (te, r) -> Sexp.List [Sexp.Atom "ArrayType"; sexp_of_type te; Int.sexp_of_t r]
  | CrossT tes -> Sexp.List[Sexp.Atom "CrossType"; List.sexp_of_t sexp_of_type tes]

let sexp_of_expr _e =
  Sexp.Atom "unimplemented"

let sexp_of_lvalue _lv = 
  Sexp.Atom "unimplemented"

let sexp_of_binding _b =
  Sexp.Atom "unimplemented"

let sexp_of_stmt _s = 
  Sexp.Atom "unimplemented"

let sexp_of_arg _a =
  Sexp.Atom "unimplemented"

let rec sexp_of_cmd c =
  match c with
  | ReadimgC (_,fn,a) ->
    Sexp.List [Sexp.Atom "ReadImageCmd" 
              ; Sexp.Atom (Filename.to_string fn); sexp_of_arg a]
  | ReadvidC (_,fn,a) ->
    Sexp.List [Sexp.Atom "ReadVideoCmd"
              ; Sexp.Atom (Filename.to_string fn); sexp_of_arg a]
  | WriteimgC (_,e,fn) -> 
    Sexp.List [ Sexp.Atom "WriteImageCmd"
              ; sexp_of_expr e; Sexp.Atom (Filename.to_string fn)]
  | WritevidC (_,e,fn) -> 
    Sexp.List [ Sexp.Atom "WriteVideoCmd"
              ; sexp_of_expr e; Sexp.Atom (Filename.to_string fn)]
  | PrintC (_,s) -> Sexp.List[Sexp.Atom "PrintCmd"; Sexp.Atom s ]
  | ShowC (_,e) -> Sexp.List[Sexp.Atom "ShowCmd"; sexp_of_expr e]
  | TimeC (_,c) -> Sexp.List[Sexp.Atom "TimeCmd"; sexp_of_cmd c]
  | FnC (_,vn,bs,te,ss) ->
    Sexp.List[Sexp.Atom "Func"
             ; Sexp.Atom (Varname.to_string vn)
             ; List.sexp_of_t sexp_of_binding bs
             ; sexp_of_type te
             ; List.sexp_of_t sexp_of_stmt ss]
  | StmtC (_,s) -> 
    Sexp.List[Sexp.Atom "StmtCmd"; sexp_of_stmt s]

let sexp_of_prog p = 
  List.sexp_of_t sexp_of_cmd p

(* type expr = *)
(*    | IntE of loc * int *)
(*    | FloatE of loc * float *)
(*    | TrueE of loc *)
(*    | FalseE of loc *)
(*    | VarE of loc * Varname.t *)
(*    | CrossE of loc * expr list *)
(*    | ArrayCE of loc * expr list *)
(*    | BinopE of loc * expr * bin_op * expr *)
(*    | UnopE of loc * un_op * expr *)
(*    (1* NOTE cross index must be int to support static typing *1) *)
(*    | CrossIdxE of loc * expr * int *)
(*    | ArrayIdxE of loc * expr * expr list *)
(*    | IteE of loc * expr * expr * expr *)
(*    | ArrayLE of loc * (Varname.t * expr) list * expr *)
(*    | SumLE of loc * (Varname.t * expr) list * expr *)
(*    | AppE of loc * Varname.t * expr list *)

(* type arg = *)
(*   | VarA of loc * Varname.t *)
(*   | ArraybindA of loc * Varname.t * Varname.t list *)

(* type lvalue = *)
(*   | ArgLV of loc * arg *)
(*   | CrossbindLV of loc * lvalue list *)

(* type binding = *)
(*   | ArgB of loc * arg * type_expr *)
(*   | CrossbindB of loc * binding list *)

(* type stmt = *)
(*   | LetS of loc * lvalue * expr *)
(*   | AssertS of loc * expr * string *)
(*   | ReturnS of loc * expr *)

(* type cmd = *)
(*   | ReadimgC of loc * filename * arg *)
(*   | ReadvidC of loc * filename * arg *)
(*   | WriteimgC of loc * expr * filename *)
(*   | WritevidC of loc * expr * filename *)
(*   | PrintC of loc * string *)
(*   | ShowC of loc * expr *)
(*   | TimeC of loc * cmd *)
(*   | FnC of loc * Varname.t * binding list * type_expr * stmt list *)
(*   | StmtC of loc * stmt *)

(* type prog = cmd list *)

