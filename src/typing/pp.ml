(************************)
(*      Gavin Gray      *)
(*       08.2021        *)
(************************)

open Ast
open Ast_utils
open Format

let rec pp_loopbinds fmt lbs =
  pp_print_list
    ~pp_sep:pp_print_space
    pp_loopbind fmt lbs

and pp_loopbind fmt (a, b) =
  fprintf fmt "(%s@ %a)"
    (Varname.to_string a)
    pp_expr b

and pp_vns fmt ls =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      (fun fmt s ->
         fprintf fmt "%s"
           (Varname.to_string s))
      fmt v
  in fprintf fmt "(%a)"
    f ls

and pp_arg fmt = function
  | VarA (t,vn) ->
    fprintf fmt "@[<2>(var-arg@ %a@ \"%s\")@]"
      pp_type t
      (Varname.to_string vn)
  | ArraybindA (t,vn,vns) ->
    fprintf fmt "@[<2>(array-bind-arg@ %a@ \"%s\"@ %a)@]"
      pp_type t
      (Varname.to_string vn)
      pp_vns vns

and pp_bindings fmt bs =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_binding fmt v
  in fprintf fmt "(%a)"
    f bs

and pp_binding fmt = function
  | ArgB (t,a) ->
    fprintf fmt "(arg-binding@ %a@ %a)"
      pp_type t
      pp_arg a
  | CrossbindB (t,bs) ->
    fprintf fmt "(cross-binding@ %a@ %a)"
      pp_type t
      pp_bindings bs

and pp_exprs fmt es =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_expr fmt v
  in fprintf fmt "(%a)"
    f es

and pp_expr fmt = function
  | IntE i ->
    fprintf fmt "@[<hov 2>(int-expr@ %a@ %Ld)@]"
      pp_type IntT i
  | FloatE f ->
    fprintf fmt "@[<hov 2>(float-expr@ %a@ %f)@]"
      pp_type FloatT f
  | TrueE ->
    fprintf fmt "@[<hov 2>(true-expr@ %a@ #t)@]"
      pp_type BoolT
  | FalseE ->
    fprintf fmt "@[<hov 2>(true-expr@ %a@ #f)@]"
      pp_type BoolT
  | VarE (t,vn) ->
    fprintf fmt "@[<hov 2>(var-expr@ %a@ %s)@]"
      pp_type t
      (Varname.to_string vn)
  | CrossE (t,es) ->
    fprintf fmt "@[<hov 2>(cross-expr@ %a@ %a)@]"
      pp_type t
      pp_exprs es
  | ArrayCE (t,es) ->
    fprintf fmt "@[<hov 2>(array-expr@ %a@ %a)@]"
      pp_type t
      pp_exprs es
  | BinopE (t,lhs,op,rhs) ->
    fprintf fmt "@[<hov 2>(binop-expr@ %a@ %a@ %a@ %a)@]"
      pp_type t
      pp_binop op
      pp_expr lhs
      pp_expr rhs
  | UnopE (t,op,e') ->
    fprintf fmt "@[<hov 2>(unop-expr@ %a@ %a@ %a)@]"
      pp_type t
      pp_unop op
      pp_expr e'
  | CastE(t,e',t') ->
    fprintf fmt "@[<hov 2>(cast-expr@ %a@ %a@ %a)@]"
      pp_type t
      pp_type t'
      pp_expr e'
  | CrossidxE (t,e',i) ->
    fprintf fmt "@[<hov 2>(cross-idx-expr@ %a@ %a@ %d)@]"
      pp_type t
      pp_expr e' i
  | ArrayidxE (t,base,idxs) ->
    fprintf fmt "@[<hov 2>(array-idx-expr@ %a@ %a@ %a)@]"
      pp_type t
      pp_expr base
      pp_exprs idxs
  | IteE (t,cnd,ie,ee) ->
    fprintf fmt "@[<hov 2>(ite-expr@ %a@ %a@ %a@ %a)@]"
      pp_type t
      pp_expr cnd
      pp_expr ie
      pp_expr ee
  | ArrayLE (t,vnes,bdy) ->
    fprintf fmt "@[<hov 2>(array-loop-expr@ %a@ %a@ %a)@]"
      pp_type t
      pp_loopbinds vnes
      pp_expr bdy
  | SumLE (t,vnes,bdy) ->
    fprintf fmt "@[<hov 2>(sum-expr@ %a@ %a@ %a)@]"
      pp_type t
      pp_loopbinds vnes
      pp_expr bdy
  | AppE (t,vn,es) ->
    fprintf fmt "@[<hov 2>(app-expr@ %a@ %s@ %a)@]"
      pp_type t
      (Varname.to_string vn)
      pp_exprs es

and pp_lvalues fmt lvs =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_lvalue fmt v
  in fprintf fmt "(%a)"
    f lvs

and pp_lvalue fmt = function
  | ArgLV (t, arg) ->
    fprintf fmt "(arg-lvalue@ %a@ %a)"
      pp_type t
      pp_arg arg
  | CrossbindLV (t, lvs) ->
    fprintf fmt "(crossbind-lvalue@ %a@ %a)"
      pp_type t
      pp_lvalues lvs

and pp_stmts fmt ss =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_stmt fmt v
  in fprintf fmt "(%a)"
    f ss

and pp_stmt fmt = function
  | LetS (lv,e) ->
    fprintf fmt "@[<2>(let-stmt@ %a@ %a)@]"
      pp_lvalue lv
      pp_expr e
  | AssertS (e,str) ->
    fprintf fmt "@[<2>(assert-stmt@ %a@ %s)@]"
      pp_expr e
      str
  (* return is the only statement where it helps to
   * display the type *)
  | ReturnS(t,e) ->
    fprintf fmt "@[<2>(return-stmt@ %a@ %a)@]"
      pp_type t
      pp_expr e

and pp_cmds fmt cs =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_cmd fmt v
  in fprintf fmt "(%a)"
    f cs

(* NOTE all commands have type UNIT except the function command *)
and pp_cmd fmt = function
  | ReadimgC (fn,a) ->
    fprintf fmt "@[<2>(read-image-cmd@ %s@ %a)@]"
      (Filename.to_string fn)
      pp_arg a
  | ReadvidC (fn,a) ->
    fprintf fmt "@[<2>(read-video-cmd@ %s@ %a)@]"
      (Filename.to_string fn)
      pp_arg a
  | WriteimgC (e,fn) ->
    fprintf fmt "@[<2>(write-image-cmd@ %a@ %s)@]"
      pp_expr e
      (Filename.to_string fn)
  | WritevidC (e,fn) ->
    fprintf fmt "@[<2>(write-video-cmd@ %a@ %s)@]"
      pp_expr e
      (Filename.to_string fn)
  | PrintC str ->
    fprintf fmt "@[<2>(print-cmd@ \"%s\")@]" str
  | ShowC e ->
    fprintf fmt "@[<2>(show-cmd@ %a)@]"
      pp_expr e
  | TimeC c ->
    fprintf fmt "@[<2>(time-cmd@ %a)@]"
      pp_cmd c
  | FnC (t,vn,bs,te,ss) ->
    fprintf fmt "@[<2>(function@ %s@ @;%a@ %a@ %a@ @[<hov 2>%a@])@]"
      (Varname.to_string vn)
      pp_type t (* now printing the arrow type *)
      pp_bindings bs
      pp_type te
      pp_stmts ss
  | StmtC s ->
    fprintf fmt "@[<2>(stmt-cmd@ %a)@]"
      pp_stmt s

let pp_prog fmt (p : Ast.prog) =
  fprintf fmt "@[<2>(jpl-typed-prog@;%a)@]@?"
    pp_cmds p
