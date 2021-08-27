(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Format
open Ast
open Ast_utils

let rec pp_loopbinds fmt lbs =
  pp_print_list
    ~pp_sep:pp_print_space
    pp_loopbind fmt lbs

and pp_loopbind fmt (a, b) =
  fprintf fmt "(%s@ %a)"
    (Varname.to_string a)
    pp_expr b

and pp_exprs fmt es =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_expr fmt v
  in fprintf fmt "(%a)"
    f es

and pp_expr fmt = function
  | IntE (_,i) ->
    fprintf fmt "%Ld" i
  | FloatE (_, f) ->
    fprintf fmt "%f" f
  | TrueE _ ->
    fprintf fmt "#t"
  | FalseE _ ->
    fprintf fmt "#f"
  | VarE (_, vn) ->
    fprintf fmt "%s"
      (Varname.to_string vn)
  | CrossE (_,es) ->
    fprintf fmt "@[<hov 2>(cross-expr@ %a)@]"
      pp_exprs es
  | ArrayCE (_,es) ->
    fprintf fmt "@[<hov 2>(array-expr@ %a)@]"
      pp_exprs es
  | BinopE (_,lhs,op,rhs) ->
    fprintf fmt "@[<hov 2>(binop-expr@ %a@ %a@ %a)@]"
      pp_binop op
      pp_expr lhs
      pp_expr rhs
  | UnopE (_,op,e') ->
    fprintf fmt "@[<hov 2>(unop-expr@ %a@ %a)@]"
      pp_unop op
      pp_expr e'
  | CastE(_,e',te) ->
    fprintf fmt "@[<hov 2>(cast-expr@ %a@ %a)@]"
      pp_expr e'
      pp_type te
  | CrossidxE (_,e',i) ->
    fprintf fmt "@[<hov 2>(cross-idx-expr@ %a@ %Ld)@]"
      pp_expr e' i
  | ArrayidxE (_,base,idxs) ->
    fprintf fmt "@[<hov 2>(array-idx-expr@ %a@ %a)@]"
      pp_expr base
      pp_exprs idxs
  | IteE (_,cnd,ie,ee) ->
    fprintf fmt "@[<hov 2>(ite-expr@ %a@ %a@ %a)@]"
      pp_expr cnd
      pp_expr ie
      pp_expr ee
  | ArrayLE (_,vnes,bdy) ->
    fprintf fmt "@[<hov 2>(array-loop-expr@ %a@ %a)@]"
      pp_loopbinds vnes
      pp_expr bdy
  | SumLE (_,vnes,bdy) ->
    fprintf fmt "@[<hov 2>(sum-expr@ %a@ %a)@]"
      pp_loopbinds vnes
      pp_expr bdy
  | AppE (_,vn,es) ->
    fprintf fmt "@[<hov 2>(app-expr@ %s@ %a)@]"
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
  | ArgLV (_, arg) ->
    fprintf fmt "(arg-lvalue@ %a)"
      pp_arg arg
  | CrossbindLV (_, lvs) ->
    fprintf fmt "(crossbind-lvalue@ %a)"
      pp_lvalues lvs

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
  | VarA (_,vn) ->
    fprintf fmt "@[<2>(var-arg@ \"%s\")@]"
      (Varname.to_string vn)
  | ArraybindA (_,vn,vns) ->
    fprintf fmt "@[<2>(array-bind-arg@ \"%s\"@ %a)@]"
      (Varname.to_string vn)
      pp_vns vns

and pp_bindings fmt bs =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_binding fmt v
  in fprintf fmt "@[<2>(%a)@]"
    f bs

and pp_binding fmt = function
  | ArgB (_,a,te) ->
    fprintf fmt "(arg-binding@ %a@ %a)"
      pp_arg a
      pp_type te
  | CrossbindB (_,bs) ->
    fprintf fmt "(cross-binding@ %a)"
      pp_bindings bs

and pp_stmts fmt ss =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_stmt fmt v
  in fprintf fmt "(%a)"
    f ss

and pp_stmt fmt = function
  | LetS (_, lv, e) ->
    fprintf fmt "@[<2>(let-stmt@ %a@ %a)@]"
      pp_lvalue lv
      pp_expr e
  | AssertS (_, e,str) ->
    fprintf fmt "@[<2>(assert-stmt@ %a@ %s)@]"
      pp_expr e
      str
  | ReturnS (_, e) ->
    fprintf fmt "@[<2>(return-stmt@ %a)@]"
      pp_expr e

and pp_cmds fmt cs =
  let f = fun fmt v ->
    pp_print_list
      ~pp_sep:pp_print_space
      pp_cmd fmt v
  in fprintf fmt "(%a)"
    f cs

and pp_cmd fmt = function
  | ReadimgC (_, fn, a) ->
    fprintf fmt "@[<2>(read-image-cmd@ %s@ %a)@]"
      (Filename.to_string fn)
      pp_arg a
  | ReadvidC (_, fn, a) ->
    fprintf fmt "@[<2>(read-video-cmd@ %s@ %a)@]"
      (Filename.to_string fn)
      pp_arg a
  | WriteimgC (_, e, fn) ->
    fprintf fmt "@[<2>(write-image-cmd@ %a@ %s)@]"
      pp_expr e
      (Filename.to_string fn)
  | WritevidC (_, e, fn) ->
    fprintf fmt "@[<2>(write-video-cmd@ %a@ %s)@]"
      pp_expr e
      (Filename.to_string fn)
  | PrintC (_, str) ->
    fprintf fmt "@[<2>(print-cmd@ \"%s\")@]" str
  | ShowC (_, e) ->
    fprintf fmt "@[<2>(show-cmd@ %a)@]"
      pp_expr e
  | TimeC (_, c) ->
    fprintf fmt "@[<2>(time-cmd@ %a)@]"
      pp_cmd c
  | FnC (_, vn, bs, te, ss) ->
    fprintf fmt "@[<2>(function@ %s@ %a@ %a@ @[<hov 2>%a@])@]"
      (Varname.to_string vn)
      pp_bindings bs
      pp_type te
      pp_stmts ss
  | StmtC (_, s) ->
    fprintf fmt "@[<2>(stmt-cmd@ %a)@]"
      pp_stmt s

and pp_prog fmt (p : Ast.prog) =
  fprintf fmt "@[<2>(jpl-prog@;%a)@]@?"
    pp_cmds p
