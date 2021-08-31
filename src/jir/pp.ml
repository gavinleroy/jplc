(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Format
open Jir_lang
open Runtime

let jir_msg =
  "// JIR program dump, view at your own risk. ~Gavin :)"

let rec ident fmt s = fprintf fmt "%s" s

and pp_const fmt = function
  | INT i64 -> fprintf fmt "%Li" i64
  | FLOAT f -> fprintf fmt "%f" f
  | TRUE -> fprintf fmt "true"
  | FALSE -> fprintf fmt "false"
  | STATIC_STRING str -> printf "\"%s\"" str

and pp_lvalues fmt ls =
  pp_print_list
    ~pp_sep:(fun ft () -> fprintf ft ", ")
    pp_lvalue fmt ls

and pp_lvalue fmt = function
  | UserBinding (str, i) -> fprintf fmt "%s.%i" str i
  | Temp i -> fprintf fmt "temp.%d" i
  | Symbol s -> fprintf fmt "%s" s

and pp_phi_vs fmt ls =
  let pp_phi_vs' f (lv, tag) =
    fprintf f "(%a %a)"
      pp_lvalue lv
      pp_bb_tag tag
  in
  pp_print_list
    ~pp_sep:pp_print_space
    pp_phi_vs' fmt ls

and pp_rvalue fmt = function
  | UnopRV (uop, lv) ->
    fprintf fmt "%a %a"
      Ast_utils.pp_unop uop
      pp_lvalue lv
  | BinopRV (lvl, bop, lvr) ->
    fprintf fmt "%a %a %a"
      pp_lvalue lvl
      Ast_utils.pp_binop bop
      pp_lvalue lvr
  | VarRV lvl -> pp_lvalue fmt lvl
  | CastRV (ty, lvl) ->
    fprintf fmt "%a : %s"
      pp_lvalue lvl
      (code_of_type ty)
  | PhiRV { ty; paths } ->
    fprintf fmt "phi %a : %s"
      pp_phi_vs paths
      (code_of_type ty)
  | ConstantRV const -> pp_const fmt const

and pp_binding fmt (lv, ty) =
  fprintf fmt "@[%a : %s@]"
    pp_lvalue lv
    (code_of_type ty)

and pp_bindings fmt ls =
  pp_print_list
    ~pp_sep:pp_force_newline
    pp_binding fmt ls

and pp_statement fmt = function
  | Bind (lv, ty, rv) ->
    fprintf fmt "@[<hov 2>%a@ :@ %s@ =@ %a;@]"
      pp_lvalue lv
      (code_of_type ty)
      pp_rvalue rv

and pp_bb_tag fmt i = fprintf fmt "BB.%i" i

and pp_terminator fmt = function
  | Goto i -> fprintf fmt "@[<hov 2>goto@ %a;@]" pp_bb_tag i

  | Ite { cond; if_bb; else_bb; merge_bb } ->
    fprintf fmt "@[<hov 2>if(%a@ |@ true@ ->@ %a@ |@ false@ ->@ %a)@ ->@ %a@]"
      pp_lvalue cond
      pp_bb_tag if_bb
      pp_bb_tag else_bb
      pp_bb_tag merge_bb

  | Call { fn_name; params; write_to; success_jump_to } ->
    fprintf fmt "@[<hov 2>call(%a@ =@ %a(%a))@ ->@ %a@]"
      pp_lvalue write_to
      pp_lvalue fn_name
      pp_lvalues params
      pp_bb_tag success_jump_to

  | Return lv -> fprintf fmt "@[<hov 2>return@ %a;@]" pp_lvalue lv

and pp_ss fmt ls =
  pp_print_list
    ~pp_sep:pp_force_newline
    pp_statement fmt ls

and pp_bb fmt = function
  | BB { id; stmts; term } ->
    fprintf fmt "@[<hov 2>%a {@\n%a@\n%a@\n}@]"
      pp_bb_tag id
      pp_ss stmts
      pp_terminator term

and pp_bbs fmt bs =
  pp_print_list
    ~pp_sep:pp_force_newline
    pp_bb fmt bs

and pp_tys fmt ts =
  pp_print_list ~pp_sep:(fun fm () -> fprintf fm ", ")
    (fun fm a -> fprintf fm "%s" (code_of_type a) )
    fmt ts

and pp_fn fmt { name
              ; signature
              ; bindings
              ; body } =
  (* the signature should /always/ be an arrow type *)
  let (rt, ps) = (function
      | ArrowRT (rt, ps) -> (rt, ps)
      | _ -> assert false) signature in
  fprintf fmt
    "@[<hov 2>%a (%a) -> %s {@\n%a@\n@\n%a@\n}@]"
    pp_lvalue name
    pp_tys ps
    (rt |> code_of_type)
    pp_bindings bindings
    pp_bbs body

and pp_jir fmt { main; globals; prog; } =
  let pp_jirs fmt js =
    pp_print_list
      ~pp_sep:pp_force_newline
      pp_fn fmt js
  in
  fprintf fmt "@[<hov>%s@\n%a@\n%a@]@?"
    jir_msg
    pp_bindings globals
    pp_jirs (main :: prog)