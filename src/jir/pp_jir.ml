(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

open Core
open Format
open Jir_lang
open Runtime

let jir_msg =
  "// JIR program dump, view at your own risk. ~Gavin :)"

let rec ident fmt s = fprintf fmt "%s" s

and pp_binop fmt = function
  | `Lt -> fprintf fmt "<"
  | `Gt -> fprintf fmt ">"
  | `Cmp -> fprintf fmt "=="
  | `Lte -> fprintf fmt "<="
  | `Gte -> fprintf fmt ">="
  | `Neq -> fprintf fmt "!="
  | `Mul -> fprintf fmt "*"
  | `Div -> fprintf fmt "/"
  | `Mod -> fprintf fmt "%%"
  | `Plus -> fprintf fmt "+"
  | `Minus -> fprintf fmt "-"

and pp_unop fmt = function
  | `Bang -> fprintf fmt "!"
  | `Neg  -> fprintf fmt "-"

and pp_const fmt = function
  | INT i64 -> fprintf fmt "%Li" i64
  | FLOAT f -> fprintf fmt "%f" f
  | TRUE -> fprintf fmt "true"
  | FALSE -> fprintf fmt "false"
  | STATIC_STRING str -> printf "\"%s\"" str

and pp_lvalue fmt = function
  | UserBinding (str, i) -> fprintf fmt "%s.%i" str i
  | Temp i -> fprintf fmt "temp.%d" i

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
      pp_unop uop
      pp_lvalue lv
  | BinopRV (lvl, bop, lvr) ->
    fprintf fmt "%a %a %a"
      pp_lvalue lvl
      pp_binop bop
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

  | Iet { cond; if_bb; else_bb } ->
    fprintf fmt "@[<hov 2>if(%a@ |@ true@ ->@ %a@ |@ false@ ->@ %a)@]"
      pp_lvalue cond
      pp_bb_tag if_bb
      pp_bb_tag else_bb

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
    "@[<hov 2>%s (%a) -> %s {@\n%a@\n@\n%a@\n}@]"
    name
    pp_tys ps
    (rt |> code_of_type)
    pp_bindings bindings
    pp_bbs (Array.to_list body)

and pp_jir fmt { main; prog; } =
  let pp_jirs fmt js =
    pp_print_list
      ~pp_sep:pp_force_newline
      pp_fn fmt js
  in
  fprintf fmt "@[<hov>%s@\n%a@]@.@?"
    jir_msg
    pp_jirs (main :: prog)

let stdout_of_jir p =
  pp_jir std_formatter p;
  ""
