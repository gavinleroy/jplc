(* Gavin Gray 09.2021 *)

open Format

open Ty

let rec pp_list fmt ls =
  Fmt.list ~sep:(fun fmt () ->
      fprintf fmt ", ") show_value fmt ls

and show_value fmt = function
  | UnitIT -> assert false
  | IntIT i -> fprintf fmt "%d" i
  | FloatIT f -> fprintf fmt "%f" f
  | BoolIT b -> fprintf fmt "%s" (if b then "true" else "false")
  | ListIT ls -> fprintf fmt "%a" pp_list ls
  | TupleIT arr -> fprintf fmt "{@ %a@ }" pp_list (Array.to_list arr)
  | ArrayIT arr -> fprintf fmt "[@ %a@ ]" pp_list (Array.to_list arr)

let show v =
  fprintf std_formatter "@[SHOW@ :@ %a@]@\n@?"
    show_value v
