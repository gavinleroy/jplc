(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

type runtime_type =
  | UnitRT | BoolRT
  | IntRT
  | FloatRT
  | StringRT
  | ArrayRT of runtime_type * int
  | CrossRT of runtime_type list
  | ArrowRT of runtime_type * runtime_type list

val string_of_rtype: runtime_type -> string

val code_of_type: runtime_type -> string

(* An interface function includes not only
 * the overall ArrowT type but also the
 * return and param list types separately
 * to make it easy to get the information needed *)
type interface_function =
  { name        : string
  ; arrow_type  : runtime_type
  ; return_type : runtime_type
  ; params      : runtime_type list }

(*********************************)
(* available interface functinos *)
(*********************************)

val read_img_info: interface_function

val write_img_info: interface_function

val print_info: interface_function

val get_time_info: interface_function
