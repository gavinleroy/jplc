(* Gavin Gray *)

open Ctypes
open Foreign

type pict
let pict : pict structure typ  = structure "pict"
let rows = field pict "rows" int64_t
let cols = field pict "cols" int64_t
let data = field pict "data" (ptr double)
let () = seal pict

let get_time =
  foreign "get_time" (void @-> returning double)

let show =
  foreign "show" ~check_errno:true (string @-> ptr void @-> returning int32_t)

let print =
  foreign "print" (string @-> returning void)

let read_image' =
  foreign "read_image" (string @-> returning pict)

let read_image fn =
  read_image' (Ast_utils.Filename.to_string fn)

let write_image' =
  foreign "write_image" (pict @-> string @-> returning void)

let write_image img fn =
  write_image' img (Ast_utils.Filename.to_string fn)

let has_size =
  foreign "has_size" (pict @-> int32_t @-> int32_t @-> returning bool)

let sepia =
  foreign "sepia" (pict @-> returning pict)

let blur =
  foreign "blur" (pict @-> double @-> returning pict)

let resize =
  foreign "resize" (pict @-> int32_t @-> int32_t @-> returning pict)

let crop =
  foreign "crop" (pict @-> int32_t @-> int32_t @-> int32_t @-> int32_t @-> returning pict)
