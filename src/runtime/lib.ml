(* Gavin Gray *)

open Ctypes
open Foreign

let get_time =
  foreign "get_time" (void @-> returning double)

let show =
  foreign "show" (string @-> ptr void @-> returning int32_t)

let print =
  foreign "print" (string @-> returning void)
