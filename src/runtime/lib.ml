(* Gavin Gray *)

open Ctypes
open Foreign

let get_time =
  foreign "get_time" (void @-> returning double)
