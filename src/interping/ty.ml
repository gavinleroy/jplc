(* Gavin Gray 09.2021 *)

type 'a ret_cps_t =
  | UnitIT
  | IntIT of int
  | FloatIT of float
  | BoolIT of bool
  | ListIT of 'a ret_cps_t list
  | TupleIT of 'a ret_cps_t Array.t
  | ArrayIT of 'a ret_cps_t Array.t
  (* FIXME PictIT should be not specific to pictures and should be for matrices in general,
   * it should also be a basic OCaml type *)
  | PictIT of (Runtime.Lib.pict, [ `Struct ]) Ctypes.structured
