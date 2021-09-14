(* Gavin Gray 09.2021 *)

(* type pict_data = (\* NOTE for the tuple : (r, g, b, alpha) channels *\)
 *   (float * float * float * float)
 *     Ctypes_static.typ
 *     Ctypes.CArray.t
 *     Ctypes_static.typ
 *     Ctypes.CArray.t *)

type 'a ret_cps_t =
  | UnitIT
  | IntIT of int
  | FloatIT of float
  | BoolIT of bool
  | ListIT of 'a ret_cps_t list
  | TupleIT of 'a ret_cps_t Array.t
  | ArrayIT of 'a ret_cps_t Array.t
  (* NOTE used only for calling read/write C functions *)
(* | PictIT of { rows : int
 *             ; cols : int
 *             ; data : pict_data } *)
