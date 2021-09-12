(* Gavin Gray *)

open Ctypes
open Foreign

let get_time =
  foreign "get_time" (void @-> returning double)

let show =
  foreign "show" (string @-> ptr void @-> returning int32_t)

let print =
  foreign "print" (string @-> returning void)

(* TODO make the following work *)
(* struct pict {
 *     int rows;
 *     int cols;
 *     double *data;
 * }; *)
(* let read_image =
 *   foreign "read_image" (string @-> returning pict) *)
(* let write_image =
 *   foreign "write_image" (pict @-> string @-> returning void) *)
(* let has_size =
 *   foreign "has_size" (pict @-> int32_t @-> int32_t @-> returning int32_t) *)
(* int32_t has_size(struct pict input, int64_t rows, int64_t cols); *)
(* let sepia =
 *   foreign "sepia" (pict @-> returning pict)
 * struct pict sepia(struct pict input); *)
(* let blur =
 *   foreign "blur" (pict @-> double @-> returning pict)
 *   struct pict blur(struct pict input, double radius); *)
(* let resize =
 *   foreign "resize" (pict @-> int32_t @-> int32_t @-> returning pict)
 *     struct pict resize(struct pict input, int64_t rows, int64_t cols); *)
(* let crop =
 *   foreign "crop" (pict @-> int32_t @-> @-> int32_t @-> int32_t @-> int32_t @-> returning pict)
 *   struct pict crop(struct pict input, int64_t top, int64_t left, int64_t bottom, int64_t right); *)
