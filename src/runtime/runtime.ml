(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

open Ast_utils

(* An interface function includes not only
 * the overall ArrowT type but also the
 * return and param list types separately
 * to make it easy to get the information needed *)
type interface_function =
  { name        : string
  ; arrow_type  : type_expr
  ; return_type : type_expr
  ; params      : type_expr list }

(* Function for getting the current time *)
let get_time_info =
  { name          = "get_time"
  ; arrow_type    = ArrowT (FloatT, [])
  ; return_type   = FloatT
  ; params        = [] }

(****************************)
(* Full C Library Interface *)
(****************************)

(*
 *  #include <stdint.h>
 * struct pict {
 *   int rows;
 *   int cols;
 *   double *data;
 * };
 *
 * int64_t sub_ints(int64_t a, int64_t b);
 * double sub_floats(double a, double b);
 * int32_t has_size(struct pict input, int64_t rows, int64_t cols);
 * struct pict sepia(struct pict input);
 * struct pict blur(struct pict input, double radius);
 * struct pict resize(struct pict input, int64_t rows, int64_t cols);
 * struct pict crop(struct pict input, int64_t top, int64_t left, int64_t bottom,
 *                  int64_t right);
 *
 * struct pict read_image(char *filename);
 * void print(char *text);
 * void write_image(struct pict input, char *filename);
 * void show(char *typestring, void *datum);
 * void fail_assertion(char *text);
 * double get_time(void);
 *  *)
