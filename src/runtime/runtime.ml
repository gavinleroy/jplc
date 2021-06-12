(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

open Core

type runtime_type =
  | UnitRT
  | BoolRT
  | IntRT
  | FloatRT
  | StringRT
  | ArrayRT of runtime_type * int
  | CrossRT of runtime_type list
  | ArrowRT of runtime_type * runtime_type list

let rec sexp_of_rtype t =
  let open Sexp in
  match t with
  | UnitRT -> Atom "UnitT"
  | BoolRT -> Atom "BoolT"
  | IntRT -> Atom "IntT"
  | FloatRT -> Atom "FloatT"
  | StringRT -> Atom "StringT"
  | ArrayRT(rt, i) ->
    List [ Atom "ArrayT"; sexp_of_rtype rt; Atom "rank ="; Atom (Int.to_string i) ]
  | CrossRT(rts) ->
    List [ Atom "CrossT"; List.sexp_of_t sexp_of_rtype rts ]
  | ArrowRT(rt, rts) ->
    List [ Atom "FnT"; sexp_of_rtype rt; List.sexp_of_t sexp_of_rtype rts ]

let repeat s n =
  let rec helper s1 n1 =
    if n1 = 0 then s1 else helper (s1 ^ s) (n1 - 1)
  in helper "" n

let rec code_of_type t =
  let open Printf in
  let concat_with = fun sep ->
    List.fold_left ~init:"" ~f:(fun a b -> a ^ sep ^ b) in
  match t with
  | UnitRT -> "unit"
  | BoolRT -> "bool"
  | IntRT -> "int"
  | FloatRT -> "float"
  | StringRT -> "string"
  | ArrayRT(rt, i) -> sprintf "%s[%s]" (code_of_type rt) (repeat "," (i-1))
  | CrossRT(rts) -> sprintf "{ %s }" (List.map rts ~f:code_of_type
                                      |> concat_with ", ")
  | ArrowRT(rt, rts) -> sprintf "( %s )" (List.map (rts @ [rt]) ~f:code_of_type
                                          |> concat_with " -> ")

(* The PICT type is something that is commonly passed around
 * and used in functions. This is just a shorthand for referencing
 * this type.
 *
 * A PICT is equivalent to float4[,] at the source level. The tuple
 * (float * float * float * float) refers to the
 * (red * green * blue * alpha) channels of a pixel respectively. *)
let pict =
  ArrayRT (CrossRT [FloatRT; FloatRT; FloatRT; FloatRT], 2)

(* An interface function includes not only
 * the overall ArrowT type but also the
 * return and param list types separately
 * to make it easy to get the information needed *)
type interface_function =
  { name        : string
  ; arrow_type  : runtime_type
  ; return_type : runtime_type
  ; params      : runtime_type list }

(* Function for getting the current time *)
let get_time_info =
  { name          = "get_time"
  ; arrow_type    = ArrowRT (FloatRT, [])
  ; return_type   = FloatRT
  ; params        = [] }

(* function for reading an image *)
let read_img_info =
  { name          = "read_image"
  ; arrow_type    = ArrowRT ( pict, [StringRT] )
  ; return_type   = pict
  ; params        = [StringRT]}

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
