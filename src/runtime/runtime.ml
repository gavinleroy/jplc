(************************)
(*      Gavin Gray      *)
(*       06.2021        *)
(************************)

module Lib = Lib

type runtime_type =
  | UnitRT
  | BoolRT
  | IntRT
  | FloatRT
  | StringRT
  | ArrayRT of runtime_type * int
  | CrossRT of runtime_type list
  | ArrowRT of runtime_type * runtime_type list

let repeat s n =
  let rec helper s1 n1 =
    if n1 = 0 then s1 else helper (s1 ^ s) (n1 - 1)
  in helper "" n

let rec code_of_type t =
  let open Printf in
  let  concat_with sep ls =
    let rec loop ls acc = match ls with
      | [] -> acc
      | [x] -> x :: acc
      | x :: y :: ls' ->
        loop (y :: ls') (sep :: x :: acc)
    in
    loop ls []
    |> (fun vs -> List.fold_right (^) vs "")
  in
  match t with
  | UnitRT -> "unit"
  | BoolRT -> "bool"
  | IntRT -> "int"
  | FloatRT -> "float"
  | StringRT -> "string"
  | ArrayRT(rt, i) ->
    sprintf "%s[%s]" (code_of_type rt) (repeat "," (i-1))
  | CrossRT(rts) ->
    sprintf "{ %s }" (List.map code_of_type rts
                      |> concat_with ", ")
  | ArrowRT(rt, rts) ->
    sprintf "( %s )" (List.map code_of_type (rts @ [rt])
                      |> concat_with " -> ")

let string_of_rtype = code_of_type

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
  ; params        = [StringRT] }

(* function for writing an image *)
let write_img_info =
  { name           = "write_image"
  ; arrow_type     = ArrowRT ( UnitRT, [pict; StringRT] )
  ; return_type    = UnitRT
  ; params         = [pict; StringRT]}

let print_info    =
  { name          = "print"
  ; arrow_type    = ArrowRT ( UnitRT, [StringRT] )
  ; return_type   = UnitRT
  ; params        = [StringRT] }

(* NOTE show was /supposed/ to simply take a
 * void* type with a type_string and then would
 * be broken down. I don't like this structure
 * and will rethink how it's done. *)

(* let show_int_info =
 *   { name          = "show_int"
 *   ; arrow_type    = ArrowRT ( UnitRT, [IntRT] )
 *   ; return_type   = UnitRT
 *   ; params        = [StringRT] } *)


(* Runtime primitives *)

type value_primitive =
  (* arithmetic primtiives *)
  | Add
  | Sub
  | Mul
  | Div
  | Mod

  (* bitwise primitives *)
  | ShiftLeft
  | ShiftRight
  | And
  | Or
  | XOr

  (* IO primitives *)
  | ByteRead
  | ByteWrite

(* FFI primitives *)
(* TODO *)

(* TODO primitives for heap interaction *)

type test_primitive =
  | Lt
  | Le
  | Eq

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
