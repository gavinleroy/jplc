(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core
open Ast_utils

type t =
  { seen_syms : (string, int, String.comparator_witness) Map.t
  ; env       : (string * string) list list
  ; new_count : int }

(* TODO account for the provided runtime env *)
let empty () =
  { seen_syms = Map.empty (module String)
  ; env       = [[]]
  ; new_count = 0 }

let add_symbol e vn =
  let vn = Varname.to_string vn in
  let m = Map.update e.seen_syms vn
      ~f:(fun vo -> match vo with
          | Some c -> c+1
          | None -> 0) in
  let c = Map.find_exn m vn in
  let newstr = Printf.sprintf "%s.%d" vn c in
  let env' = match e.env with
    | scope :: ss -> ((vn, newstr) :: scope) :: ss
    | [] -> [[(vn, newstr)]] in
  newstr, { e with seen_syms = m; env = env' }


(* NOTE should never fail if program is typechecked  *)
let lookup e vn =
  List.find_exn (List.join e.env)
    ~f:(fun tup ->
        String.( = ) (fst tup) (Varname.to_string vn))
  |> snd

let get_unique_var e =
  let newstr = Printf.sprintf "_t.%d" e.new_count in
  newstr, { e with new_count = e.new_count + 1 }

let open_scope v =
  let e = v.env in
  { v with env = [] :: e }

let close_scope v =
  let e = v.env in
  try { v with env = List.tl_exn e } with
  | _ -> v
