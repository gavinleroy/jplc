(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

let print_sexp ?(channel = Out_channel.stdout) sexp =
   let formatter = Format.formatter_of_out_channel channel in
   Sexp.pp_hum_indent 1 formatter sexp;
   Format.pp_print_flush formatter ();
   Out_channel.flush channel
