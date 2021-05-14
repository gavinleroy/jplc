(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

val cerr_msg: pos:Lexing.position -> t:string -> msg:string -> (_, Error.t) Result.t
