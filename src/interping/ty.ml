(************************)
(*      Gavin Gray      *)
(*       09.2021        *)
(************************)

type 'a ret_cps_t =
  | UnitIT
  | IntIT of int
  | FloatIT of float
  | BoolIT of bool
  | ListIT of 'a ret_cps_t list
  | TupleIT of 'a ret_cps_t array
  | ArrayIT of 'a ret_cps_t array
