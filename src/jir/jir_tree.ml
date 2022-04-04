(************************)
(*      Gavin Gray      *)
(*       07.2021        *)
(************************)

module type CPSIR = sig

  type name
  type literal
  type value_primitive
  type test_primitive

  type atom =
    | AtomN of name
    | AtomL of literal

  type tree =
    | LetP of { name : name; prim: value_primitive; args: atom list; body: tree }
    | LetC of { cnts : cnt list; body: tree }
    | LetF of { funs: lambda list }
    | AppC of { cnt: name; args: atom list; }
    | AppF of { lambda: atom; retc: name; args: atom list }
    | If of { cond: test_primitive; args: atom list; then_c: name; else_c: name }
    | Halt of atom

  and cnt = { name: name; args: name list; body: tree }
  and lambda = { fname: name; ret_c: name; fargs: name list; fbody: tree }

end

module Symbol : sig
  type t
  val fresh : ?stem:string -> unit -> t
  val of_string : string -> t
end = struct
  type t = string
  let count = ref 0
  let fresh ?(stem = "t.") () =
    begin
      incr count;
      stem ^ (string_of_int !count)
    end
  let of_string s = s
end

type lit =
  | IntLit of int64
  | FloatLit of float
  | BoolLit of bool
  | UnitLit

(* TODO create modules more elegantly *)
module Jir_high = struct

  type name = Symbol.t
  type literal = lit
  type value_primitive = Runtime.value_primitive
  type test_primitive = Runtime.test_primitive

  type atom =
    | AtomN of name
    | AtomL of literal

  type tree =
    | LetP of { name : name; prim: value_primitive; args: atom list; body: tree }
    | LetC of { cnts : cnt list; body: tree }
    | LetF of { funs: lambda list }
    | AppC of { cnt: name; args: atom list; }
    | AppF of { lambda: atom; retc: name; args: atom list }
    | If of { cond: test_primitive; args: atom list; then_c: name; else_c: name }
    | Halt of atom

  and cnt = { name: name; args: name list; body: tree }
  and lambda = { fname: name; ret_c: name; fargs: name list; fbody: tree }

end
