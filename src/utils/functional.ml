(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

open Core

module type MONOID = sig
  type t
  val mempty: unit -> t
  val mappend: t -> t -> t
end

module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> f:('a -> 'b t) -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val run: 'a t -> 'a
end

module Identity = struct
  type 'a t = 'a
  let run m = m
  let return x = x
  let bind m ~f = f m
  let ( >>= ) m f = bind m ~f:f
end

module StateT (S: MONOID) (M: MONAD) = struct
  type 'a t =
    | StateT of (S.t -> ('a * S.t) M.t)
  let run_state_t = function
    | StateT f -> f
  let eval_state_t m s =
    M.bind (run_state_t m s) ~f:(fun (a, _) ->
        M.return a)
  let run = function
    | StateT m ->
      M.run (M.bind (m (S.mempty ())) ~f:(fun (x, _) ->
          M.return x))
  let return a = StateT (fun s ->
      M.return (a, s))
  let bind sta ~f = StateT (fun s ->
      match sta with
      | StateT fa ->
        M.(>>=) (fa s) (fun (a, s') ->
            run_state_t (f a) s'))
  let ( >>= ) m f = bind m ~f:f
end

module State (S: MONOID) = struct
  module St = StateT(S)(Identity)
  include St
end

module Utils (M : MONAD) = struct
  open M

  let foldM
      (xs : 'a list)
      ~(f : 'b -> 'a -> 'b t)
      ~(init : 'b) : 'b t =
    let c = (fun x k z -> (f z x) >>= k) in
    let go = List.fold_right xs ~f:c ~init:return in
    go init
end
