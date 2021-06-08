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
  val liftM: 'a t -> f:('a -> 'b) -> 'b t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ): 'a t -> ('a -> 'b) -> 'b t
  val run: 'a t -> 'a
end

module Identity = struct
  type 'a t =
    | Identity of 'a
  let run = function
    | Identity a -> a
  let return x = Identity x
  let bind m ~f : 'b t = run m |> f
  let liftM m ~f = run m |> f |> return
  let ( >>= ) m f = bind m ~f:f
  let ( >>| ) m f = liftM m ~f:f
end

module StateT (S: MONOID) (M: MONAD) = struct
  type 'a t =
    | StateT of (S.t -> (('a * S.t) M.t))
  let run_state_t (StateT f) = f
  let eval_state_t m s =
    M.bind (run_state_t m s) ~f:(fun (a, _) ->
        M.return a)
  let exec_state_t m s =
    M.bind (run_state_t m s) ~f:(fun (_, s') ->
        M.return s')
  let run (StateT m) =
    M.run (M.bind (m (S.mempty ())) ~f:(fun (x, _) ->
        M.return x))
  let return a = StateT (fun s ->
      M.return (a, s))
  let bind (StateT fa) ~f = StateT (fun s ->
      M.bind (fa s) ~f:(fun (a, s') ->
          run_state_t (f a) s'))
  let liftM (StateT fa) ~f = StateT (fun s ->
      M.bind (fa s) ~f:(fun (a, s') ->
          M.return (f a, s')))
  let map = `Custom liftM
  let ( >>= ) m f = bind m ~f:f
  let ( >>| ) m f = liftM m ~f:f
  (* state monad functions *)
  let get = StateT (fun s ->
      M.return (s, s))
  let put s = StateT (fun _ ->
      M.return ((), s))
  let modify f = StateT (fun s ->
      M.return ((), f s))
  let lift m = StateT (fun s -> (* XXX monad transformer class *)
      M.bind m ~f:(fun a ->
          M.return (a, s)))
end

module State (S: MONOID) = struct
  module St = StateT(S)(Identity)
  include St
  let state f = StateT (fun s ->
      f s |> Identity.return)
  let run_state m = (fun s ->
      run_state_t m |> fun f ->
      f s |> Identity.run)
  let eval_state m s =
    run_state m s |> fst
  let exec_state m s =
    run_state m s |> snd
end

module Basic = struct
  let curry f = fun fst snd -> f (fst, snd) [@@inline always]
  let uncurry f = fun (fst, snd) -> f fst snd [@@inline always]
  let ( <.> ) f g = fun x -> f (g x) [@@inline always]
  let ( <$> ) f v = f v [@@inline always]
end

module Utils (M : MONAD) = struct
  open M
  include Basic

  let fold_m (xs : 'a list)
      ~(f : 'b -> 'a -> 'b t) ~(init : 'b) : 'b t =
    let c = (fun x k z -> (f z x) >>= k) in
    let go = List.fold_right xs ~f:c ~init:return in
    go init

  let map_m (xs : 'a list) ~(f : 'a -> 'b t)
    : ('b list) t =
    let cons_f = (fun x ys ->
        f x >>= fun x' ->
        ys >>= fun ys' -> return (x' :: ys')) in
    List.fold_right ~f:cons_f ~init:(return []) xs

  let map_m_ (xs : 'a list) ~(f : 'a -> 'b t)
    : unit t =
    let c = (fun x k -> (f x) >>= fun _ -> k) in
    List.fold_right xs ~f:c ~init:(return ())

  let ( >> ) (m : 'a t) (k : 'b t) : 'b t =
    m >>= fun _ -> k
end
