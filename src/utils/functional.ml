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

module State (S: MONOID) = struct
  type 'a t =
    | State of (S.t -> ('a * S.t))

  (* val run_state: 'a t -> S.t -> ('a * S.t) *)
  let run_state = function
    | State f -> fun s ->
      f s
  (* val get: S.t t *)
  let get = State
      (fun s -> (s, s))
  (* val put: S.t -> unit t *)
  let put s' = State
      (fun _s -> ((), s'))

  (* val ( >>= ): 'a t-> ('a -> 'b t) -> 'b t *)
  let ( >>= ) sa k =
    State (fun s ->
        let (a, s') = run_state sa s in
        let sb = k a in
        run_state sb s')

  (* val return: 'a -> 'a t *)
  let return a = State
      (fun s -> (a, s))

  (* val ( >>| ): 'a t -> ('a -> 'b) -> 'b t *)
  let ( >>| ) a f =
    return (f a)

  (* val join: 'a t t -> 'a t *)
  let join ssa = State
      (fun s ->
         let (sa, s') = run_state ssa s in
         run_state sa s')

  (* val all: ('a t) list -> ('a list) t *)
  let all stas = State
      (fun s ->
         let vs = List.map stas
             ~f:(fun sa -> run_state sa s) in
         let st = List.fold_left vs ~init:(S.mempty ())
             ~f:(fun acc t -> snd t |> S.mappend acc) in
         List.map vs ~f:fst, st)
end

module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val join: 'a t t -> 'a t
  val all: ('a t) list -> ('a list) t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ): 'a t -> ('a -> 'b) -> 'b t
end

module Utils (M : MONAD) = struct
  include M

  let foldMN
      (r1 : 'd t -> 'e t)
      (r2 : 'd t -> 'f t)
      (unwrap : ('b -> 'a -> 'd t) -> 'a -> 'd -> 'd t)
      (f : 'b -> 'a -> 'd t)
      (env : 'b) (xs : 'a list) =
    let rec foldM' prev xs =
      match xs with
      | [] -> [r1 prev], r2 prev
      | y :: ys ->
        foldM' (prev >>= unwrap f y) ys
        |> fun (vs', env') ->
        ((r1 prev) :: vs'), env' in
    match xs with
    | [] -> return ([], env)
    | x :: xs ->
      foldM' (f env x) xs
      |> fun (vs, env') ->
      all vs >>= fun vs' ->
      env' >>| fun env'' -> vs', env''

  let foldM3 f env xs =
    foldMN
      (fun a -> a >>| snd3)
      (fun a -> a >>| trd3)
      (fun g v (_,_,e) -> g e v) f env xs

  let foldM2 f env xs =
    foldMN
      (fun a -> a >>| fst)
      (fun a -> a >>| snd)
      (fun g v (_,e) -> g e v) f env xs
end
