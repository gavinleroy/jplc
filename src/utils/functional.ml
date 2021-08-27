(************************)
(*      Gavin Gray      *)
(*       05.2021        *)
(************************)

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

(* FIXME earlier the JaneStreet Core 'Or_error' module
 * was used but I am trying to get away from using he
 * Core library.
 * Instead of using 'string' for the error variant something
 * a little smarter should be used. *)
module Or_error = struct
  include Result
  type 'a t = ('a, string) Result.t
  let return a = Ok a
  let run = function
    | Ok a -> a
    (* NOTE run shouldn't be called in an Error_option *)
    | Error _ -> assert false
  let liftM m ~f = run m |> f |> return
  let bind at ~f =
    match at with
    | Ok a -> f a
    | Error msg -> Error msg
  let ( >>= ) v f = bind v ~f:f
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
  let modify_ f = StateT (fun s ->
      f s; (* NOTE to use this the `state` is mutable *)
      M.return ((), s))
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
  let flip f = fun a b -> f b a [@@inline always]
  (* Basic Utilities *)
  let list_zip ls rs =
    let rec loop acc l r =
      match l, r with
      | [], [] -> `Ok (List.rev acc)
      | [], _ | _, [] -> `Unequal_lengths
      | (lv :: l'), (rv :: r') ->
        loop ((lv, rv) :: acc) l' r'
    in loop [] ls rs
  let all_equal xs ~equal =
    let all_equal' x xs ~equal =
      List.fold_left (fun acc v ->
          acc && equal x v)
        true xs
    in match xs with
    | [] | [_] -> true
    | x :: xs' ->
      all_equal' x xs' ~equal:equal
  let list_hd = function
    | [] -> None
    | hd :: _ -> Some hd
  let list_find f ls =
    try List.find f ls
        |> Some
    with Not_found -> None
  let list_equal equal ls rs =
    match list_zip ls rs with
    | `Ok zipped ->
      let zs = List.map (fun (a, b) ->
          equal a b) zipped in
      List.fold_right (&&) zs true
    | `Unequal_lengths -> false
  let ident x = x
end

module Utils (M : MONAD) = struct
  open M
  include Basic

  let fold_m (xs : 'a list)
      ~(f : 'b -> 'a -> 'b t) ~(init : 'b) : 'b t =
    let c = (fun x k z -> (f z x) >>= k) in
    let go = List.fold_right c xs return in
    go init

  let map_m (xs : 'a list) ~(f : 'a -> 'b t)
    : ('b list) t =
    let cons_f = (fun x ys ->
        f x >>= fun x' ->
        ys >>= fun ys' -> return (x' :: ys')) in
    List.fold_right cons_f xs (return [])

  let map_m_ (xs : 'a list) ~(f : 'a -> 'b t)
    : unit t =
    let c = (fun x k -> (f x) >>= fun _ -> k) in
    List.fold_right c xs (return ())

  let ( >> ) (m : 'a t) (k : 'b t) : 'b t =
    m >>= fun _ -> k
end
