module type MONAD = sig
  type 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module Option : MONAD with type 'a t = 'a option = struct
  type 'a t = 'a option

  let ( let* ) = Option.bind

  let return = Option.some
end

module Reader (E : sig type t end) : MONAD with type 'a t = E.t -> 'a = struct
  type 'a t = E.t -> 'a

  let ( let* ) r f e = f (r e) e

  let return a _ = a
end

module State (S : sig type t end) : MONAD with type 'a t = S.t -> 'a * S.t = struct
  type 'a t = S.t -> 'a * S.t

  let ( let* ) s f s0 =
    let a, s1 = s s0 in
    f a s1

  let return a s = a, s
end
