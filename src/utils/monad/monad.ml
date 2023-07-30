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

module Utility (M : MONAD) = struct
  open M

  let rev_mapM f =
    let rec rev_mapM_aux accum = function
        [] ->
        return accum
      | hd::tl ->
        let* b = f hd in
        rev_mapM_aux (b::accum) tl in
    rev_mapM_aux []

  let rev_replicateM m =
    let rec rev_replicateM_aux accum count =
      if count <= 0 then
        return accum
      else
        let* m in
        rev_replicateM_aux (m::accum) (count - 1) in
    rev_replicateM_aux []

  let rev_ListTransformerM xs =
    let rec l_transformerM_aux accum = function
      | [] -> return accum
      | hd :: tl ->
        let* hd in
        l_transformerM_aux (hd :: accum) tl
      in
    l_transformerM_aux [] xs

end
