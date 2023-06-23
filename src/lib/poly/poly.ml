module type DOMAIN = sig
  type t
  val zero : t
  val one : t
  val neg : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end

module type ATOM = sig
  type t
  val equal : t -> t -> bool
end

module Polynomial (C : DOMAIN) (A : ATOM) = struct
  let locate pred =
    let rec locate_aux rev = function
        [] ->
        rev, []
      | hd::tl ->
        if pred hd then
          rev, hd::tl
        else
          locate_aux (hd::rev) tl in
    locate_aux []

  let rec similar l1 l2 =
    match l1, l2 with
      [], [] ->
      true
    | [], _ ->
      false
    | hd::tl, _ ->
      match locate (A.equal hd) l2 with
        _, [] -> false
      | rev, _::rest -> similar tl (List.rev_append rev rest)

  type t = (C.t * A.t list) list

  let zero : t = []

  let one : t = [(C.one, [])]

  let neg : t -> t = List.map (fun (c, l) -> C.neg c, l)

  let add : t -> t -> t =
    let rec add_aux accum p1 p2 =
      match p1 with
        [] ->
        List.rev_append accum p2
      | (c1, l1)::tl ->
        match locate (fun (_, l2) -> similar l1 l2) p2 with
          _, [] ->
          add_aux
            ((c1, l1)::accum)
            tl
            p2
        | rev, (c2, _)::rest ->
          add_aux
            ((C.add c1 c2, l1)::accum)
            tl
            (List.rev_append rev rest) in
    add_aux []

  let mul p1 p2 =
    let rec mul_aux accum = function
        [] ->
        accum
      | (c1, l1)::tl ->
        mul_aux
          (List.map
             (fun (c2, l2) -> C.mul c1 c2, l1 @ l2)
             p2
           |> add accum)
          tl in
    mul_aux [] p1
end
