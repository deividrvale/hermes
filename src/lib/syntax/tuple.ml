module type COEF = sig
  type t
  val one : t
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
end

module type ATOM = sig
  type t
  val equal : t -> t -> bool
end

module type POLY = functor (C : COEF) (A : ATOM) -> sig
  val similar : A.t list -> A.t list -> bool
  include COEF
end with type t = (C.t * A.t list) list

module Poly : POLY = functor (C : COEF) (A : ATOM) -> struct
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

  let one = (C.one, [])::[]

  let neg = List.map (fun (c, l) -> C.neg c, l)

  let add =
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

  let sub p1 p2 = add p1 (neg p2)

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

type ('a, 'b, 'c) expr =
    Abs of ('a -> 'b)
  | Ret of 'c

module IntVec = struct
  module Coef = Poly(Int)(Int)

  module Atom = struct
    type repr = ..

    type t = { simpl : (Coef.t * t list) list;
               exhib : repr;
               equal : t -> bool }

    let equal { equal; _ } = equal

    type repr += Var

    let var vec idx = {
      simpl = List.nth vec idx;
      exhib = Var;
      equal = Fun.const false }

    type repr += Ind of int * int

    let ind var idx =
      let equal = function
          { exhib = Ind (v, i); _ } -> v = var && i = idx
        | _ -> false in
      let rec self = {
        simpl = (Coef.one, self::[])::[];
        exhib = Ind (var, idx);
        equal } in
      self
  end

  module P = Poly(Coef)(Atom)

  let simpl =
    let rec simpl_aux accum = function
        [] ->
        accum
      | (c, l)::tl ->
        simpl_aux
          (List.fold_left
             (fun p (a : Atom.t) -> P.mul p a.simpl)
             [(c, [])]
             l
           |> P.add accum)
          tl in
    simpl_aux []

  type cost = Cost of P.t * (P.t list, cost, unit) expr

  type size = Size of (P.t list, size, P.t list) expr

  let apply
      (Cost (fun_n, Abs fun_c), Size (Abs fun_s))
      (Cost (arg_n, Ret ())   , Size (Ret arg_s)) =
    let Cost (n, f) = fun_c arg_s in
    (Cost (P.add (P.add fun_n arg_n) n, f),
     fun_s arg_s)
end
