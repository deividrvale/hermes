open Poly

module C = Polynomial(Int)(Int)

module A = struct
  type repr = ..

  type t = { simpl : (C.t * t list) list;
             exhib : repr;
             equal : t -> bool }

  let equal { equal; _ } = equal
end

module P = Polynomial(C)(A)

let simpl =
  let rec simpl_aux accum = function
      [] ->
      accum
    | (c, l)::tl ->
      simpl_aux
        (List.fold_left
           (fun p (a : A.t) -> P.mul p a.simpl)
           [(c, [])]
           l
         |> P.add accum)
        tl in
  simpl_aux []

type ('a, 'b, 'c) hoas =
    Abs of ('a -> 'b)
  | Ret of 'c

type cost = Cost of P.t * (P.t list, cost, unit) hoas

type size = Size of (P.t list, size, P.t list) hoas

type A.repr += Indet of int * int

let indet var idx =
  let rec self : A.t = {
    simpl = [(C.one, [self])];
    exhib = Indet (var, idx);
    equal = function
      { exhib = Indet (v, i); _ } -> var = v && idx = i
    | _ -> false } in
  self

type A.repr += Bound

let bound vec idx : A.t = {
  simpl = simpl @@ List.nth vec idx;
  exhib = Bound;
  equal = Fun.const false }

let apply
    (Cost (fun_n, Abs fun_c), Size (Abs fun_s))
    (Cost (arg_n, Ret ())   , Size (Ret arg_s)) =
  let Cost (n, f) = fun_c arg_s in
  Cost (P.add (P.add fun_n arg_n) n, f),
  fun_s arg_s
[@@warning "-8"]

let saturate cs indims =
  let rec saturate_aux var par = function
      [] ->
      par
    | hd::tl ->
      saturate_aux
        (var + 1)
        (apply par
           (Cost (P.zero, Ret ()),
            Size (Ret (List.init hd (fun i -> [(C.one, [indet var i])])))))
        tl in
  let Cost (n, _), Size (Ret s) = saturate_aux 0 cs indims in
  simpl n, List.map simpl s
[@@warning "-8"]

let poly_to_string coef_to_string atom_to_string poly =
  let mono_to_string (c, l) =
    coef_to_string c::List.map atom_to_string l |> String.concat "*" in
  List.map mono_to_string poly |> String.concat " + "
