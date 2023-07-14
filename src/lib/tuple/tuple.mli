open Poly

(** Polynomials of undetermined coefficients. *)
module C : DOMAIN
  with type t = (int * int list) list

(** Atoms featuring extensibility. *)
module A : sig
  type repr = ..
  type t = { simpl : (C.t * t list) list;
             exhib : repr;
             equal : t -> bool }
  val equal : t -> t -> bool
end

(** Polynomials of atoms. *)
module P : DOMAIN
  with type t = (C.t * A.t list) list

(** Simplification of polynomials. *)
val simpl : P.t -> P.t

(** Higher-order abstract syntax. *)
type ('a, 'b, 'c) hoas =
    Abs of ('a -> 'b) (** Function abstraction. *)
  | Ret of 'c         (** Function body. *)

(** Cost tuples. *)
type cost = Cost of P.t * (P.t list, cost, unit) hoas

(** Size tuples. *)
type size = Size of (P.t list, size, P.t list) hoas

(** *)
type A.repr += Indet of int * int


(** Indeterminates. *)
val indet : int -> int -> A.t

(** Bound variables in HOAS. *)
val bound : P.t list -> int -> A.t

(** Semantic application. *)
val apply : cost * size -> cost * size -> cost * size

(**
[saturate cs dim] returns the cost-size [cs] in fully applied form, so the user can inspect its body.
The argument [dim] is a list of integers collecting
the dimension of each argument.

For instance, consider a generic expression
[cs = (λxyz.P,λxyz.Q)] with dimentions [[2,3,5]].
[saturate ]
<continue the example later>

{b Note:} the caller is responsible of providing the correct
list [dim].

*)
val saturate : cost * size -> int list -> cost * size

(** The string representation of a polynomial. *)
val poly_to_string : (C.t -> string) -> (A.t -> string) -> P.t -> string
