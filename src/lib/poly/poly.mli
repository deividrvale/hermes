(** Domains. *)
module type DOMAIN = sig
  type t
  val zero : t
  val one : t
  val neg : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
end

(** Atoms include indeterminates and possibly other atomic terms. *)
module type ATOM = sig
  type t
  val equal : t -> t -> bool
end

(** Polynomials carry over operations from the given domain. *)
module Polynomial (C : DOMAIN) (A : ATOM) : DOMAIN
  with type t = (C.t * A.t list) list
