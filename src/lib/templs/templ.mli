open Monad
open Tuple

(** State monad for undetermined coefficients. *)
module Undet : MONAD
  with type 'a t = int -> 'a * int

(** Templates for interpretation generation. *)
type templ = { cnt : int list -> int;                    (** Overestimation of the usage of undetermined coefficients. *)
               exp : int -> (P.t list * int) list -> P.t (** Generated polynomial. *) }

(** Cost generation based on a template. *)
val cost_t : templ -> int list -> cost Undet.t

(** Size generation based on a template. *)
val size_t : templ -> int list -> int -> size Undet.t
