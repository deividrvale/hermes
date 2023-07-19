open Syntax.Term
open Tuple
open Affine
open Generics

type int_choice =
  Generics.int_choice = ZeroCost | CostSize

(** Responsible for generating additive interpretations.
An interpretatin shape will come an interpretation function
and a list of Z3 expressions, putting constraints on the
underterminate coefficients.
*)

(*-----------------------------------------------------------------------------
   Types for Input/Output
  ---------------------------------------------------------------------------*)

(* * the choice   *)
(* type int_choice =
  | ZeroCost (** teste *)
  | CostSize * teste  *)

(** teste *)
(* type config = {
    state : int; (** the state *)
    int_key : sort -> int; (** int_key *)
    fn_list : func list; (** teste *)
    choice : int_choice (** teste *)
  } *)
  (** teset gai *)

val additive_int : shape_config -> shape_int
