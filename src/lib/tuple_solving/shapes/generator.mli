open Syntax.Term
open Tuple

(** Generation of generic interpretation shapes.

*)

(**  *)
val func_shapes : (sort -> int) ->
    Templ.templ ->
    func list ->
    (func * (cost * size)) list Templ.Undet.t

(* (** [zero_cost f] is a cost tuple that is zero everywhere.
  It is generated based on the type information of [f].
*)
val zero_cost : func -> cost *)

(** *)
val func_shapes_zero_cost : (sort -> int) ->
    Templ.templ ->
    func list ->
    (func * (cost * size)) list Templ.Undet.t

val func_int : (func * 'a) list -> func -> 'a

(** [var_int int_key x] is the cost-size tuple interpreting the variable x.

{b Note:} the expression [var_int int_key : var -> cost * size] is equivalent
to a {i valuation}, i.e., a map that associates each variable x to a cost-size
tuple in the interpretation domain.
It interprets all variables that was symbolized in the parsing process.
*)
val var_int : (sort -> int) -> var -> cost * size

(** [get_indims int_key f] is the list of integers where each element is the
{b dimension} of an argument of [f] according to the interpretation key [int_key].
*)
val get_indims : (sort -> int) -> func -> int list

(** [get_outdim int_key f] is the integer representing the dimention of the
output type of [f] according to the interpretation key [int_key].
*)
val get_outdim : (sort -> int) -> func -> int

val indet_to_string : A.t -> string

val sat_shape_to_string : (C.t -> string) -> (A.t -> string) -> P.t * P.t list -> string

val print_sat_shape : P.t * P.t list -> unit
