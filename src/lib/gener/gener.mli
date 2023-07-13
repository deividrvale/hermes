open Syntax.Term
open Tuple
open Templ

(** Interpretation generation for a single function symbol.

[gener_f k tpl fn] generates an interpretation for the function symbol [fn] using a dimension function [k] and a template [tpl].

The dimension function [k : sort -> int] is the function that
sets the length of the tuples for each sort.
In the theory, we call this the {b interpretation key} function
of the tuple interpretation.
*)
val gener_f : (sort -> int) -> templ -> func -> (cost * size) Undet.t

(** Interpretation generation for a single variable. *)
val gener_v : (sort -> int) -> int -> var -> cost * size

val gener_r : (func -> cost * size) -> (var -> cost * size) -> term * term -> Z3env.t -> Z3.Expr.expr list
(** Assertion generation for a single rewrite rule. *)


val undet_ge : int -> int -> Z3env.t -> Z3.Expr.expr
(**
    [undet_ge n m env] is the Z3 expression stating
    that the undetermined coefficient [n] is greater
    than equal [m] in the Z3 environment [env].
*)

(** Assertion of an undetermined coefficient's upper bound. *)
val undet_le : int -> int -> Z3env.t -> Z3.Expr.expr
