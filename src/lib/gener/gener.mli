open Syntax.Term
open Tuple
open Templ

(** Interpretation generation for a single function symbol.

[gener_f k tpl fn] generates an interpretation for the function symbol [fn] using a dimension function [k], a template [tpl].
*)
val gener_f : (sort -> int) -> templ -> func -> (cost * size) Undet.t

(** Interpretation generation for a single variable.
[gener_v k ]
*)
val gener_v : (sort -> int) -> int -> var -> cost * size

(** Assertion generation for a single rewrite rule. *)
val gener_r : (func -> cost * size) -> (var -> cost * size) -> term * term -> Z3env.t -> Z3.Expr.expr list

(** Assertion of an undetermined coefficient's lower bound. *)
val undet_ge : int -> int -> Z3env.t -> Z3.Expr.expr

(** Assertion of an undetermined coefficient's upper bound. *)
val undet_le : int -> int -> Z3env.t -> Z3.Expr.expr
