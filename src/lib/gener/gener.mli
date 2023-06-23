open Syntax.Term
open Tuple
open Templ

(** Interpretation generation for a single function symbol. *)
val gener_f : (sort -> int) -> templ -> func -> (cost * size) Undet.t

(** Interpretation generation for a single variable. *)
val gener_v : (sort -> int) -> int -> var -> cost * size

(** Assertion generation for a single rewrite rule. *)
val gener_r : (func -> cost * size) -> (var -> cost * size) -> term * term -> Z3env.t -> Z3.Expr.expr list

(** Assertion of an undetermined coefficient's nonnegativity. *)
val undet_nonneg : int -> Z3env.t -> Z3.Expr.expr
