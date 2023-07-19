open Syntax.Term
open Tuple

(** [ge_zero_stm n] is a list of Z3 expressions stating that
the undeterminate coefficients from [0] to [n - 1]
are greater-than-or-equal zero.

@raise Invalid_argument if [n < 0].
*)
val ge_zero_stms : int -> Z3.Expr.expr list Monad.Reader(Z3env).t

(** [rules_stms func_int valuation trs] is the list of Z3 statements
interpreting the compability conditions for the TRS.*)
val rules_stms : (func -> cost * size) ->
    (var -> cost * size) ->
    (term * term) list ->
    Z3env.t ->
    Z3.Expr.expr list
