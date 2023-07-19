open Syntax.Term
open Tuple

(*
config ->
( (func * (cost * size)) list,
  Z3env.t -> Z3.Expr.expr list )
additive
*)

type strategy_result = {
  int_key : sort -> int;
  func_int : func -> cost * size;
  model : (int * Z3.Expr.expr) list
}

