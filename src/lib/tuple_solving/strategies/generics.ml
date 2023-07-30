open Syntax.Term
open Tuple

type shape = ADD | AFF | QUA

let shape_to_string = function
  | ADD -> "ADDITIVE"
  | AFF -> "AFFINE"
  | QUA -> "QUADRATIC"

type strategy_result = {
  int_key : sort -> int;
  func_int : func -> cost * size;
  model : (int * Z3.Expr.expr) list
}
