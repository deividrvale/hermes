open Syntax.Term
open Tuple

type shape = ADD | LIN | QUA

type strategy_config = {
  shape : shape;
}

type strategy_result = {
  int_key : sort -> int;
  func_int : func -> cost * size;
  model : (int * Z3.Expr.expr) list
}
