open Syntax.Term
open Tuple

type int_choice = ZeroCost | CostSize

type shape_config = {
  state : int;
  int_key : sort -> int;
  fn_list : func list;
  choice : int_choice
}

type shape_int = {
  int_assoc : (func * (cost * size)) list;
  asserts: Z3env.t -> Z3.Expr.expr list;
  state : int
}
