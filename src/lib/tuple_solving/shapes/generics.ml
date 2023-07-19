open Syntax.Term
open Tuple

type shape_int = {
  int_assoc : (func * (cost * size)) list;
  asserts: Z3env.t -> Z3.Expr.expr list;
  state : int
}
