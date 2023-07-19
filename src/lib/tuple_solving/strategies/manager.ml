(* A strategy is a function that gets a  *)
open Syntax.Term
open Syntax.Trs
open Tuple

module File = File.Onijn
module Ctr = Constraints
module Additive = Shape.Additive
module Gen = Shape.Generator

type strat_result = {
  int_key : sort -> int;
  func_int : func -> cost * size;
  model : Z3.Model.model option
}
