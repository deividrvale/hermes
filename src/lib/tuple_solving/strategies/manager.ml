(* A strategy is a function that gets a  *)
open Syntax.Term
open Syntax.Trs
open Tuple

module File = File.Onijn
module Ctr = Constraints
module Additive = Shape.Additive
module Gen = Shape.Generator
open Generics


let run_strat data =
  Progressive.progressive QUA data
