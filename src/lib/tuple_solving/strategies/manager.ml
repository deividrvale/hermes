(* A strategy is a function that gets a  *)
open Syntax.Term
open Syntax.Trs
open Tuple

module File = File.Onijn
module Ctr = Constraints
module Additive = Shape.Additive
module Gen = Shape.Generator
open Generics
open Progressive

let run_strat data =
  (* Try Progressive strategy using all the 3 shapes available *)
  match progressive ADD data with
  | None ->
    begin
      match progressive AFF data with
      | None ->
        begin
          match progressive QUA data with
          | None -> None
          | Some sol -> Some sol
        end
      | Some sol -> Some sol
    end
  | Some sol -> Some sol
