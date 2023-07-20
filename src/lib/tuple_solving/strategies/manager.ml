open Generics
module Ctr = Constraints
module Additive = Shape.Additive
module Gen = Shape.Generator
module File = File.Onijn
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
