open Syntax.Term
open Strat.Generics
module Gen = Shape.Generator

let parse_file file =
  Hermes_parser.parse_from_string Hermes_parser.parser Hermes_parser.lexer file

let get_data file = parse_file file |> File.Onijn.process_file

let coef_value_to_string value_map = function
  | [ (1, [ name ]) ] -> value_map name
  | _ -> failwith "Error! Trying to print coefficient of wrong shape."

let tuple_prove (data : strategy_result option) =
  match data with
  | None ->
      print_endline
      "No tuple interpretation over the natural numbers could be found.";
      exit 1
  | Some r ->
      let () =
        print_endline
        "The interpretations below are written in reduced form in which we only show the body of each tuple.\nNotice that for each argument i of a function symbol, its corresponding variable name is written as Xi_j in which j represents the component of the variable Xi.\nAn example: for a function symbol f : a => b, we will write [f] := < cost || size >.\nThe variables appearing in the body of the interpretation depends on the dimension given to the type a. They are written from 0 to n.\nFor instance, in a type of dimension 2, we will have variable names X0_0 and X0_1 appearing in the body.\nAnalogously, the dimension found for the type b determines how many components the size part of the interpretation have.\nCost components that are equivalent to the constant function that outputs only 0 values, so a zero-cost tuple, are written as simply as (). This only happens to cost components.";
        print_newline ();
      in
      let coef_value = Lists.map_from_assoc_list Int.equal r.model in
      let value_map i = coef_value i |> Z3.Expr.to_string in
      let j_map = r.func_int in
      List.iter
        (fun f ->
          print_string ("[" ^ func_to_string f ^ "] := ");
          (* print_newline (); *)
          print_endline
            (Gen.sat_shape_to_string
               (coef_value_to_string value_map)
               Gen.indet_to_string
               (Tuple.saturate (j_map f) (Gen.get_indims r.int_key f)));
        )
        (func_sylst ());
        print_newline ();
        print_endline "By the Compatibility Theorem, we should have that the interpretations should satisfy [[l]] > [[r]] and [[l]] >= [[r]], for all rules in l => r in the input TRS.";
        print_endline "The theory implemented in this current version in Hermes can be read in the paper https://doi.org/10.4204/EPTCS.376.5"
