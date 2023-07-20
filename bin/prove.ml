open Config
open Syntax.Term
open Strat.Generics
module Gen = Shape.Generator


let parse_file file =
  Hermes_parser.parse_from_string
  Hermes_parser.parser
  Hermes_parser.lexer
  file

let get_data file =
  parse_file file |> File.Onijn.process_file


let print_pairs (i, exp) =
  "\n" ^ (Int.to_string i) ^ " := " ^
  Z3.Expr.to_string exp

let coef_value_to_string value_map = function
  | [(1, [name])] -> value_map name
  | _ -> failwith "Error! Trying to print coefficient of wrong shape."


let tuple_prove (data : strategy_result option) =
  match data with
  | None -> print_endline "no solution"
  | Some r ->
    let coef_value =
      Lists.map_from_assoc_list Int.equal r.model in
    let value_map i =
      coef_value i |> Z3.Expr.to_string in
    let j_map = r.func_int in
      List.iter
      (fun f ->
        print_endline (func_to_string f);
        print_endline (
          Gen.sat_shape_to_string
            (coef_value_to_string value_map)
            Gen.indet_to_string
            (Tuple.saturate (j_map f)
            (Gen.get_indims r.int_key f))
        );
        print_endline "\n"
      ) (func_sylst ())
