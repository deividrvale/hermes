(*
  The progressive strategy strategy is characterized by the
  the choice of the dimension choice.
  We start from 1 ... K (the upper bound on the type_count)
  and increase the dimension by one at each iteration.
  Each iteration we generate interpretation shapes
  and try to solve for the rule orientations.
*)

open Syntax.Term
open Syntax.Trs
open Tuple

module File = File.Onijn
module Ctr = Constraints
module Additive = Shape.Additive
module Linear = Shape.Linear
module Qua = Shape.Qua
module Gen = Shape.Generator
open Generics

(* Set the config type *)
let set_config state int_key fn_list choice =
  let open Additive in
  let open Shape.Generics in
  let int_key = Fun.const int_key in
  {
    state = state;
    int_key = int_key;
    fn_list = fn_list;
    choice = choice
  }

let select_shape = function
  | ADD -> Additive.additive_int
  | AFF -> Linear.linear_int
  | QUA -> Qua.quadratic_int

(* One step of the progressive strategy *)
let progressive_one_step shape (data : File.trs_data) int_key =

  (* Select the shape functions to interpret function symbols *)
  let interpret = select_shape shape in
  (* <debug> *)
  let () =
    print_endline ("Progressive Strategy on dimension " ^ Int.to_string int_key);
    print_endline ("Interpretation shape choosen: " ^ shape_to_string shape)
  in

  let splitted_sig = split_sig data.trs in
  (* Interpretation phase -------------------------------------------------- *)
  (* Interpret Constructors.
    Constructors will be additive.
    So we don't respect the choice given as argument for them. *)
  let ctrs_config =
    set_config 0 int_key (splitted_sig |> fst) Additive.ZeroCost in
  let ctrs = Additive.additive_int ctrs_config in

  (* Interpret Defined Symbols *)
  (* Notice that we set the state of the generation step for
    defined symbols as ctrs.state in order to continue the generation
    of names *)
  let def_config =
    set_config ctrs.state int_key (splitted_sig |> snd) Additive.CostSize in
  let def = interpret def_config in

  (* Combination phase ----------------------------------------------------- *)
  (* Combine the results from interpreting constructors
     and defined symbols to form the j_map and valuation function.
     Now those interpret the full signature. *)
  let j_map =
    Gen.func_int (List.rev_append ctrs.int_assoc def.int_assoc) in
  let valuation =
    Gen.var_int ctrs_config.int_key in

  (* <debug> print the interpretation shapes found *)
  (* let () =
    print_endline "Interpretation Shapes Produced -----";
    List.iter
    ( fun f ->
      print_endline (func_to_string f);
        Gen.print_sat_shape
          (Tuple.saturate (j_map f) (Gen.get_indims def_config.int_key f))
    ) (func_sylst ());
    print_endline "----------\n"
  in *)

  (* Assertions collection phase ------------------------------------------- *)
  (* The first assertions set all undeterminate coefficients of the
    interpretations as non-negative. *)
  let nn_asserts =
    Ctr.Expr.ge_zero_stms def.state in
  (* Now we collect the assertions comming from the interpretation shape. *)
  let ctrs_asserts = ctrs.asserts in
  let def_asserts = def.asserts in
  (* The assertions comming from the compatibility conditions over the
    set of rules. *)
  let rules_expressions =
    Ctr.Expr.rules_stms j_map valuation data.trs in
  (* Finally, we combine all of those assertions in a single list. *)
  let combined_list =
    let open Monad.Reader(Z3env) in
      let* nn_asserts in
      let* ctrs_asserts in
      let* def_asserts in
      let* rules_expressions in
      return (ctrs_asserts @ def_asserts @ nn_asserts @ rules_expressions)
  in
  (* Checking for solutions phase ------------------------------------------ *)
  let model = Ctr.Solver.get_model combined_list in
  let run_result = Ctr.Solver.check_model_existence model in
  match run_result with
  | None -> None
  | Some l ->
    Some {
      int_key = ctrs_config.int_key;
      func_int = j_map;
      model = l
    }

let progressive shape data =
  (* Get General Hermes Config *)
  let hermes_cfg = Config.get_config () in
  let max_ty_dim = hermes_cfg.tuples.type_dim in
  let rec progressive_aux run_index =
    if run_index <= max_ty_dim then
      begin
        match progressive_one_step shape data run_index with
        | None ->
          let () =
            print_endline ("\n failed on :" ^ Int.to_string run_index)
          in
          progressive_aux (run_index + 1)
        | Some sol ->
          let () =
            print_endline ("sucessive step: " ^ Int.to_string run_index)
          in
          Some sol
      end
    else
      begin
        None
      end
  in
  progressive_aux 1

