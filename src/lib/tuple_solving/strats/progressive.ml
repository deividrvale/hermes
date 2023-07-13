(*
  The progressive strategy strategy is characterized by the
  the choice of the dimension choice.
  We start from 1 ... K (the upper bound on the type_count)
  and increase the dimension by one at each iteration.
  Each iteration we generate interpretation shapes
  and try to solve for the rule orientations.
*)

module F = File.Onijn
open Syntax.Term
open Syntax.Trs
module Additive = Shape.Additive
module Gen = Shape.Generator

(* a function that splits the signature into two parts *)
let split_sig (rewrite_system : F.trs_data) =
  ctr_symb rewrite_system.trs, def_symb rewrite_system.trs


(* Set the config type *)
let set_config state int_key fn_list choice =
  let open Shape.Additive in
  let int_key = Fun.const int_key in
  {
    state = state;
    int_key = int_key;
    fn_list = fn_list;
    choice = choice
  }

let progressive (trs : F.trs_data) =
  (* Get General Hermes Config *)
  let hermes_cfg = Config.get_config () in
  let max_ty_dim = hermes_cfg.tuples.type_dim in

  (* Interpret Constructors *)
  let ctrs_config = set_config 0 2 (split_sig trs |> fst) Additive.ZeroCost in

  let ctrs = Additive.additive_int ctrs_config in

  (* Interpret Defined Symbols *)
  let def_config = set_config (ctrs.state) 2 (split_sig trs |> snd) Additive.CostSize in
  let def = Additive.additive_int def_config in

  (* Combine the Intepretations to form the j_map and valuation function *)
  let j_map = Shape.Generator.func_int (List.rev_append ctrs.int_assoc def.int_assoc) in
  let valuation = Shape.Generator.gen_var_int ctrs_config.int_key in

  let gen_rules_expr j_map valuation rules =
    let exprs = List.map (Gener.gener_r j_map valuation) rules in
      let open Monad.Utility(Monad.Reader (Z3env)) in
      let open Monad.Reader(Z3env) in
        let* l = rev_ListTransformerM exprs in
        return (List.flatten l)
  in
  (* Combine the assertion expressions:
     1. from the constructors' shape
     2. from the def. symbols' shape
     3. saying *)
  let z3_env = Z3env.mk_env [] in
  let ctrs_asserts = ctrs.asserts in
  let def_asserts = def.asserts in
  let get_asserts = Gen.ge_stm def.state in
  let rules_expressions = gen_rules_expr j_map valuation trs.trs in
  let combined_list =
    let open Monad.Reader(Z3env) in
      let* ctrs_asserts in
      let* def_asserts in
      let* get_asserts in
      let* rules_expressions in
      return (ctrs_asserts @ def_asserts @ get_asserts @ rules_expressions)
      (* return (rules_expressions) *)
  in
  let model =
    (let open Monad.Reader(Z3env) in
      let* combined_list in
        Z3env.check_for_model combined_list) z3_env
  in
  model
  (* combined_list z3_env *)










