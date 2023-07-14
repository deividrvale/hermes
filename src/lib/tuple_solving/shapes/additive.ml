open Syntax.Term
open Tuple

open Affine

(* open Monad *)
module G = Generator
(* module Tm = Syntax.Term *)

(* Responsible for generating additive interpretations.
An interpretatin shape will come an interpretation function
and a list of Z3 expressions, putting constraints on the
underterminate coefficients.
*)

(*-----------------------------------------------------------------------------
   Types for Input/Output
  ---------------------------------------------------------------------------*)

type int_choice = ZeroCost | CostSize

type config = {
  state : int;
  int_key : sort -> int;
  fn_list : func list;
  choice : int_choice
}

type ('a, 'b) additive = {
  int_assoc : 'a;
  asserts : 'b;
  state : int
}

(*-----------------------------------------------------------------------------
  Generation of Interpretations
  ---------------------------------------------------------------------------*)

(* Generates interpretations with the zero function in the cost component
  and an additive interpretation in the size component *)
let gen_zero_cost_int (state : int) (int_key : sort -> int) (fn_list : func list) =
  (G.gen_zero_cost_int int_key Affine.affine fn_list) state

let gen_affine_int (state : int) (int_key : sort -> int) (fn_list : func list) =
  (G.gen_func_int int_key Affine.affine fn_list) state

(* generate an interpretation map based on a config provided *)
let int_map cfg =
  match cfg.choice with
  | ZeroCost ->
    gen_zero_cost_int cfg.state cfg.int_key cfg.fn_list
  | CostSize ->
    gen_affine_int cfg.state cfg.int_key cfg.fn_list

(*-----------------------------------------------------------------------------
  Assertion of Upper Bounds
  ---------------------------------------------------------------------------*)
(* What makes an affine interpretation additive is the assertion on the
  upper bounds of the underteminates that are multiplied by a monomial,
  they should be <= 1
*)

let filter_poly poly =
  List.filter (fun monomial -> not (Lists.is_empty (snd monomial))) poly

let filter_size x =
  List.concat (List.map filter_poly x)

let get_und_coef poly =
  (* First collect the coeffients in poly, which are again of type P.t *)
  let coefs = List.map fst poly in
  (* each coefficient when interpretations are generated are of the form
    [(1, A.t list)]
    then we apply snd to ge that A.t list
  *)
  let und_coefs' = List.map (List.map snd) coefs in
  (* To get the list of coefficients, we concatenate [und_coefs'] twice *)
  List.concat (List.concat und_coefs')

let assert_le unds =
  List.map (fun name -> Gener.undet_le name 1) unds

(* Generate the constraints over the undeterminate coefficients *)
let additive_expr (int_key : sort -> int) func_int f =
  let indims = G.get_indims int_key f in
  let tpl = Tuple.saturate (func_int f) indims in
  match tpl with
  | Cost (p, _), Size (Ret s) ->
    (* Get the und. coefficients for cost *)
      let poly_cost = filter_poly p in
      let und_coefs_c = get_und_coef poly_cost in
    (* Get the und. coefficients for size *)
      let poly_size = filter_size s in
      let und_coefs_s = get_und_coef poly_size in
    (* Generate the list of expressions for cost and size *)
    let assertions_c = assert_le und_coefs_c in
    let assertions_s = assert_le und_coefs_s in
      let open Monad.Utility(Monad.Reader (Z3env)) in
      rev_ListTransformerM (List.rev_append assertions_c assertions_s)
  | _ -> exit 0

let additive_exprs (int_key : sort -> int) func_int fns =
  let exprs = (List.map (additive_expr int_key func_int) fns) in
      let open Monad.Utility(Monad.Reader (Z3env)) in
        let open Monad.Reader(Z3env) in
          let* l = rev_ListTransformerM exprs in
            return (List.flatten l)

let additive_int (cfg : config) =
  (* first we match on the choice of int *)
  let fn_cs,state = int_map cfg in
  let int_func = G.func_int fn_cs in
  let ass_list =
    additive_exprs cfg.int_key int_func cfg.fn_list in
  {
    int_assoc = fn_cs;
    asserts = ass_list;
    state = state
  }











(* A strategy should:
   1. Generate interpretations for each function symbols and variables.
   2. Define the bounds for the polynomials underteminates.
*)

(*-----------------------------------------------------------------------------
   Generation of Interpretations
  ---------------------------------------------------------------------------*)
(*
The generation of interpretation for this strategy is as follows:
there is two main functions:
   1. [gen_func_int] which takes an [int] as input to generate affine interpretations
      (according to the affine template).
   2. [gen_var_int] which also takes an [int] as input to generate interpretations
      for variables at this int's dimention.
*)

(* let func_interpret i =
  let j, state = gen_func_int i 0 in
  let () = gen_state_value := state in
  Lists.map_from_assoc_list func_equal j *)

(* let valuation i = Lists.map_from_assoc_list var_equal (gen_var_int i) *)

(* let get_input_dim (ty_count : sort -> int) f =
  List.map ty_count (typ_ins (func_get_typ f)) *)

(* let f_nempty_list = function [] -> false | _ :: _ -> true *)

(* let am_fucked dim f =
  let _ = func_interpret dim f in
  let names_list = List.init !gen_state_value Fun.id in
  let fucked_test_3 = List.map (fun name -> Gener.undet_ge name 0) names_list in
  let open Monad.Reader (Z3env) in
  let rec fuck accum = function
    | [] -> return accum
    | hd :: tl ->
        let* hd = hd in
        fuck (hd :: accum) tl
  in
  fuck [] fucked_test_3 *)

(* let am_fucked_2 dim f =
  let in_dim = get_input_dim (Fun.const dim) in
  let jf = Tuple.saturate (func_interpret dim f) (in_dim f) in
  match jf with
  | Cost (p, _), Size (Ret s) ->
      let fucked_test_0 = List.filter (fun l -> f_nempty_list (snd l)) p in
      let fucked_test_1 = List.map fst fucked_test_0 in
      let fucked_test_2 = List.map (List.map snd) fucked_test_1 in
      let concat_twice_fucked_0 = List.concat (List.concat fucked_test_2) in
      let fucked_test_3 =
        List.map (fun name -> Gener.undet_le name 1) concat_twice_fucked_0
      in
      let open Monad.Reader (Z3env) in
      let rec fuck accum = function
        | [] -> return accum
        | hd :: tl ->
            let* hd = hd in
            fuck (hd :: accum) tl
      in
      fuck [] fucked_test_3
  | _ -> exit 0 *)

(* let additive (loop_idx : int) (* loop index *) (k : sort -> int)
     (config : hermes_config) (trs : trs) =
   () *)

(* z3_test with only am_fucked *)
(* let z3_test dim fn =
  let z3 = am_fucked dim fn in
  let env = Z3env.mk_env [] in
  (* the model *)
  let m =
    (let open Monad.Reader (Z3env) in
     let* z3 = z3 in
     Z3env.check_for_model z3)
      env
  in
  let open Monad.Option in
  let* m = m in
  let* ass = Read_model.get_assigns m in
  return ass *)

(* this combines at least two lists of expressions *)
(* let z3_test_2 dim fn (s, t) =
  let z3_1 = am_fucked dim fn and z3_2 = am_fucked_2 dim fn in
  let rules_expr = Gener.gener_r (func_interpret dim) (valuation dim) (s, t) in
  let env = Z3env.mk_env [] in
  (* the model *)
  let m =
    (let open Monad.Reader (Z3env) in
     (* let* z3_1 = z3_1... can be written as just let* z3_1 (ocaml syntax bullshit) *)
     let* z3_1 = z3_1 in
     let* z3_2 = z3_2 in
     let* rules_expr = rules_expr in
     let z3 = List.rev_append (List.rev_append z3_1 z3_2) rules_expr in
     Z3env.check_for_model z3)
      env
  in
  let open Monad.Option in
  let* m = m in
  let* ass = Read_model.get_assigns m in
  return ass *)
