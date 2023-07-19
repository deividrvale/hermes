open Syntax.Term
open Tuple
module Gen = Generator
include Generics

(* Responsible for generating additive interpretations.
An interpretatin shape will come an interpretation function
and a list of Z3 expressions, putting constraints on the
underterminate coefficients.
*)

(*-----------------------------------------------------------------------------
   Types for Input/Output
  ---------------------------------------------------------------------------*)

(* type int_choice = ZeroCost | CostSize

type config = {
  state : int;
  int_key : sort -> int;
  fn_list : func list;
  choice : int_choice
} *)

(*-----------------------------------------------------------------------------
  Generation of Interpretations
  ---------------------------------------------------------------------------*)

(* Generates interpretations with the zero function in the cost component
  and an additive interpretation in the size component *)
let gen_zero_cost_int (state : int) (int_key : sort -> int) (fn_list : func list) =
  (Gen.func_shapes_zero_cost int_key Affine.affine fn_list) state

let gen_affine_int (state : int) (int_key : sort -> int) (fn_list : func list) =
  (Gen.func_shapes int_key Affine.affine fn_list) state

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
  let indims = Gen.get_indims int_key f in
  let tpl = Tuple.saturate (func_int f) indims in
  match tpl with
  | (p, s) ->
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

let additive_exprs (int_key : sort -> int) func_int fns =
  let exprs = (List.map (additive_expr int_key func_int) fns) in
      let open Monad.Utility(Monad.Reader (Z3env)) in
        let open Monad.Reader(Z3env) in
          let* l = rev_ListTransformerM exprs in
            return (List.flatten l)

let additive_int (cfg : shape_config) : shape_int =
  (* first we match on the choice of int *)
  let fn_cs,state = int_map cfg in
  let int_func = Gen.func_int fn_cs in
  let ass_list =
    additive_exprs cfg.int_key int_func cfg.fn_list in
  {
    int_assoc = fn_cs;
    asserts = ass_list;
    state = state
  }
