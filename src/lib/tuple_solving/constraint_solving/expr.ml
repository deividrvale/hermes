open Syntax.Term
open Tuple

let ge_zero_stms (state : int) =
  let names = List.init state Fun.id in
  let stms = List.map (fun name -> Gener.undet_ge name 0) names in
  let open Monad.Utility(Monad.Reader(Z3env)) in
  rev_ListTransformerM stms

let rules_stms j_map valuation rules =
  let exprs = List.map (Gener.gener_r j_map valuation) rules in
    let open Monad.Utility(Monad.Reader (Z3env)) in
    let open Monad.Reader(Z3env) in
      let* l = rev_ListTransformerM exprs in
      return (List.flatten l)
