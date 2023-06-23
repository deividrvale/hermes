open Syntax.Term
open Syntax.Trs
open Config

(* A strategy should essentially:
   1. choose an interpretation to the f
   2. define the bounds for the polynomials
   underteminates *)
(* we know what type of interpretation
   we want, they must be affine... *)

(* set the interpretation of each function *)
let gen_func_int i =
  let k = Fun.const i and
      tpl = Affine.affine and
      funcs = func_sylst () in
      let j = fun fn -> Gener.gener_f k tpl fn in
      List.combine funcs (List.map j funcs)

let gen_var_int i =
   let k = Fun.const i and vars = var_sylst () in
   List.combine vars (List.mapi (Gener.gener_v k) vars)

let additive (loop_idx : int) (* loop index *)
             (k : sort -> int)
             (config : hermes_config)
             (trs : trs)
             =
             ()
