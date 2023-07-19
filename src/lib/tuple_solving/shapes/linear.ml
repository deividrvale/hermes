open Syntax.Term
open Tuple
module Gen = Generator
include Generics

(* Generation of Affine Interpretations.
   Those do not have any upper-bound restrictions on the coefficients. *)
let gen_affine_int state int_key fn_list =
  (Gen.func_shapes int_key Affine.affine fn_list) state

let linear_int (cfg : shape_config) : shape_int =
  let fn_cs, state =
    gen_affine_int cfg.state cfg.int_key cfg.fn_list
  and
  ass_list = let open Monad.Reader(Z3env) in
    return []
  in
  {
    int_assoc = fn_cs;
    asserts = ass_list;
    state = state
  }
