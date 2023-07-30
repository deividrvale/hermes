module Gen = Generator
include Generics

let gen_quadratic state int_key fn_list =
  (Gen.func_shapes int_key Quadratic.quadratic fn_list) state

let quadratic_int (cfg : shape_config) : shape_int =
  let fn_cs, state =
    gen_quadratic cfg.state cfg.int_key cfg.fn_list
  and
  ass_list = let open Monad.Reader(Z3env) in
    return []
  in
  {
    int_assoc = fn_cs;
    asserts = ass_list;
    state = state
  }
