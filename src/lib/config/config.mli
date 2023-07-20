exception Invalid_config of string

type input_format = NONE | ONIJN
type strategy = NONE | BLD | PRG | PTN
type termination_technique = NONE | TUPLE
type smt_solver = NONE | Z3
type answer = YES | MAYBE

type search_space_bounds = {
  mutable type_dim : int;
  mutable param_int : int * int;
  mutable poly_deg : int;
}

type hermes_config = {
  mutable format : input_format;
  mutable strategy : strategy;
  mutable technique : termination_technique;
  mutable solver : smt_solver;
  mutable tuples : search_space_bounds;
}

val get_config : unit -> hermes_config
