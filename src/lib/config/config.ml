exception Invalid_config of string

(* Supported input file formats *)
type input_format =
  | NONE
  | ONIJN

let input_format_eq f f' =
  match (f,f') with
  | (NONE, NONE)   -> true
  | (ONIJN, ONIJN) -> true
  | (_,_)          -> false

let format_to_string = function
  | NONE -> "None"
  | ONIJN -> "ONijn"


type strategy = NONE | BLD | PRG | PTN

let str_eq str1 str2 =
  match (str1, str2) with
  | (BLD, BLD) -> true | (PRG, PRG) -> true
  | (PTN, PTN) -> true | _ -> false

let str_to_string = function
  | NONE -> "None" | BLD -> "Blind"
  | PRG -> "Progressive" | PTN -> "Pattern"

type termination_technique =
  | NONE
  | TUPLE

let tt_to_string = function
  | NONE -> "None"
  | TUPLE -> "Tuple"

let tt_eq x y =
  match (x,y) with
  | (NONE, NONE) -> true
  | (TUPLE, TUPLE) -> true
  | (_, _) -> false

type smt_solver = NONE | Z3

type answer = YES | MAYBE

(* configuration parameters for the run *)
type search_space_bounds = {
  mutable type_dim : int;
  mutable param_int : int * int;
  mutable poly_deg : int
}

type hermes_config = {
  mutable format : input_format;
  mutable strategy : strategy;
  mutable technique : termination_technique;
  mutable solver : smt_solver;
  mutable tuples : search_space_bounds
}

let config = {
  format = ONIJN;
  strategy = PRG;
  technique = TUPLE;
  solver = Z3;
  tuples = {
    type_dim = 3;
    param_int = (0,2);
    poly_deg = 2
  }
}

let get_initial_config () = config
