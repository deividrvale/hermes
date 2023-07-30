open Syntax.Term

type fakeTy = Name of string | Arr of fakeTy * fakeTy

type signature = (string * fakeTy) list

type environment = (string * fakeTy) list

type trs = ((string, unit) term_tree * (string, unit) term_tree) list

type parsed_file = {
  ar : signature;
  var : environment;
  rules : trs
}

type trs_data = {
  trs : (term * term) list
}

val parsed_file_mk : signature -> environment -> trs -> parsed_file

val process_file : parsed_file -> trs_data
