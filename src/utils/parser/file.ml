open Syntax.Term
(*

*)

type parsed_file = {
  ar : (string * typ) list;
  var : (string * typ) list;
  rules : ( (string, unit) term_tree * (string, unit) term_tree ) list
}

let parsed_file_mk ar var rules = {
    ar = ar;
    var = var;
    rules = rules
}
