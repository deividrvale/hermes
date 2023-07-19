(**  *)
type rule = Term.term * Term.term

val rule_check : rule -> unit
(** [rule_check_syntax rul] is whether [rul]
    is valid.
*)

val rule_check_lhs : rule -> unit
(** [rule_check_lhs rul] is whether [rul]
    is valid.
*)

val rule_check_rhs : rule -> unit
(** [rule_check_rhs rul] is whether [rul]
    is valid.
*)

open Term

type trs = rule list

(* val get_rule_head : rule -> func *)

val def_symb : trs -> func list

val ctr_symb : trs -> func list

open Format

val pp_print_rule : formatter -> rule -> unit

val print_rule : rule -> unit

val split_sig : trs -> func list * func list
