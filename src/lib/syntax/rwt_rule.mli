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

open Format

val pp_print_rule : formatter -> rule -> unit

val print_rule : rule -> unit
