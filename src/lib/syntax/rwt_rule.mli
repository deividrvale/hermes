(**  *)
type rule = Term.term * Term.term

val rule_check : rule -> bool
(** [rule_check_syntax rul] is whether [rul]
    is valid.
*)

val rule_check_lhs : rule -> bool
(** [rule_check_lhs rul] is whether [rul]
    is valid.
*)

val rule_check_rhs : rule -> bool
(** [rule_check_rhs rul] is whether [rul]
    is valid.
*)
