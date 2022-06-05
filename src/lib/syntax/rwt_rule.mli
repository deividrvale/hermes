(**  *)
type rule = Term.term * Term.term

val rule_check_syntax : rule -> bool
(** [rule_check_syntax rul] is whether [rul]
    is valid.
*)
