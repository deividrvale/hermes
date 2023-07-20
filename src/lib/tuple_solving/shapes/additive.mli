open Generics

(** Responsible for generating additive interpretations.
An interpretatin shape will come an interpretation function
and a list of Z3 expressions, putting constraints on the
underterminate coefficients.
*)

type int_choice = Generics.int_choice = ZeroCost | CostSize

val additive_int : shape_config -> shape_int
(** [additive_int cfg] returns the additive interpretation shape according
to the configuration [cfg].

Check {Generics.shape_config} for more details on how to configure the
additive generation.
*)
