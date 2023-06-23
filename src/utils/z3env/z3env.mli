open Z3

type t

val mk_env : (string * string) list -> t

val mk_int_const : int -> t -> Expr.expr

val mk_int_numeral : int -> t -> Expr.expr

val mk_add : Expr.expr list -> t -> Expr.expr

val mk_mul : Expr.expr list -> t -> Expr.expr

val mk_ge : Expr.expr -> Expr.expr -> t -> Expr.expr

val check_for_model : Expr.expr list -> t -> Model.model option
