open Z3

val get_assigns : Model.model -> (int * Expr.expr) list option

val get_model : (Z3env.t -> Z3.Expr.expr list) -> Model.model option

val check_model_existence : Model.model option -> (int * Z3.Expr.expr) list option

val z3_env : Z3env.t
