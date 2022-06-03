module type Z3ENV = sig
  type t
  val mk_env : (string * string) list -> t
  val mk_int_const : int -> t -> Z3.Expr.expr
  val mk_int_numeral : int -> t -> Z3.Expr.expr
  val mk_add : Z3.Expr.expr list -> t -> Z3.Expr.expr
  val mk_mul : Z3.Expr.expr list -> t -> Z3.Expr.expr
  val mk_ge : Z3.Expr.expr -> Z3.Expr.expr -> t -> Z3.Expr.expr
  val check_for_model : Z3.Expr.expr list -> t -> Z3.Model.model option
end

module Z3Env : Z3ENV = struct
  open Z3

  type t = { context : context;
             solver : Solver.solver }

  let mk_env cfg =
    let context = mk_context cfg in
    let solver = Solver.mk_solver context None in
    { context; solver }

  let mk_int_const n { context; _ } =
    Arithmetic.Integer.mk_const context (Symbol.mk_int context n)

  let mk_int_numeral i { context; _ } =
    Arithmetic.Integer.mk_numeral_i context i

  let mk_add ts { context; _ } =
    Arithmetic.mk_add context ts

  let mk_mul ts { context; _ } =
    Arithmetic.mk_mul context ts

  let mk_ge t1 t2 { context; _ } =
    Arithmetic.mk_ge context t1 t2

  let check_for_model assertions { solver; _ } =
    let open Solver in
    reset solver;
    ignore (check solver assertions);
    get_model solver
end

module IntVec = struct
  module Z3Reader = Monad.Reader(Z3Env)
  open Z3Env
  open Z3Reader
  open Monad.Utility(Z3Reader)
  open Syntax.Term
  open Syntax.Tuple.IntVec

  let ind_tup v d =
    let proj i = (Coef.one, Atom.ind v i::[])::[] in
    let cost = Cost ([], Ret ()) in
    let size = Size (Ret (List.init d proj)) in
    cost, size

  let dictionary_var dim =
    let rec dictionary_var_aux accum num = function
        [] ->
        accum
      | hd::tl ->
        dictionary_var_aux
          ((hd, ind_tup num (hd |> var_get_sort |> dim))::accum)
          (num + 1)
          tl in
    dictionary_var_aux [] 0 @@ var_sylst ()

  let coef_to_expr coef =
    let* addends =
      rev_mapM
        (fun (i, cs) ->
           let* coefficient = mk_int_numeral i in
           let* factors = rev_mapM mk_int_const cs in
           mk_mul (coefficient::factors))
        coef in
    mk_add addends

  let nonnegative e =
    let* zero = mk_int_numeral 0 in
    mk_ge e zero

  let poly_nonnegative =
    let mono_nonnegative (c, _) =
      let* coef = coef_to_expr c in
      nonnegative coef in
    rev_mapM mono_nonnegative

  let solve rules dict_var dict_func undet_count =
    let* bounds =
      rev_mapM
        (fun n ->
           let* c = mk_int_const n in
           nonnegative c)
        (List.init undet_count Fun.id) in
    let rec interp = function
        Sym (F f), _ ->
        dict_func
        |> List.find (fun (s, _) -> func_equal s f)
        |> snd
      | Sym (V v), _ ->
        dict_var
        |> List.find (fun (s, _) -> var_equal s v)
        |> snd
      | App (t1, t2), _ ->
        let interp1 = interp t1 in
        let interp2 = interp t2 in
        apply interp1 interp2 in
    let rec assert_rules accum = function
        [] ->
        return accum
      | (l, r)::tl ->
        let Cost (l_n, _), Size (Ret l_s) = interp l in
        let Cost (r_n, _), Size (Ret r_s) = interp r in
        let simpl_sub p1 p2 = P.sub (simpl p1) (simpl p2) in
        let* constr_cost = poly_nonnegative (simpl_sub l_n (P.add r_n P.one)) in
        let* constr_size = rev_mapM poly_nonnegative (List.rev_map2 simpl_sub l_s r_s) in
        assert_rules
          (List.rev_append
             constr_cost
             (List.fold_left (Fun.flip List.rev_append) accum constr_size))
          tl in
    let* assertions = assert_rules bounds rules in
    let* model_opt = check_for_model assertions in
    return begin
      let open Monad.Option in
      let open Monad.Utility(Monad.Option) in
      let open Z3 in
      let* model = model_opt in
      rev_mapM
        (fun decl ->
           let* interp = Model.get_const_interp model decl in
           return (Symbol.get_int (FuncDecl.get_name decl),
                   interp))
        (Model.get_const_decls model)
    end

  let retrieve_interp dim dict_coef (f, tup) =
    let rec saturate tup num = function
        [] ->
        tup
      | hd::tl ->
        saturate
          (apply tup (ind_tup num hd))
          (num + 1)
          tl in
    let Cost (f_c, _), Size (Ret f_s) =
      saturate tup 0 @@ List.map dim (typ_ins (func_get_typ f)) in
    let retrieve_mono ((_, c::_)::_, atoms) = List.assoc c dict_coef, atoms in
    let retrieve_poly p = List.map retrieve_mono (simpl p) in
    retrieve_poly f_c, List.map retrieve_poly f_s
end
