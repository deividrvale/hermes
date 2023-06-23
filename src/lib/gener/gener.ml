open Monad
open Z3env
open Syntax.Term
open Tuple
open Templ

let gener_f dim t sy =
  let ty = func_get_typ sy in
  let indims = List.map dim (typ_ins ty) in
  let outdim = dim (typ_out ty) in
  let open Undet in
  let* c = cost_t t indims in
  let* s = size_t t indims outdim in
  return (c, s)

let gener_v dim v sy =
  let c = Cost (P.zero, Ret ()) in
  let s = Size (Ret (List.init
                       (sy |> var_get_sort |> dim)
                       (fun i -> [(C.one, [indet v i])]))) in
  c, s

let gener_r interp_f interp_v (lhs, rhs) =
  let rec interp = function
      Sym (F sy), _ ->
      interp_f sy
    | Sym (V sy), _ ->
      interp_v sy
    | App (t1, t2), _ ->
      apply (interp t1) (interp t2) in
  let Cost (l_n, _), Size (Ret l_s) = interp lhs in
  let Cost (r_n, _), Size (Ret r_s) = interp rhs in
  let simpl_sub p1 p2 = P.add (simpl p1) (P.neg (simpl p2)) in
  let module R = Reader(Z3env) in
  let open R in
  let open Utility(R) in
  let coef_nonneg p =
    let* adds =
      rev_mapM
        (fun (n, cs) ->
           let* coef = mk_int_numeral n in
           let* fcts = rev_mapM mk_int_const cs in
           mk_mul (coef::fcts))
        p in
    let* e = mk_add adds in
    let* zero = mk_int_numeral 0 in
    mk_ge e zero in
  let poly_nonneg = rev_mapM (fun (c, _) -> coef_nonneg c) in
  let* assert_c = poly_nonneg (simpl_sub l_n (P.add r_n P.one)) in
  let* assert_s = rev_mapM poly_nonneg (List.rev_map2 simpl_sub l_s r_s) in
  return @@ List.rev_append assert_c @@ List.concat assert_s
[@@warning "-8"]

let undet_ge c n =
  let open Reader(Z3env) in
  let* c = mk_int_const c in
  let* n = mk_int_numeral n in
  mk_ge c n

let undet_le c n =
  let open Reader(Z3env) in
  let* c = mk_int_const c in
  let* n = mk_int_numeral n in
  mk_ge n c
