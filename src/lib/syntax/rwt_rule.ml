open Term

(**
  [is_sublist eq xs ys] is whether [xs] is a sublist of [ys],
  with [eq] used as equality operator.
*)
let rec is_sublist eq (xs : 'a list) (ys : 'a list) : bool =
  match (xs, ys) with
  | ([],[]) -> true
  | (_, []) -> false
  | ([], _) -> true
  | (x :: xs, y :: ys) ->
    if (eq x y) then
      is_sublist eq xs ys
    else is_sublist eq (x :: xs) ys


type rule = term * term



(*
  a lhs is syntactically valid
  if it is headed by a function symbol
  and it is of base type
*)
let rec rule_check_lhs (((l, ty), _) : rule) : bool =
  match l with
  | Sym (F _) -> typ_is_sort ty
  | Sym (V _) -> false
  | l -> test_head_symbol l && typ_is_sort ty
and test_head_symbol = function
    Sym (V _) -> false
  | Sym (F _) -> true
  | App ((l, _), _) -> test_head_symbol l

(*
  A (rhs) is syntactically valid if
  all variables in the (rhs) also occurs in the (lhs)
  and their have the same base type.
*)
let rule_check_rhs (((_, l_ty) as lhs, ((_, r_ty) as rhs)) : rule) : bool =
  let r_vars = term_get_vars rhs in
    match r_vars with
    | [] -> typ_equal l_ty r_ty
    | _ ->
      if is_sublist var_equal r_vars (term_get_vars lhs) && typ_equal l_ty r_ty
      then true else false

let rule_check =
  fun rul -> rule_check_lhs rul && rule_check_rhs rul
