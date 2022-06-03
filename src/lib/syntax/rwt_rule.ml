open Term


type rule = term * term


(*
  a lhs is syntactic valid
  if it is headed by a function symbol
  and it is of base type
*)
let rule_check_lhs (((l, ty), (_, _)) : rule) : bool =
  match l with
  | Sym (F _) -> typ_is_sort ty
  | Sym (V _) -> false
  | App ((Sym (F _), _), _) -> typ_is_sort ty
  | _ -> false

let rec rule_check_rhs (((l, l_ty), ((r, r_ty) as rhs)) : rule) : bool =
  let r_vars = term_get_vars rhs in
    match r_vars with
    | [] -> typ_equal l_ty r_ty
    | _ -> false


let rec is_sublist eq xs ys =
  match (xs, ys) with
  | ([],[]) -> true
  | (_, []) -> false
  (* | () *)


