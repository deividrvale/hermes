open Term

(**
  [is_sublist eq xs ys] is whether [xs] is a sublist of [ys],
  with [eq] used as equality operator.
*)
let rec is_sublist eq (xs : 'a list) (ys : 'a list) : bool =
  List.for_all (fun x -> List.exists (eq x) ys) xs

type rule = term * term

let pp_print_rule (f : Format.formatter) (rule : rule) =
  let open Format in
  open_hbox ();
  pp_print_term f (fst rule);
  pp_print_string f " => ";
  pp_print_term f (snd rule);
  close_box ()

let print_rule rule =
  pp_print_rule Format.std_formatter rule

type inv_lhs = VAR | SORT | HEAD

let print_lhs_err err rule =
  let open Format in
  match err with
  | VAR -> begin
    open_hbox ();
    print_string "The rule ";
    print_rule rule;
    print_string " is not valid.\nIts (lhs) is a variable.";
    close_box ()
  end
  | SORT -> begin
    open_hbox ();
    print_string "The rule ";
    print_rule rule;
    print_string " is not valid.\nIts (lhs) is not of base type.";
    close_box ()
  end
  | HEAD -> begin
    open_hbox ();
    print_string "The rule ";
    print_rule rule;
    print_string " is not valid.\nIts (lhs) is not of headed by a function symbol."
  end

(*
A lhs is syntactically valid if
  (i) it is headed by a function symbol,
  (ii) and it is of base type.
If a given rule is not valid, there is no way of recovering from it.
So we print an error message and quits the application.*)
let rec rule_check_lhs (((l, ty), _) as rule : rule) : unit =
  match l with
  | Sym (F _) -> if typ_is_sort ty then ()
    else (
      print_lhs_err SORT rule;
      exit 1
    )
  | Sym (V _) ->
    print_lhs_err VAR rule;
    exit 1
  | _ ->
    if test_head_symbol l then ()
    else (
      print_lhs_err HEAD rule;
      exit 1;
    )
and test_head_symbol = function
    Sym (V _) -> false
  | Sym (F _) -> true
  | App ((l, _), _) -> test_head_symbol l

type inv_rhs = TYEQ | RVAR

let print_rhs_err err rule =
  let open Format in
  match err with
  | TYEQ -> begin
    open_hbox ();
    print_string "The rule ";
    print_rule rule;
    print_string " is not valid.\nThe type of (lhs) and (rhs) are different.";
    close_box ()
  end
  | RVAR -> begin
    open_hbox ();
    print_string "The rule ";
    print_rule rule;
    print_string " is not valid.\nThe variables in the (rhs) should all also be variables in the (lhs).";
    close_box ()
  end


(*
A (rhs) is syntactically valid if
  (i) all variables in the (rhs) also occurs in the (lhs), and
  (ii) the (lhs) and (rhs) have the same base type.
*)

let rule_check_rhs ((((_, l_ty) as lhs, ((_, r_ty) as rhs) ) as rule) : rule) =
  let type_check = begin
    if typ_equal l_ty r_ty then ()
      else (
        print_rhs_err TYEQ rule;
        exit 1
    )
  end in
  let r_vars = term_get_vars rhs in
    match r_vars with
    | [] -> type_check
    | _ ->
      if is_sublist var_equal r_vars (term_get_vars lhs)
        then ()
      else (
        print_rhs_err RVAR rule;
        exit 1
      )

let rule_check =
  fun rule ->
    rule_check_lhs rule;
    rule_check_rhs rule
