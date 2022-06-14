open Syntax.Term
open Format

(*--------------------------------------------------------------------
  Pretty printing for types
--------------------------------------------------------------------*)
(* let rec print_typ (ty : typ) =
  if typ_is_sort ty then
    print_string (sort_to_string ty) *)


let pp_typ_sep (f : formatter) = fun () ->
  pp_print_space f ();
  pp_print_string f "-->";
  pp_print_space f ()

let pp_print_sort f =
  pp_print_string f

let pp_print_typ_ins f typ_ins =
  let ins_tags = List.map sort_to_string typ_ins in
  open_box 0;
  (* print_string "["; *)
  pp_print_list ~pp_sep: pp_typ_sep pp_print_string f ins_tags
  (* print_string "]"; *)

let pp_print_typ f typ =
  match (typ_ins typ) with
  | [] ->
    open_box 0;
    pp_print_sort f (sort_to_string (typ_out typ))
  | _ :: _ ->
    open_box 0;
    pp_print_typ_ins f (typ_ins typ);
    pp_typ_sep f ();
    pp_print_sort f (sort_to_string (typ_out typ))

let print_type = pp_print_typ std_formatter

(*--------------------------------------------------------------------
  Pretty Printing for terms
--------------------------------------------------------------------*)

let pp_print_func (f : formatter) (func : func) =
  open_box 0;
  pp_print_string f (func_to_string func)

let print_func func =
  pp_print_func std_formatter func

let pp_print_var (f : formatter) (x : var) =
  open_box 0;
  pp_print_string f (var_to_string x)

let print_var (x : var) =
  pp_print_var std_formatter x

let pp_print_symb (f : formatter) (s : sym) =
  open_box 0;
  match s with
    | V x -> pp_print_var f x
    | F func -> pp_print_func f func

let print_symb (s : sym) =
  pp_print_symb std_formatter s

(**
    auxiliary function to [pp_print_term]
    it receives an extra boolean argument [b]
    which defines the behavior of outer-parentheses when
    printing terms
*)
let rec pp_print_term' (f : formatter ) (b : bool) (t : term) =
  match t with
  | (Sym s, _) -> pp_print_symb f s
  | e ->
    if b then (* an outer parenthesis on terms is not printed *)
      begin
        open_hovbox 0;
        print_app f e;
        close_box ()
      end
    else (* this case is only evaluated when printing applications on the rhs which doesnt occur in the root position*)
      begin
        open_hovbox 0;
        pp_print_string f "(";
        print_app f e;
        pp_print_string f ")";
        close_box ()
      end
and print_app (f : formatter) = function
  e -> open_hovbox 0;
  print_other_applications f e;
  close_box ()
and print_other_applications (f : formatter) (t : term) =
  match t with
  | (App (t1, t2), _) ->
    print_app f t1;
    pp_print_space f ();
    pp_print_term' f false t2
  | e -> pp_print_term' f false e

let pp_print_term (f : formatter) (t : term) =
  pp_print_term' f true t

let print_term =
  pp_print_term std_formatter
