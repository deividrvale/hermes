open Syntax.Term
open Format

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
