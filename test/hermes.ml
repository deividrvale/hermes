open Syntax.Term
open Syntax.Rwt_rule

(* A test on parsing types *)
let example = "[nat; nat ] --> nat";;
let tp = Parser.parse_from_string Parser.parser_typ example

(* Building terms *)

(* symbolize symbols *)
let x = var_symbolize (fun _ -> ()) "x"
let f = func_symbolize (fun _ -> print_endline "Can't symbolize this symbol.") "f"
let s = typ_out (typ_mk [] "nat")

let () =
  var_set_sort x s;
  func_set_typ f (typ_mk ["nat"; "nat"] "nat")

let () =
  Printer.print_type tp;
  Format.print_newline ()

let input_lhs = "f (x) x"
let input_rhs = "f x"

let parsed_lhs = Parser.parse_from_string Parser.parser_term_tree input_lhs
let parsed_rhs = Parser.parse_from_string Parser.parser_term_tree input_rhs
let () =
  let (lhs, rhs) = (term_mk_opt parsed_lhs, term_mk_opt parsed_rhs)  in
  match lhs with
  | Some t ->
    Format.print_string "Parsed term: ";
    Printer.print_term t;
    Format.print_string " with type: ";
    Printer.print_type (term_get_typ t);
    Format.print_newline ();
    Format.print_bool (rule_check_lhs (t,t))
  | None ->
    Format.print_string "terms didn't get created"


let file = "
signature [
  zero : nat;
  one : nat
]
vars [
  zero : nat
]
rules [
  zero ==> jose
]
"

let f = Parser.parse_from_string Parser.parse_file file

let signature = Parser.symbolize_parsed_sym f
let vars = Parser.symbolize_parsed_var f

let sep = Some (
  fun f -> (
  fun () -> Format.pp_print_string f "; ")
)

let () =
  (* Format.print_newline ();
  Format.print_string "[ ";
  Format.pp_print_list Printer.pp_print_func Format.std_formatter signature ?pp_sep: (sep);
  Format.print_string " ]"; *)
  print_endline (
    String.concat ";" (
      List.map var_to_string (var_sylst ())
    )
  )

