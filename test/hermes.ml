open Syntax.Term

(* A test on parsing types *)
let example =
"
[nat; nat ] --> nat
";;

let () =
  print_endline "Printer: printing a parsed line.";
  Printer.print_type (Parser.parse_from_string example);
  print_endline ""

(* Building terms *)

(* symbolize a variable *)
let x = var_symbolize (fun _ -> ()) "x"

let s = typ_out (typ_mk [] "s")

let () =
  var_set_sort x s

