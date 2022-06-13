open Syntax.Term

(* A test on parsing types *)
let example =
"
[nat; nat ] --> nat
";;



(* Building terms *)

(* symbolize a variable *)
let x = var_symbolize (fun _ -> ()) "x"
let f = func_symbolize (fun _ -> ()) "f"

let s = typ_out (typ_mk [] "s")


let () =
  print_endline "Printer: printing a parsed type";
  Printer.print_type (Parser.parse_from_string example);
  Format.printf "@[%s@]@." "\n Printing symbols and variables";
  Printer.print_func f;
  Printer.print_var x;
  Printer.print_symb (F f);
  Printer.print_symb (V x)

