open Lex
open Syntax.Term
include File

(*--------------------------------------------------------------------
  Individual parsers
--------------------------------------------------------------------*)

(* Print position information when an error occurs. *)
let print_pos outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(** Parse a lexer bufffer or exit in a failure state. *)
let parse_with_error parser (lexbuf : Lexing.lexbuf) =
  try parser Lex.lexer lexbuf with
  | SyntaxError msg ->
    Printf.fprintf stderr "%a: %s.\n" print_pos lexbuf msg;
    exit (-1)
  | Par.Error ->
    Printf.fprintf stderr "\nCould not parse string: syntax error at %a \n" print_pos lexbuf;
    exit (-1)

(*
let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_with_error lexbuf;
  In_channel.close inx;; *)

(** Parse a AFS file format from string. *)
let parse_from_string p (s : string) =
  let lexbuf = Lexing.from_string s in
    parse_with_error p lexbuf

(*--------------------------------------------------------------------
  Individual parsers
--------------------------------------------------------------------*)

let parser_typ = Par.typ

let parser_term_tree = Par.parse_term_tree

let parse_file = Par.file

let parse_file_from_string s =
  parse_from_string parse_file s

let rec symbolize_parsed_sym (f : parsed_file ) =
  let ls = f.ar in
  to_func ls
and to_func = function
  | [] -> []
  | (name, ty) :: tail ->
    let func = func_symbolize (fun _ -> print_string "Error while symbolizing parsed symbols.") name in
      func_set_typ func ty;
      func :: to_func tail

let rec symbolize_parsed_var (f : parsed_file ) =
  let ls = f.var in
  to_var ls
and to_var = function
  [] -> []
  | (name, ty) :: tail ->
    let var = var_symbolize
      (fun _ -> Format.print_string
      "\nError while symbolizing parsed variables.
       \nCheck for multiple variable declaration with diferent types.") name in
      var_set_sort var (typ_out ty);
      var :: to_var tail

(*
  end of day:
    two symbolizers seems to work
    TODO create a generic function module on utils to hold some functions over lists
    1.
*)
