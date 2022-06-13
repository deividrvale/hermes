open Lex

(*--------------------------------------------------------------------
  Abstract symbols
--------------------------------------------------------------------*)

(* Print position information when an error occurs. *)
let print_pos outx (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(** Parse a lexer bufffer or exit in a failure state. *)
let parse_with_error (lexbuf : Lexing.lexbuf) =
  try Par.typ Lex.lexer lexbuf with
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
let parse_from_string (s : string) =
  let lexbuf = Lexing.from_string s in
    parse_with_error lexbuf
