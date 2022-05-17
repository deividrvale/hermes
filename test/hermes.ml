open Syntax.Term

let x = Syntax.Term.typ_mk ["nat"; "sort"] "asd"

let () = print_endline (sort_to_string (typ_out x))
