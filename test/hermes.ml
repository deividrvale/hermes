open Syntax.Term

let x = Syntax.Term.typ_mk ["nat"; "sort"] "asd"

let example = "[nat] ==> nat ";;

let typ_example = Parser.parse_from_string example;;
