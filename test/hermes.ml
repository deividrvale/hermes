open Syntax.Term

let arr = Syntax.Term.typ_mk ["nat"; "sort"] "asd";;

func_symbolize



let example = "[nat] ==> nat ";;

let typ_example = Parser.parse_from_string example;;
