(* A very simple main module *)

(* Print string and flush standard output --- side effects baby! *)
let print_string_flush s = (print_string s ; flush stdout)

(* Defines string for man page. *)
let str_usage = "\
\n\
Usage :\n\
-------\n\
\n\
hermes help                : print help\n\
\n\
hermes version             : show version\n\
\n\
"

exception Bad_usage

(* Get arguments provided by the user. *)
let getArgument i =
    if i < Array.length Sys.argv then
        Sys.argv.(i)
    else
        raise Bad_usage

let print_help () =
    print_string_flush str_usage

let print_version () =
  print_string_flush "0.1.0"

let no_args () =
    print_help ()

let main () =
  try (match getArgument 1 with
        | "help"    -> print_help ()
        | "version" -> print_version ()
        | _         -> raise Bad_usage
      )
  with Bad_usage -> no_args ()
;;

main ()
