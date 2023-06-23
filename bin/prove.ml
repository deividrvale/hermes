open Config

let parse_file file =
  Hermes_parser.parse_from_string
  Hermes_parser.parser
  Hermes_parser.lexer
  file

let get_data file =
  parse_file file |> File.Onijn.process_file
