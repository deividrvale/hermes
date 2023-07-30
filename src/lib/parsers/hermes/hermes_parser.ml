open Parser

module LexPar : PARSER with type t = Par.token = struct
  type t = Par.token
  include Lex
  include Par
end

module P = ParseWithErrors (LexPar)
include P

let lexer : lexer = Lex.lexer

let parser : 'a parser = Par.file

let debug_parser : 'a parser = Par.debug_parser
