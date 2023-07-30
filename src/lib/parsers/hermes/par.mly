%{
  open Syntax.Term
  open File.Onijn
%}

// Tokens
%token <string> STRING
%token RW_ARR
%token TY_ARR
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SIG_ID
%token VAR_ID
%token RULE_ID

%token COLON

%token SEP
%token EOF

// Declarations
%start parse_term_tree file debug_parser

%type < fakeTy > fake_ty
%type < (string, unit) term_tree > term_tree
%type < (string, unit) term_tree > parse_term_tree

%type < string * fakeTy > sym_dec
%type < string * fakeTy > var_dec
%type < (string * fakeTy) list > signature
%type < (string * fakeTy) list > variables
%type < (string, unit) term_tree * (string, unit) term_tree > rule
%type < ( (string, unit) term_tree * (string, unit) term_tree ) list > trs

%type < 'a > debug_parser

%type < File.Onijn.parsed_file > file

%%

// Types
baseT:
    | STRING { $1 }
    | LPAREN baseT RPAREN { $2 }

fake_ty:
    | baseT { Name $1 }
    | fake_ty TY_ARR fake_ty { Arr ($1, $3) }
    | LPAREN fake_ty RPAREN { $2 }

// Terms a.k.a terms tree
parse_term_tree:
  | t = term_tree EOF                 { t }

term_tree:
  | non_app                           { $1 }
  | app                               { $1 }

app:
  | app non_app                       { App (($1, ()), ($2, ()))}
  | non_app non_app                   { App (($1, ()), ($2, ()))}

non_app:
  | s = STRING                        {Sym s }
  | LPAREN t = term_tree RPAREN       { t }

sym_dec:
  f = STRING COLON ty = fake_ty { (f, ty) }

var_dec:
  x = STRING COLON ty = fake_ty { (x, ty) }

signature:
  ls = separated_nonempty_list(SEP, sym_dec) { ls }

variables:
  ls = separated_list(SEP, var_dec) { ls }

rule:
  l = term_tree RW_ARR r = term_tree { (l, r) }

trs:
  ls = separated_nonempty_list(SEP, rule)  { ls }

file:
  SIG_ID COLON LBRACE s = signature RBRACE
  VAR_ID COLON LBRACE v = variables RBRACE
  RULE_ID COLON LBRACE r = trs RBRACE
  EOF { File.Onijn.parsed_file_mk s v r }

debug_parser:
    | term_tree EOF { $1 }
