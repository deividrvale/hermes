%{
  open Syntax.Term
  open File
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
%token HAS_TYPE

%token SEP
%token EOF

// Declarations
%start typ parse_term_tree file

%type < typ > typ
%type < (string, unit) term_tree > term_tree
%type < (string, unit) term_tree > parse_term_tree

%type < string * typ > sym_dec
%type < string * typ > var_dec
%type < (string * typ) list > signature
%type < (string * typ) list > variables
%type < (string, unit) term_tree * (string, unit) term_tree > rule
%type < ( (string, unit) term_tree * (string, unit) term_tree ) list > trs

%type < File.parsed_file > file

%%

// Types
baseT:
  | STRING                            { $1 }

input_sort_list:
  | LBRACE sl = separated_nonempty_list(SEP, baseT) RBRACE { sl }

typ:
  | baseT                             { typ_mk [] $1 }
  | input_sort_list TY_ARR baseT      { typ_mk $1 $3 }

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
  f = STRING HAS_TYPE ty = typ { (f, ty) }

var_dec:
  x = STRING HAS_TYPE ty = typ { (x, ty) }

signature:
  ls = separated_nonempty_list(SEP, sym_dec) { ls }

variables:
  ls = separated_list(SEP, var_dec) { ls }

rule:
  l = term_tree RW_ARR r = term_tree { (l, r) }

trs:
  ls = separated_nonempty_list(SEP, rule)  { ls }

file:
  SIG_ID LBRACE s = signature RBRACE
  VAR_ID LBRACE v = variables RBRACE
  RULE_ID LBRACE r = trs RBRACE
  EOF { parsed_file_mk s v r }
