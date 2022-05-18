%{
    open Syntax.Term
%}

%token <string> STRING
%token <string> VNAME
%token RW_ARR
%token TY_ARR
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEP
%token EOF

%start typ

%type < Syntax.Term.typ > typ

%%

// file:
//   typ
//   EOF { typ }

baseT:
  | STRING              { $1 }

// argument list for type construction
input_sort_list:
  | LBRACE sl = separated_nonempty_list(SEP, baseT) RBRACE { sl }

typ:
  | input_sort_list TY_ARR baseT      { typ_mk $1 $3 }
