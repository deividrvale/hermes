(**
  {0 {b Module} Term}
*)

(*--------------------------------------------------------------------
  Sort and Type
--------------------------------------------------------------------*)

type sort

val sort_equal : sort -> sort -> bool

val sort_sylst : unit -> sort list

val sort_to_string : sort -> string

type typ

val typ_mk : string list -> string -> typ

val typ_ins : typ -> sort list

val typ_out : typ -> sort

val typ_equal : typ -> typ -> bool
(**
  [type_equal typ typ'] is whether [typ] and [typ']
  are structurally equal.
*)

val typ_is_sort : typ -> bool
(** [type_is_sort typ] is whether [typ] is a sort. *)

(*--------------------------------------------------------------------
  Function Symbols
--------------------------------------------------------------------*)

type func

val func_equal : func -> func -> bool

val func_sylst : unit -> func list

val func_of_string : string -> func

val func_of_string_opt : string -> func option

val func_symbolize : (func -> unit) -> string -> func

val func_to_string : func -> string

val func_set_typ : func -> typ -> unit

val func_get_typ : func -> typ

(*--------------------------------------------------------------------
  Variable
--------------------------------------------------------------------*)

type var
(** OCaml type for variables *)

val var_equal : var -> var -> bool
(**
[var_equal x y] is wheater [x] and [y]
are structurally equal.
*)

val var_sylst : unit -> var list

val var_of_string : string -> var

val var_of_string_opt : string -> var option

val var_symbolize : (var -> unit) -> string -> var

val var_to_string : var -> string

val var_set_sort : var -> sort -> unit

val var_get_sort : var -> sort

(*--------------------------------------------------------------------
  Term Syntax
--------------------------------------------------------------------*)

type sym = F of func | V of var

val sym_mk_opt : string -> sym option

type ('a, 'b) term_tree =
    Sym of 'a
  | App of (('a, 'b) term_tree * 'b) * (('a, 'b) term_tree * 'b)

type term = (sym, typ) term_tree * typ

val term_mk : (string, 'b) term_tree -> term

val term_mk_opt : (string, 'b) term_tree -> term option

val term_get_vars : term -> var list
(**
[term_get_vars tm]
    is the list containing all
    variables occurring in [tm].
*)

val term_get_typ : term -> typ
(**
    [term_get_typ tm] is the type of [tm].
*)

(*--------------------------------------------------------------------
  Pretty Printing
--------------------------------------------------------------------*)
open Format

val pp_print_func : formatter -> func -> unit

val print_func : func -> unit

val pp_print_var : formatter -> var -> unit

val print_var : var -> unit

val pp_print_symb : formatter -> sym -> unit

val print_sym : sym -> unit

val pp_print_term : formatter -> term -> unit

val print_term : term -> unit
