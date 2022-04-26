type sort

val sort_equal : sort -> sort -> bool

val sort_syseq : unit -> sort Seq.t

val sort_to_string : sort -> string

type typ

val typ_mk : string list -> string -> typ

val typ_ins : typ -> sort list

val typ_out : typ -> sort

val typ_equal : typ -> typ -> bool

type func

val func_equal : func -> func -> bool

val func_syseq : unit -> func Seq.t

val func_of_string_opt : string -> func option

val func_symbolize : (func -> unit) -> string -> func

val func_to_string : func -> string

type var

val var_equal : var -> var -> bool

val var_syseq : unit -> var Seq.t

val var_of_string_opt : string -> var option

val var_symbolize : (var -> unit) -> string -> var

val var_to_string : var -> string

type sym = F of func | V of var

val sym_mk_opt : string -> sym option

type ('a, 'b) term_tree =
    Sym of 'a
  | App of (('a, 'b) term_tree * 'b) * (('a, 'b) term_tree * 'b)

type term = (sym, typ) term_tree * typ

val term_mk_opt : (sym -> typ option) -> (string, 'a) term_tree -> term option
