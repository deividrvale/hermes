(*-----------------------------------------------------------------------------
  Abstract symbols
-----------------------------------------------------------------------------*)
module type SYMBOL = sig
  exception Name_Not_found of string

  type t
  val equal : t -> t -> bool
  val sylst : unit -> t list
  (** sylst returns a list of all known symbols *)

  val of_string : string -> t
  (** of_string_opt looks up the given name and fails when no known symbol has the name *)

  val of_string_opt : string -> t option
  (** of_string_opt looks up the given name and fails when no known symbol has the name *)

  val symbolize : (t -> unit) -> string -> t
  (* symbolize registers the given name (if not registered already) and returns the corresponding symbol *)
  val to_string : t -> string
  (* to_string returns the name of the given symbol *)
end

(* SymInt is a SYMBOL module built over integers. *)
module SymInt () : SYMBOL = struct
  exception Name_Not_found of string
  let count = ref 0                                     (* !count equals the length of !token *)

  let token = ref []                                    (* token stores symbol names in reverse order *)

  type t = int

  let equal = Int.equal

  let sylst _ = List.init !count Fun.id                 (* 0; 1; ...; !count - 1 *)

  let rec mem_idx str lst idx =                         (* mem_idx str lst 0 is the index of the first element that equals str in lst *)
    match lst with
      [] ->
      idx
    | hd::tl ->
      if String.equal str hd then
        idx
      else
        mem_idx str tl (idx + 1)

  let of_string_opt name =
    let idx = mem_idx name !token 0 in
    if idx >= !count then
      None
    else
      Some (!count - 1 - idx)

  let of_string name =
    match of_string_opt name with
    | None -> raise (Name_Not_found ("Symbol: '" ^ name ^ "' is not registered."))
    | Some x -> x

  let symbolize known name =                            (* known may raise exceptions *)
    match of_string_opt name with
      None ->
      let n = !count in
      begin
        count := n + 1;
        token := name::!token;
        n
      end
    | Some sym ->
      begin
        known sym;                                      (* known provides a means of changing the control flow when the given name is already known *)
        sym
      end

  let to_string sym = List.nth !token (!count - 1 - sym)
end

(*--------------------------------------------------------------------
  Sort, Func, and Var
--------------------------------------------------------------------*)

module Sort = SymInt()

type sort = Sort.t

let sort_equal = Sort.equal

let sort_sylst = Sort.sylst

let sort_to_string = Sort.to_string

type typ = sort list * sort                             (* flattened *)

let typ_mk ins out =
  let symbolize = Sort.symbolize @@ Fun.const () in
  List.map symbolize ins, symbolize out                 (* sorts are not checked and registered on the fly *)

let typ_ins = fst

let typ_out = snd

let typ_equal t1 t2 =
  List.equal sort_equal (typ_ins t1) (typ_ins t2) &&
  sort_equal (typ_out t1) (typ_out t2)


let typ_is_sort (ty : typ) : bool =
  match (typ_ins ty) with
  | [] -> true
  | _ -> false

module Func = SymInt()

type func = Func.t

let func_equal = Func.equal

let func_sylst = Func.sylst

let func_of_string = Func.of_string

let func_of_string_opt = Func.of_string_opt

let func_symbolize = Func.symbolize

let func_to_string = Func.to_string

module Var = SymInt()

type var = Var.t

let var_equal = Var.equal

let var_sylst = Var.sylst

let var_of_string = Var.of_string

let var_of_string_opt = Var.of_string_opt

let var_symbolize = Var.symbolize

let var_to_string = Var.to_string

let decl_set (decl : ('a * 'b) list ref) sym t =
  decl := (sym, t)::!decl

let decl_get (decl : ('a * 'b) list ref) equal sym =
  !decl
  |> List.find (fun (s, _) -> equal s sym)
  |> snd

let func_decl : (func * typ) list ref = ref []

let func_set_typ = decl_set func_decl

let func_get_typ = decl_get func_decl func_equal

let var_decl : (var * sort) list ref = ref []

let var_set_sort = decl_set var_decl

let var_get_sort = decl_get var_decl var_equal

(*--------------------------------------------------------------------
  Terms
--------------------------------------------------------------------*)

exception Typing_error of string

type sym = F of func | V of var
(* function symbols and variables *)

let sym_mk_opt name =
  match func_of_string_opt name with                    (* first check if name corresponds to any known function symbol *)
    None ->
    Option.map (fun v -> V v) (var_of_string_opt name)
  | Some f ->
    Some (F f)

type ('a, 'b) term_tree =
    Sym of 'a
  | App of (('a, 'b) term_tree * 'b) * (('a, 'b) term_tree * 'b)

type term = (sym, typ) term_tree * typ                  (* a term is a pair consisting of its tree structure and its type *)

let rec term_mk_opt =
  let open Monad.Option in
  function
    Sym name ->
    let* sy = sym_mk_opt name in
    let ty = match sy with
        F f -> func_get_typ f
      | V v -> [], var_get_sort v in
    return (Sym sy, ty)
  | App ((tr1, _), (tr2, _)) ->
    let* (_, ty1) as t1 = term_mk_opt tr1 in
    let* (_, ty2) as t2 = term_mk_opt tr2 in
    match typ_ins ty1, typ_ins ty2 with
      hd::tl, [] ->                                     (* arguments are supposed to be first-order *)
      if sort_equal hd (typ_out ty2) then
        return (App (t1, t2), (tl, typ_out ty1))
      else
        None
    | _ ->
      None

let term_mk t =
  match term_mk_opt t with
  | None -> raise (Typing_error "Term cannot be typed.")
  | Some s -> s

let rec term_get_vars = function
    Sym (F _), _ ->
    []
  | Sym (V v), _ ->
    v::[]
  | App (t1, t2), _ ->
    List.rev_append
      (term_get_vars t1)
      (term_get_vars t2)

let term_get_typ = snd


(*--------------------------------------------------------------------
  Pretty printing for types
--------------------------------------------------------------------*)

let pp_typ_sep (f : formatter) = fun () ->
  pp_print_space f ();
  pp_print_string f "-->";
  pp_print_space f ()

let pp_print_sort f =
  pp_print_string f

let pp_print_typ_ins f typ_ins =
  let ins_tags = List.map sort_to_string typ_ins in
  open_box 0;
  (* print_string "["; *)
  pp_print_list ~pp_sep: pp_typ_sep pp_print_string f ins_tags
  (* print_string "]"; *)

let pp_print_typ f typ =
  match (typ_ins typ) with
  | [] ->
    open_box 0;
    pp_print_sort f (sort_to_string (typ_out typ))
  | _ :: _ ->
    open_box 0;
    pp_print_typ_ins f (typ_ins typ);
    pp_typ_sep f ();
    pp_print_sort f (sort_to_string (typ_out typ))

let print_type = pp_print_typ std_formatter

(*--------------------------------------------------------------------
  Pretty Printing for terms
--------------------------------------------------------------------*)
open Format
let pp_print_func (f : formatter) (func : func) =
  open_box 0;
  pp_print_string f (func_to_string func)

let print_func func =
  pp_print_func std_formatter func

let pp_print_var (f : formatter) (x : var) =
  open_box 0;
  pp_print_string f (var_to_string x)

let print_var (x : var) =
  pp_print_var std_formatter x

let pp_print_symb (f : formatter) (s : sym) =
  open_box 0;
  match s with
    | V x -> pp_print_var f x
    | F func -> pp_print_func f func

let print_symb (s : sym) =
  pp_print_symb std_formatter s

(**
    auxiliary function to [pp_print_term]
    it receives an extra boolean argument [b]
    which defines the behavior of outer-parentheses when
    printing terms
*)
let rec pp_print_term' (f : formatter ) (b : bool) (t : term) =
  match t with
  | (Sym s, _) -> pp_print_symb f s
  | e ->
    if b then (* an outer parenthesis on terms is not printed *)
      begin
        open_hovbox 0;
        print_app f e;
        close_box ()
      end
    else (* this case is only evaluated when printing applications on the rhs which doesnt occur in the root position*)
      begin
        open_hovbox 0;
        pp_print_string f "(";
        print_app f e;
        pp_print_string f ")";
        close_box ()
      end
and print_app (f : formatter) = function
  e -> open_hovbox 0;
  print_other_applications f e;
  close_box ()
and print_other_applications (f : formatter) (t : term) =
  match t with
  | (App (t1, t2), _) ->
    print_app f t1;
    pp_print_space f ();
    pp_print_term' f false t2
  | e -> pp_print_term' f false e

let pp_print_term (f : formatter) (t : term) =
  pp_print_term' f true t

let print_term =
  pp_print_term std_formatter
