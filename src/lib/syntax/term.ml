module type SYMBOL = sig
  type t
  val equal : t -> t -> bool
  val syseq : unit -> t Seq.t                           (* syseq returns a sequence of all known symbols *)
  val of_string_opt : string -> t option                (* of_string_opt looks up the given name and fails when no known symbol has the name *)
  val symbolize : (t -> unit) -> string -> t            (* symbolize registers the given name (if not registered already) and returns the corresponding symbol *)
  val to_string : t -> string                           (* to_string returns the name of the given symbol *)
end

module type REFSYMINT = sig
  val count : int ref                                   (* !count equals the length of !token *)
  val token : string list ref                           (* token stores symbol names in reverse order *)
end

module SymInt (R : REFSYMINT) : SYMBOL = struct
  open R

  type t = int

  let equal = Int.equal

  let syseq _ = Seq.init !count Fun.id                  (* 0; 1; ...; !count - 1 *)

  let rec mem_idx str lst idx =                         (* mem_idx str lst 0 is the index of the first element that equals str in lst *)
    match lst with
      [] ->
      idx
    | hd::tl ->
      if String.equal str hd
      then idx
      else mem_idx str tl (idx + 1)

  let of_string_opt name =
    let idx = mem_idx name !token 0 in
    if idx >= !count
    then None
    else Some (!count - 1 - idx)

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

(* hidden state variables below *)

module Sort = SymInt(
  struct
    let count = ref 0
    let token = ref []
  end)

module Func = SymInt(
  struct
    let count = ref 0
    let token = ref []
  end)

module Var = SymInt(
  struct
    let count = ref 0
    let token = ref []
  end)

(* export below *)

type sort = Sort.t

let sort_equal = Sort.equal

let sort_syseq = Sort.syseq

let sort_to_string = Sort.to_string

type typ = sort list * sort                             (* flattened *)

let typ_mk ins out =
  let symbolize = Sort.symbolize @@ Fun.const () in
  (List.map symbolize ins, symbolize out)               (* sorts are not checked and registered on the fly *)

let typ_ins = fst

let typ_out = snd

let typ_equal t1 t2 =
  List.equal sort_equal (typ_ins t1) (typ_ins t2) &&
  sort_equal (typ_out t1) (typ_out t2)

type func = Func.t

let func_equal = Func.equal

let func_syseq = Func.syseq

let func_of_string_opt = Func.of_string_opt

let func_symbolize = Func.symbolize

let func_to_string = Func.to_string

type var = Var.t

let var_equal = Var.equal

let var_syseq = Var.syseq

let var_of_string_opt = Var.of_string_opt

let var_symbolize = Var.symbolize

let var_to_string = Var.to_string

type sym = F of func | V of var                         (* function symbols and variables *)

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

let rec term_mk_opt env name_tree =
  match name_tree with
    Sym name ->
    Option.bind
      (sym_mk_opt name)
      (fun sy ->
         Option.map
           (fun ty -> (Sym sy, ty))
           (env sy))
  | App ((tr1, _), (tr2, _)) ->
    Option.bind
      (term_mk_opt env tr2)
      (fun ((_, ty2) as t2) ->
         match typ_ins ty2 with
           [] ->                                        (* arguments are supposed to be first-order *)
           Option.bind
             (term_mk_opt env tr1)
             (fun ((_, ty1) as t1) ->
                match typ_ins ty1 with
                  [] ->
                  None
                | hd::tl ->
                  if sort_equal (typ_out ty2) hd        (* type checking *)
                  then Some (App (t1, t2), (tl, typ_out ty1))
                  else None)
         | _ ->
           None)
