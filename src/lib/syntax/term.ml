open Seq

module type SYMBOL = sig
  type t
  val equal : t -> t -> bool
  val syseq : unit -> t Seq.t
  val of_string_opt : string -> t option
  val symbolize : (t -> unit) -> string -> t
  val to_string : t -> string
end

module type SYMINTREC = sig
  val count : int ref
  val token : string list ref (* in reverse order *)
end

module SymInt (R : SYMINTREC) : SYMBOL = struct
  open R

  type t = int

  let equal = Int.equal

  let syseq _ = Seq.init !count Fun.id

  let rec mem_idx str lst idx =
    match lst with
      [] ->
      idx
    | hd::tl ->
      if String.equal str hd
      then idx
      else mem_idx str tl (idx + 1)

  let of_string_opt str =
    let idx = mem_idx str !token 0 in
    if idx >= !count
    then None
    else Some (!count - 1 - idx)

  let symbolize xst str =     (* xst may raise exceptions *)
    match of_string_opt str with
      None ->
      let n = !count in
      begin
        count := n + 1;
        token := str::!token;
        n
      end
    | Some sym ->
      begin
        xst sym;
        sym
      end

  let to_string sym = List.nth !token (!count - 1 - sym)
end

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

type typ = sort list * sort

let typ_mk lst str =
  let symbolize = () |> Fun.const |> Sort.symbolize in
  (List.map symbolize lst, symbolize str)

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

type sym = F of func | V of var

let sym_mk_opt str =
  match func_of_string_opt str with
    None -> Option.map (fun v -> V v) (var_of_string_opt str)
  | Some f -> Some (F f)

type ('a, 'b) term_tree =
    Sym of 'a
  | App of (('a, 'b) term_tree * 'b) * (('a, 'b) term_tree * 'b)

type term = (sym, typ) term_tree * typ

let rec term_mk_opt env ttr =
  match ttr with
    Sym str ->
    Option.bind (sym_mk_opt str)
      (fun sy -> Option.map (fun ty -> (Sym sy, ty)) (env sy))
  | App (op, ar) ->
    Option.bind (term_mk_opt env (fst ar))
      (fun ((_, aty) as atr) ->
         match typ_ins aty with
           [] ->
           Option.bind (term_mk_opt env (fst op))
             (fun ((_, oty) as otr) ->
                match typ_ins oty with
                  [] ->
                  None
                | hd::tl ->
                  if sort_equal (typ_out aty) hd
                  then Some (App (otr, atr), (tl, typ_out oty))
                  else None)
         | _ ->
           None)
