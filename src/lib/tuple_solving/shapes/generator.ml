open Syntax.Term
open Syntax.Trs
open Config
open Monad
open Tuple
module G = Gener

(* Generates the interpretations of function symbols.
Its return type [(func * (cost * size)) list Templ.Undet.t]
is in fact a list of pairs of function symbols and cost-size tuples
living in the Templ.Undet.t state monad.
It will be generally the case that form the output of this the caller will
apply the state monad to [0].
This value defines the initial name for the coefficients generated in the body of the polynomials.
*)
let func_shapes (int_key : sort -> int) (template : Templ.templ)
    (fn_list : func list) : (func * (cost * size)) list Templ.Undet.t =
  let open Templ.Undet in
  let open Utility (Templ.Undet) in
  let j_map f = G.gener_f int_key template f in
  let m fn =
    let* tuple = j_map fn in
    return (fn, tuple)
  in
  rev_mapM m fn_list

(* given an interpretation key and a function symbol
we return a list of input dimentions for each
sort argument of fn *)
let get_indims (int_key : sort -> int) fn =
  List.map int_key (typ_ins (func_get_typ fn))

let get_outdim (int_key : sort -> int) fn =
  int_key (typ_out (func_get_typ fn))

(* Generates a zero-cost cost component *)
let zero_cost (f : func) : cost =
  let arity = List.length (typ_ins (func_get_typ f)) in
  let rec generator ar =
    match ar with
    | 0 -> Cost (P.zero, Ret ())
    | _ -> Cost (P.zero, Abs (fun _ -> generator (ar - 1) ))
  in generator arity


let func_shapes_zero_cost (int_key : sort -> int) (template : Templ.templ) (fn_list : func list) =
  let open Templ.Undet in
  let open Utility (Templ.Undet) in
  let gen_size f = Templ.size_t template (get_indims int_key f) (get_outdim int_key f) in
  let m fn =
    let cost = (zero_cost fn) in
    let* size = gen_size fn in
    return (fn, (cost,size))
  in
  rev_mapM m fn_list

(* The func interpret *)
let func_int intpr =
  Lists.map_from_assoc_list func_equal intpr

(*  *)
let var_int (int_key : sort -> int) =
  let vars = var_sylst () in
  List.combine vars (List.mapi (G.gener_v int_key) vars)
  |> Lists.map_from_assoc_list var_equal

let coef_to_string = function
  | [(1, [name])] -> "c" ^ Int.to_string name
  | _ -> failwith "Error! Trying to print coefficient of wrong shape."

let indet_to_string (atom : A.t) =
  match atom.exhib with
  | Indet (name,proj) ->
    "X" ^ (Int.to_string name) ^ "_" ^ (Int.to_string proj)
  | _ -> failwith "Error! Trying to print atom of wrong shape."

let sat_shape_to_string coef indet = function
  | (cost, size) ->
      "<" ^ "(" ^ (Tuple.poly_to_string coef indet) cost ^ ")" ^
      " || " ^
      "(" ^
      String.concat ", "
        (List.map (fun x -> (Tuple.poly_to_string coef indet x)) size) ^
      ")" ^
      ">"

let print_sat_shape s =
  s |> sat_shape_to_string coef_to_string indet_to_string |> print_endline
