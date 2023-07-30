open Syntax.Term
module StringSet = Set.Make (String)

(*-----------------------------------------------------------------------------
    Functions that print critical errors and exit the application
  ---------------------------------------------------------------------------*)
let symbolize_err symb =
  let msg =
    "The name " ^ symb
    ^ " is duplicated in the input file.\n\
       Please note that the declarations for the signature and variables must \
       be unique\n\
       as two function symbols (and variables) cannot have different types."
  in
  print_string msg;
  exit 1

(*-----------------------------------------------------------------------------
    Base Types and Types
  ---------------------------------------------------------------------------*)
(* Since our inner representation of types is not compatible with ONijn input
   directly, we define a fakeTy datatype to be the intermediate parsing tree for
   types represented in the input file.
   Fake types are then translated to Herme's internal types.
*)
type fakeTy = Name of string | Arr of fakeTy * fakeTy

let rec fakeTy_to_string = function
  | Name s -> s
  | Arr (a, b) -> "(" ^ fakeTy_to_string a ^ "->" ^ fakeTy_to_string b ^ ")"

let rec args_of_fakeTy = function
  | Name _ -> []
  | Arr (Name n, ty) -> n :: args_of_fakeTy ty
  | Arr (_, _) ->
      (* TODO: implement error mensaging here *)
      let () = print_string "error in args_of_fakeTy" in
      exit 1

let rec output_of_fakeTy = function
  | Name n -> n
  | Arr (_, ty) -> output_of_fakeTy ty

let var_fakeTyArrMsg x fakeTy =
  "The variable " ^ x ^ " is declared in the input file with the type "
  ^ fakeTy_to_string fakeTy
  ^ " which is an arrow type.\nThis is not allowed on first-order systems.\n"

let fakeTy_to_sort x fty =
  match fty with
  | Name n -> typ_out (typ_mk [] n)
  | _ ->
      let _ = print_string (var_fakeTyArrMsg x fty) in
      exit 23

let fakeTy_to_ty fty =
  let args = args_of_fakeTy fty and out = output_of_fakeTy fty in
  Syntax.Term.typ_mk args out

(*-----------------------------------------------------------------------------
    Signature
  ---------------------------------------------------------------------------*)
type signature = (string * fakeTy) list

let register_signature (s : signature) =
  (* let open Syntax.Term in *)
  (* Register function symbol names *)
  let fn_names = List.map fst s in
  List.iter
    (fun x ->
      let _ = func_symbolize (fun _ -> symbolize_err x) x in
      ())
    fn_names;
  (* Register arities for each function symbol *)
  List.iter (fun (f, ty) -> func_set_typ (func_of_string f) (fakeTy_to_ty ty)) s

(*-----------------------------------------------------------------------------
    Variable enviroment
  ---------------------------------------------------------------------------*)
type environment = (string * fakeTy) list

let register_enviroment (env : environment) =
  let var_names = List.map fst env in
  List.iter
    (fun x ->
      let _ = var_symbolize (fun _ -> symbolize_err x) x in
      ())
    var_names;
  List.iter
    (fun (x, ty) -> var_set_sort (var_of_string x) (fakeTy_to_sort x ty))
    env

(*-----------------------------------------------------------------------------
    TRS
  ---------------------------------------------------------------------------*)
type trs = ((string, unit) term_tree * (string, unit) term_tree) list

let to_trs (trs : trs) =
  let open Syntax.Trs in
  let rules = List.map (fun (lhs, rhs) -> (term_mk lhs, term_mk rhs)) trs in
  List.iter rule_check rules;
  rules

(*-----------------------------------------------------------------------------
    File
  ---------------------------------------------------------------------------*)
type parsed_file = { ar : signature; var : environment; rules : trs }
type trs_data = { trs : (term * term) list }

let parsed_file_mk ar var rules = { ar; var; rules }

let process_file file =
  let _ = register_signature file.ar in
  let _ = register_enviroment file.var in
  { trs = to_trs file.rules }
