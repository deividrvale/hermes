open Z3

let get_assigns model =
  let open Monad in
  let open Option in
  let open Utility(Option) in
  rev_mapM
    (fun decl ->
       let* interp = Model.get_const_interp model decl in
       return (Symbol.get_int (FuncDecl.get_name decl), interp))
    (Model.get_const_decls model)

let z3_env = Z3env.mk_env []

let get_model exprs =
  (let open Monad.Reader(Z3env) in
    let* exprs = exprs in
      print_endline "Checking for Z3 model of constraints...";
      Z3env.check_for_model exprs) z3_env

let check_model_existence model =
  let result =
    let open Monad.Option in
        let* model in
        get_assigns model
  in
  match result with
  | None ->
    print_endline "Z3 model doesn't exist.";
    None
  | Some [] ->
    print_endline "Z3 model doesn't exist.";
    None
  | Some l -> Some l
