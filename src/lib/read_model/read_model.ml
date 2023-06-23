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
