open Z3

type t = { context : context;
           solver : Solver.solver }

let mk_env cfg =
  let context = mk_context cfg in
  let solver = Solver.mk_solver context None in
  { context; solver }

let mk_int_const n { context; _ } =
  Arithmetic.Integer.mk_const context (Symbol.mk_int context n)

let mk_int_numeral i { context; _ } =
  Arithmetic.Integer.mk_numeral_i context i

let mk_add ts { context; _ } =
  Arithmetic.mk_add context ts

let mk_mul ts { context; _ } =
  Arithmetic.mk_mul context ts

let mk_ge t1 t2 { context; _ } =
  Arithmetic.mk_ge context t1 t2

let check_for_model assertions { solver; _ } =
  let open Solver in
  reset solver;
  let check_status = check solver assertions in
  (* Note:
    verbosily printing the status of Z3 checking step in the stdoutput.
    This is specially usefull for debugging.
  *)
  let () = begin
    match check_status with
    | UNSATISFIABLE ->
      print_endline
      ("Z3 check of constraints is " ^ string_of_status UNSATISFIABLE)
    | UNKNOWN ->
      print_endline
      ("Z3 check of constraints is " ^ string_of_status UNKNOWN)
    | SATISFIABLE ->
      print_endline
      ("Z3 check of constraints is " ^ string_of_status SATISFIABLE)
    end
  in
  match check solver assertions with
  | SATISFIABLE ->
    get_model solver
  | _ ->
    None
