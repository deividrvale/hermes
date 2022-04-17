(* Simple Types *)
type 'b ty =
  | Sort of 'b
  | Arrow of 'b ty * 'b ty

(* Applicative Terms *)
type ('v, 'f) term =
  | Var of 'v
  | Symbol of 'f
  | App of ('v, 'f) term * ('v, 'f) term

(* Defines the signature of applicative systems and the base types.
  'v is the type for variables
  'f is the type for symbols
  'b is the type for base symbols
*)
type ('v, 'f, 'b) signature = {
  symbols : 'f list;
  arity : ('f -> 'b);
}
