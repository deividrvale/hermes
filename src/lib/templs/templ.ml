open Monad
open Tuple

module Undet = State(Int)
open Undet
open Utility(Undet)

type templ = { cnt : int list -> int;
               exp : int -> (P.t list * int) list -> P.t }

let monadify { cnt; exp } indims c =
  (fun vs -> exp c @@ List.combine vs indims),
  c + cnt indims

let cost_t t indims =
  let* p = monadify t indims in
  let rec cost_t_aux vs = function
      [] ->
      Cost (p (List.rev vs), Ret ())
    | _::tl ->
      Cost (P.zero, Abs (fun v -> cost_t_aux (v::vs) tl)) in
  return @@ cost_t_aux [] indims

let size_t t indims outdim =
  let m = monadify t indims in
  let* ps = rev_replicateM m outdim in
  let rec size_t_aux vs = function
      [] ->
      Size (Ret (List.rev_map (fun p -> p (List.rev vs)) ps))
    | _::tl ->
      Size (Abs (fun v -> size_t_aux (v::vs) tl)) in
  return @@ size_t_aux [] indims
