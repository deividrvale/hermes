open Tuple
open Templ
open Undet
open Monad.Utility(Undet)

let cnt indims = List.fold_left ( + ) 1 indims

let exp c vds =
  let undet c = [(Int.one, [c])], c + 1 in
  let rec exp_aux accum = function
      [] ->
      let* u = undet in
      return @@ (u, [])::accum
    | (vec, dim)::tl ->
      let* us = rev_replicateM undet dim in
      exp_aux (List.mapi (fun i u -> u, [bound vec (dim - 1 - i)]) us @ accum) tl in
  exp_aux P.zero vds c |> fst |> List.rev

let affine = { cnt; exp }
