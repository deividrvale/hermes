open Tuple
open Templ
open Affine
open Undet

let cnt indims =
  let n = affine.cnt indims in
  n * (n + 1) / 2

let exp c vds =
  let undet c = [(Int.one, [c])], c + 1 in
  let rec exp_aux accum = function
      [] ->
      return accum
    | (_, 0)::tl ->
      exp_aux accum tl
    | ((vec, dim)::tl as vds) ->
      let rec exp_aux_aux accum = function
          [] ->
          exp_aux accum ((vec, dim - 1)::tl)
        | (_, 0)::t ->
          exp_aux_aux accum t
        | (v, d)::t ->
          let* u = undet in
          exp_aux_aux
            ((u, [bound v (d - 1); bound vec (dim - 1)])::accum)
            ((v, d - 1)::t) in
      exp_aux_aux accum vds in
  let accum = affine.exp c vds in
  exp_aux accum vds (c + List.length accum) |> fst

let quadratic = { cnt; exp }
