module IntVec = struct
  module CoefState = Monad.State(Int)
  open CoefState
  open Monad.Utility(CoefState)
  open Syntax.Term
  open Syntax.Tuple.IntVec

  let mono_undet atoms c0 =
    let coef = (1, c0::[])::[] in
    (coef, atoms), c0 - 1

  let affine vecdims =
    let rec affine_aux accum = function
        [] ->
        return accum
      | (vec, dim)::tl ->
        if dim <= 0 then
          affine_aux accum tl
        else
          let* mono = mono_undet [Atom.var vec (dim - 1)] in
          affine_aux (mono::accum) ((vec, dim - 1)::tl) in
    let* c = mono_undet [] in
    affine_aux [c] vecdims

  let affine_cost indims c0 =
    let c1 = c0 + List.fold_left ( + ) 1 indims in
    let rec affine_cost_aux vecdims = function
        [] ->
        Cost (fst (affine vecdims (c1 - 1)), Ret ())
      | hd::tl ->
        Cost ([], Abs (fun v -> affine_cost_aux ((v, hd)::vecdims) tl)) in
    affine_cost_aux [] indims, c1

  let affine_size indims outdim c0 =
    let c1 = c0 + outdim * List.fold_left ( + ) 1 indims in
    let rec affine_size_aux vecdims = function
        [] ->
        Size (Ret (fst (rev_replicateM (affine vecdims) outdim (c1 - 1))))
      | hd::tl ->
        Size (Abs (fun v -> affine_size_aux ((v, hd)::vecdims) tl)) in
    affine_size_aux [] indims, c1

  let gener dim assum (template_c, template_s) =
    let rec gener_aux accum = function
        [] ->
        return accum
      | hd::tl ->
        if List.exists (fun (f, _) -> func_equal f hd) assum then
          gener_aux accum tl
        else
          let func_t = func_get_typ hd in
          let indims = List.map dim (typ_ins func_t) in
          let outdim = dim (typ_out func_t) in
          let* c = template_c indims in
          let* s = template_s indims outdim in
          gener_aux ((hd, (c, s))::accum) tl in
    gener_aux [] @@ func_sylst ()
end
