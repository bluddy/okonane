(* Randomness functions *)
open Util

(* initialize random generator *)
let _ = Random.self_init ()

(* carry out a random roll and compare it to a given probability *)
let roll_f prob = Random.float 1. < prob

(* randomness functions *)
(* select x members at random, assuming equal likelihood *)
let select_random num pop =
  let total = List.length pop in
  let init_p = 1. /. foi total in
  let _, (_, taken, left) = 
    iterate_until (* keep looping until we have enough *)
      (fun (i, (prob, take, rest)) -> 
        if list_null rest then failwith "population too small" else
        foldl_until_fin  (* loop over the population *)
          (fun (j, (p, t, r)) x ->
              if roll_f p
              then  (* adjust total and prob *)
                let j' = j + 1 in
                let total' = total - j' in
                if total' <= 0 then failwith "total num reached 0" else
                let p' = 1. /. foi total' in
                   j+1, (p', x::t, r)
              else j,   (p,  t, x::r)
          )
          (fun (j, _) _ -> j >= num) (* stop condition *)
          (fun (j, (p, t, r)) xs -> j, (p, t, r@xs))
          (0, (prob, take, []))
          rest
      )
      (fun (i, _) -> i >= num)
      (0, (init_p, [], pop)) in
  taken, left

(* select members from one run over the population, at uniform prob *)
let select_random_prob prob pop =
  List.fold_left  (* loop over the population *)
    (fun (t, r) tree ->
      if roll_f prob
      then tree::t, r
      else t, tree::r
    )
    ([], [])
    pop

(* select one member from a list randomly *)
let random_select_one l = List.nth l @: Random.int @: List.length l

(* choose randomly from an array *)
let random_select_from_arr arr = arr.(Random.int @: Array.length arr)

(* choose a random subset of a list, with 1 being minimum *)
let random_subset l = 
  let to_take = 1 + (Random.int @: (List.length l) - 1) in
  fst @: select_random to_take l

