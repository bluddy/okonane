(* Randomness functions *)
open Util

(* initialize random generator *)
let _ = Random.self_init ()

(* carry out a random roll and compare it to a given probability *)
let roll_f prob = Random.float 1. < prob

(* randomness functions *)
(* select x members at random, assuming equal likelihood *)
let select_random num pop =
  let prob = (foi num) /. (foi pop) in
  snd @: iterate_until   (* keep looping until we have enough *)
    (fun (i, (take, rest)) -> 
      foldl_until  (* loop over the population *)
        (fun (j, (t, r)) tree ->
          if roll_f prob
          then (j+1, (tree::t, r))
          else (j,   (t, tree::r))
        )
        (fun (j, _) -> j >= num) (* stop condition *)
        (i, take, [])
        rest)
    (fun (i, _) -> i >= num)
    (0, [], pop)

(* select members from one run over the population, at uniform prob *)
let select_random_prob prob pop =
  snd @: List.fold_left  (* loop over the population *)
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

(* choose a random subset of a list *)
let random_subset l = 
  let len = List.length l in
  List.fold_left (fun acc x -> if roll_f 0.5 then x::acc else acc) [] l