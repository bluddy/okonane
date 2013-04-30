(* Randomness functions *)
open Util

(* initialize random generator *)
let _ = Random.self_init ()

(* carry out a random roll and compare it to a given probability *)
let roll_f prob = Random.float 1. < prob

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

