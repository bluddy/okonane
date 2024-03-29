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
  let init_p = foi num /. foi total in
  let _, (_, taken, left) = 
    iterate_until (* keep looping until we have enough *)
      (fun (i, (prob, take, rest)) -> 
        (*Printf.printf "-take: %d, rest: %d\n" (List.length take) (List.length rest);*)
        if list_null rest then failwith "population too small" else
        foldl_until_fin  (* loop over the population *)
          (fun (j, (p, t, r)) x ->
              (*Printf.printf "j: %d\n" j;*)
              if roll_f p
              then  (* adjust total and prob *)
                let j' = j-1 in
                let total' = total - num + j' in
                if total' <= 0 then failwith "total num reached 0" else
                let p' = foi j' /. foi total' in
                   j',  (p', x::t, r)
              else j,   (p,  t, x::r)
          )
          (fun (j, _) _ -> j <= 0) (* stop condition *)
          (fun (j, (p, t, r)) xs -> j, (p, t, r@xs))
          (i, (prob, take, []))
          rest
      )
      (fun (i, _) -> i <= 0)
      (num, (init_p, [], pop)) in
  taken, left

(* select randomly with replacement *)
let select_random_replace_arr num pop_arr = 
  let len = Array.length pop_arr in
  snd @: iterate_until (fun (j,acc) ->
      let rand = Random.int len in
      let e = pop_arr.(rand) in
      j+1, e::acc)
    (fun (j,_) -> j >= num)
    (0, [])

(* select randomly with replacement *)
let select_random_replace num pop = 
  let pop_arr = Array.of_list pop in
  select_random_replace_arr num pop_arr

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

