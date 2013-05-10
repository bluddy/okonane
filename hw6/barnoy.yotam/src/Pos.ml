(* Everything position related *)
open Util

type pos_t = int * int

let string_of_pos (x,y) = soi x^","^soi y

let add_pair (a,b) (c,d) = (a+c,b+d)

let mult_pair x (a,b) = (x*a, x*b)

let add_pair_bound a b max = 
  let (c,d) = add_pair a b in
  abs_bound c max, abs_bound d max

let round_pos (x,y) = (round x, round y)

let dist_pos_no_root (x1,y1) (x2,y2) =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  dx * dx + dy * dy

let dist_pos a b = sqrt @: foi @: dist_pos_no_root a b

