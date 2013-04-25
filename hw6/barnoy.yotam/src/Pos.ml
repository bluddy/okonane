open Util

type pos_t = int * int

let string_of_pos (x,y) = soi x^","^soi y

let add_pair (a,b) (c,d) = (a+c,b+d)

let add_pair_bound a b = 
  let (c,d) = add_pair a b in
  abs_bound c, abs_bound d

let round_pos (x,y) = (round x, round y)
