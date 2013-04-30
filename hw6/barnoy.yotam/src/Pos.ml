(* Everything position related *)
open Util

type pos_t = int * int

let string_of_pos (x,y) = soi x^","^soi y

let add_pair (a,b) (c,d) = (a+c,b+d)

let add_pair_bound a b max = 
  let (c,d) = add_pair a b in
  abs_bound c max, abs_bound d max

let round_pos (x,y) = (iof x, iof y)


