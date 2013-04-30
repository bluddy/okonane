(* file to deal with reward functions *)
open Util
open WorldMap

(* we get -1 for non-finish, and 0 for finish *)
let get_reward world (pos,_) = 
  if in_start_finish world.pos_finish pos then 0.
  else (-1.)


