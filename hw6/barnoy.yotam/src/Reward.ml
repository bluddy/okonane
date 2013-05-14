(* file to deal with reward functions *)
open Util
open WorldMap
open State

(* we get -1 for non-finish, and 0 for finish *)
let get_reward world ((pos,_):state_t) = 
  if in_finish world pos then 0.
  else (-1.)


