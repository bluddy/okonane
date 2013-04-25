(* file to deal with reward functions *)
open Util

type reward_fn_t = worldmap_t

(* we get -1 for non-finish, and 0 for finish *)
let get_reward reward_fn pos = 
  if in_start_finish reward_fn.pos_finish pos then 0.
  else (-1.)


