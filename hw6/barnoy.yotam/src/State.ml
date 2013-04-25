(* Reperesents the state of the agent in the world *)
open Util
open Pos

type state_t = pair_t * pair_t (* pos, velocity *)

let string_of_state_t ((p,v):state_t) =
  "[p="^string_of_pos p^";v="^string_of_pos v^"]"

