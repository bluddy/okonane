(* Reperesents the state of the agent in the world *)
open Util
open Pos

type state_t = pos_t * pos_t (* pos, velocity *)

let string_of_state_t ((p,v):state_t) =
  "[p="^string_of_pos p^";v="^string_of_pos v^"]"

module StateMap = Map.Make(
  struct 
    type t = state_t
    let compare = Pervasives.compare
  end)

