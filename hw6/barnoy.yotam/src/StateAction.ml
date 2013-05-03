open Util
open State
open Actions

type state_action_t = state_t * action_t

let sam_distance 
  (((p1, v1), a1):state_action_t) (((p2, v2), a2):state_action_t) =
    let dp = Pos.dist_pos_no_root in
    foi @: dp p1 p2 + dp v1 v2 + dp a1 a2

module StateActionMap = Map.Make(
  struct
    type t = state_action_t
    let compare = Pervasives.compare
  end)

module SAM = StateActionMap (* shortcut *)

(* lookup functions that nullify an unavailable value in StateActionMap *)
let sam_lookup_int key map = try SAM.find key map with Not_found -> 0
let sam_lookup_float key map = try SAM.find key map with Not_found -> 0.

