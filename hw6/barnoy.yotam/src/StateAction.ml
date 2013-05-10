open Util
open State
open Actions
open Pos

type state_action_t = state_t * action_t
let string_of_st_act (st, act) = 
  string_of_state st^" "^string_of_action act

(* euclidean distance isn't good enough *)
let v_f p = mult_pair 3 p
let a_f p = mult_pair 15 p
let sam_distance 
  (((p1, v1), a1):state_action_t) (((p2, v2), a2):state_action_t) =
    let dp = Pos.dist_pos_no_root in
    foi @: dp p1 p2 + dp (v_f v1) (v_f v2) + dp (a_f a1) (a_f a2)

module StateActionMap = Map.Make(
  struct
    type t = state_action_t
    let compare = Pervasives.compare
  end)

module SAM = StateActionMap (* shortcut *)

(* lookup functions that nullify an unavailable value in StateActionMap *)
let sam_lookup_int key map = try SAM.find key map with Not_found -> 0
let sam_lookup_float key map = try SAM.find key map with Not_found -> 0.
let sam_lookup_m key map = 
  try Some (SAM.find key map) 
  with Not_found -> None

