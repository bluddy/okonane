(* Actions that can be carried out by an agent *)
open Util

(* legal moves that can be performed *)
let legal_actions = 
  let r = create_range -1 3 in
  List.flatten @: 
    list_map (fun i -> list_map (fun j -> (i,j)) @: create_range -1 3) r

let string_of_action (x,y) = "["^soi x^soi y^"]"
