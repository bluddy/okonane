(* Actions that can be carried out by an agent *)
open Util

type action_t = int * int

(* legal moves that can be performed *)
let legal_actions : action_t list = 
  let r = create_range (-1) 3 in
  List.flatten @: 
    list_map (fun i -> list_map (fun j -> (i,j)) r) r

let string_of_action (x,y) = "[a:"^soi x^","^soi y^"]"
