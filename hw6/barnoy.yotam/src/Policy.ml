(* Policy *)
open Util
open State
open TransFunc
open WorldMap
open Actions
open StateAction
module F = FunMap

type policy_t = ValuePolicy of worldmap_t * float StateMap.t * trans_fn_t
          | QPolicy of F.funmap_t * int SAM.t * int

(* get all possible states we can go to *)
let get_possible_states world start_state trans_fn =
  List.rev_map (fun action ->
      action, transition world trans_fn start_state action)
    legal_actions

(* get the maximum expected value from all possible transition states *)
let get_max_exp_val state_exp_vals possible_states = 
  list_max 
    (fun (_, dest_states) ->
      List.fold_left (fun sum (state, prob) ->
          (* multiply the prob by the state's exp val *)
          let v = try
                    StateMap.find state state_exp_vals
                  with Not_found -> failwith "Incomplete state map"
          in
          sum +. (prob *. v)
        )
        0. dest_states
    )
    possible_states

(* choose an action based on the current state and a policy *)
let decide_action policy state : action_t = match policy with
  | ValuePolicy(w, exp_vals, trans) ->
      let poss = get_possible_states w state trans in 
      fst @: fst @: get_max_exp_val exp_vals poss

  | QPolicy(exp_reward, visited, min_explore) -> 
      (* first see if we have less than the min number of visits *)
      let action_by_visit, num = 
        list_min (fun action -> sam_lookup_int (state,action) visited)
          legal_actions in
      if num < min_explore then action_by_visit
      else (* now go by best expectation *)
        fst @: list_max 
          (fun action -> F.lookup_val (state,action) exp_reward)
          legal_actions

      

