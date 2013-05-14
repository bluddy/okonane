(* Policy *)
open Util
open State
open TransFunc
open WorldMap
open Actions
open StateAction
module F = FunMap
module R = MyRandom

type policy_t = ValuePolicy of worldmap_t * float StateMap.t * trans_fn_t
          | QLearnPolicy of F.funmap_t * int SAM.t * int
          | QPolicy of F.funmap_t 

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
                  with Not_found -> failwith @: 
                    "Incomplete state map: "^string_of_state state
          in
          sum +. (prob *. v)
        )
        0. dest_states
    )
    possible_states

(* increment the visit for the policy *)
let inc_visit st_act v = 
    let num_visits = sam_lookup_int st_act v in
    SAM.add st_act (num_visits+1) v

(* go by best expectation *)
let select_by_exp exp_rewards state poss_actions =
  let actions, value = list_max_set
    (fun action -> F.lookup_val (state,action) exp_rewards)
    poss_actions in
  MyRandom.random_select_one actions

(* go by best expectation. allow only values we have *)
(* assumes we have at least one action in the map *)
let select_by_exp_m exp_rewards state poss_actions =
  let actions, value = List.fold_left
    (fun (acc, max) action -> 
      match F.lookup_val_m (state,action) exp_rewards with
      | Some x when x > max -> [action], x
      | Some x when x = max -> action::acc, x
      | _ -> acc, max)
    ([], min_float)
    poss_actions in
  MyRandom.random_select_one actions

(* choose an action based on the current state and a policy *)
let decide_action policy state = match policy with
  | ValuePolicy(w, exp_vals, trans) ->
      let poss = get_possible_states w state trans in 
      fst @: fst @: get_max_exp_val exp_vals poss, policy

  | QLearnPolicy(exp, visited, min_explore) -> 
      let inc_update st_act = 
        QLearnPolicy(exp, inc_visit st_act visited, min_explore) in

      (* first see if we have less than the min number of visits *)
      let by_visit, num = 
        list_min_set (fun action -> sam_lookup_int (state,action) visited)
          legal_actions 
      in
      (* use random rolls to make sure we don't get stuck 
       * in a min explore loop * *)
      if num < min_explore && R.roll_f 0.8 then
        let action = MyRandom.random_select_one by_visit in
        action, inc_update (state, action) 
      else if R.roll_f 0.9 then
        let action = select_by_exp exp state legal_actions in
        action, inc_update (state, action) 
      else 
        let action = R.random_select_one legal_actions in
        action, inc_update (state, action) 

  | QPolicy exp -> select_by_exp exp state legal_actions, policy



