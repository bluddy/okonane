(* Implements both learning agents *)
open Util
open Actions
open State
open SimTypes
open TransFunc
open WorldMap
open Reward

let tolerance = 0.000000001

module Value = struct
  type agent_t = {
      (* states in the world and their expected values *)
      expected_values : float StateMap.t;
      discount_factor : float;
      trans_fn : trans_fn_t;
      world : worldmap_t;
      conv_tolerance : float;
  }

  (* create a new value iterating agent *)
  let new_agent w t = 
    let velocities = 
      create_range min_velocity @: max_velocity - min_velocity + 1 in
    let x_y_vels = cross_product velocities velocities in
    let all_states = cross_product (all_positions w) x_y_vels in
    (* initialize the expected values to the rewards *)
    let m = List.fold_left 
      (fun acc state -> StateMap.add state (get_reward w state) acc)
      StateMap.empty
      all_states
    in {
      expected_values = m;
      discount_factor = 0.5;
      world = w;
      trans_fn = t;
      conv_tolerance = tolerance;
    }
end

module StateActionMap = Map.Make(
  struct
    type t = state_t * action_t
    let compare = Pervasives.compare
  end)

module Q = struct
  type agent_t = {
    min_explore_count : int; (* num of times to explore state-action pair *)
    discount_factor : float;
    learning_factor : float;
    conv_tolerance : float;
    max_change : float; (* max delta perception of env during iter *)
    visit_events : int StateActionMap.t;
    expected_rewards : float StateActionMap.t;
    sim : sim_t option; (* simulator of the environment *)
  }
  
  let new_agent () = { 
    min_explore_count = 1;
    discount_factor = 0.99;
    learning_factor = 0.5;
    conv_tolerance = tolerance;
    max_change=0.;
    visit_events = StateActionMap.empty;
    expected_rewards = StateActionMap.empty;
    sim = None;
  }
end

type agent_t = ValueIterAgent of Value.agent_t
             | QAgent of Q.agent_t

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
          sum +. (prob *. StateMap.find state state_exp_vals)
        )
        0. dest_states
    )
    possible_states

(* single update *)
let iterate agent = match agent with
  | ValueIterAgent a -> 
      let w, trans_fn = a.Value.world, a.Value.trans_fn in
      let old_exp_vals = a.Value.expected_values in
      let conv, exp_vals = StateMap.fold 
        (fun state expected (conv, acc) ->
          (* the reward for the current state *)
          let reward = get_reward w state in 
          (* get all possible states we can go to, and probs *)
          let poss = get_possible_states w state trans_fn in
          (* calculate the expected value *)
          let max = snd @: get_max_exp_val old_exp_vals poss in
          let new_expected = reward +. a.Value.discount_factor *. max in
          let new_acc = StateMap.add state new_expected acc in
          (* check for convergence *)
          if (abs_float @: new_expected -. expected) > a.Value.conv_tolerance 
          then false, new_acc (* we haven't converged *)
          else conv,  new_acc (* we may have converged *)
        ) 
        old_exp_vals
        (true, StateMap.empty) (* assume convergence to begin with *)
      in
      conv, ValueIterAgent({a with Value.expected_values = exp_vals})

  | QAgent a -> true, agent


type policy_t = ValuePolicy of worldmap_t * float StateMap.t * trans_fn_t
          | QPolicy


(* get a policy from an agent *)
let get_policy = function
  | ValueIterAgent a -> 
      ValuePolicy(a.Value.world, a.Value.expected_values, a.Value.trans_fn)
  | QAgent a -> QPolicy

(* choose an action based on the current state and a policy *)
let decide_action policy state : action_t = match policy with
  | ValuePolicy(w, exp_vals, trans) ->
      let poss = get_possible_states w state trans in 
      fst @: fst @: get_max_exp_val exp_vals poss

  | QPolicy -> list_head legal_actions



