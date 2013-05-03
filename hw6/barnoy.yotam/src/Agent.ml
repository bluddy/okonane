(* Implements both learning agents *)
open Util
open Actions
open State
open Simulator
open SimTypes
open TransFunc
open WorldMap
open Reward
open Policy

let tolerance = 0.000000001

(* list of all possible velocity combinations *)
let x_y_velocities () = 
  let vels = create_range min_velocity @: max_velocity - min_velocity + 1 in
  cartesian_product vels vels

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
    let all_states = cartesian_product (all_positions w) (x_y_velocities ()) in
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

module Q = struct
  type agent_t = {
    min_explore_count : int;    (* num of times to explore state-action pair *)
    discount_factor : float;
    learning_factor : float;
    annealing : int;            (* how much to anneal by *)
    conv_tolerance : float;
    max_change : float;         (* tracks max delta in our perception of env during iter *)
    visit_events : int SAM.t;
    expected_rewards : float SAM.t;
    sim : sim_t;                (* simulator of the environment *)
  }
  
  (* Create a new Q agent *)
  let new_agent w t = 
    { 
      min_explore_count = 1;
      discount_factor = 0.99;
      annealing = 0;
      learning_factor = 0.5;
      conv_tolerance = tolerance;
      max_change=0.;
      visit_events = SAM.empty;
      expected_rewards = SAM.empty;
      sim = {SimTypes.world = w; trans_fn = t; verbose = false; 
        output_delay = 0; last_display = 0.}
    }
end

type agent_t = ValueIterAgent of Value.agent_t
             | QAgent of Q.agent_t


(* get a policy from an agent *)
let get_policy = function
  | ValueIterAgent a -> 
      ValuePolicy(a.Value.world, a.Value.expected_values, a.Value.trans_fn)
  | QAgent a -> 
      QPolicy(a.Q.expected_rewards, a.Q.visit_events, a.Q.min_explore_count)

(* if we're using annealing, annealing must be > 1. 
 * Otherwise, we use a constant *)
let get_learn_factor const_learn_fact annealing num_visits =
  match annealing with
  | 0 | 1  -> const_learn_fact (* constant *)
  | an     -> 
      const_learn_fact *. (foi an /. (foi (an - 1) +. foi num_visits))

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

  | QAgent a -> 
      let exp_rs, visits = a.Q.expected_rewards, a.Q.visit_events in
      let learn_fac, discount_fac = a.Q.learning_factor, a.Q.discount_factor in
      let anneal = a.Q.annealing in
      let policy = get_policy agent in
      let history = simulate a.Q.sim policy in
      let max_delta, exp_rs, visits = List.fold_left 
        (fun (max_delta, acc_vals, acc_visits) step ->
          let st, act, r_st = step.state, step.action, step.result_state in
          let reward = step.after_score -. step.before_score in
          (* find the score of the max action for the result state *)
          let max_next = snd @: list_max
            (fun action -> sam_lookup_float (r_st, action) acc_vals) 
            legal_actions
          in
          let cur_val = sam_lookup_float (st, act) acc_vals in
          let learning = reward +. (discount_fac *. max_next) in
          let num_visit = sam_lookup_int (st, act) acc_visits in
          let alpha = get_learn_factor learn_fac anneal num_visits in
          let exp_val = (1. -. alpha) *. cur_val +. alpha *. learning in
          let exp_rs' = SAM.add (st,act) exp_val acc_vals in
          let visits' = SAM.add (st, act) (num_visit+1) visits in
          let delta = abs_float @: exp_val -. cur_val in
          let new_max_d = if delta > max_delta
                          then delta else max_delta in
          (new_max_d , exp_rs', visits')
        )
        (0., exp_rs, visits)
        history
    in
    let conv = if max_delta <= a.Q.conv_tolerance then true else false in
    conv, QAgent({ a with 
            Q.max_change = max_delta; 
            expected_rewards = exp_rs;
            visit_events = visits
          })


