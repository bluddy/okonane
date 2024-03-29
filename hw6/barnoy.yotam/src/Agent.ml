(* Implements both learning agents *)
open Util
open Actions
open State
open StateAction
open Simulator
open SimTypes
open TransFunc
open WorldMap
open Reward
open Policy
module P = Printf

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
    (*List.iter (fun s -> P.printf "%s \n" (string_of_state s)) all_states;*)
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
    expected_rewards : FunMap.funmap_t;
    sim : sim_t option;         (* simulator of the environment *)
  }
  
  (* Create a new Q agent *)
  let new_agent use_basis basis_max_dist basis_width = 
    { 
      min_explore_count = 1;
      discount_factor = 0.99;
      annealing = 0;
      learning_factor = 0.5;
      conv_tolerance = tolerance;
      max_change=0.;
      visit_events = SAM.empty;
      expected_rewards = 
        FunMap.new_funmap use_basis basis_max_dist basis_width;
      sim = None   
    }
end

type agent_t = ValueIterAgent of Value.agent_t
             | QAgent of Q.agent_t


(* get a policy from an agent *)
let get_policy ?(learn=true) = function
  | ValueIterAgent a -> 
      ValuePolicy(a.Value.world, a.Value.expected_values, a.Value.trans_fn)
  | QAgent a -> 
      if learn then QLearnPolicy(
          a.Q.expected_rewards, a.Q.visit_events, a.Q.min_explore_count)
      else QPolicy(a.Q.expected_rewards)


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

          (* debug *)
          (*List.iter (fun (action, l) ->*)
            (*P.printf "state: %s, action: %s\n"*)
            (*(string_of_state state) (string_of_action action);*)
              (*List.iter (fun (st, p) -> P.printf "pos_state: %s, prob: %f\n"*)
                (*(string_of_state st) p) l;) poss;*)

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
      let sim = unwrap_maybe a.Q.sim in
      let exp_vals, visits = a.Q.expected_rewards, a.Q.visit_events in
      let learn_fac, discount_fac = a.Q.learning_factor, a.Q.discount_factor in
      let anneal = a.Q.annealing in
      let policy = get_policy ~learn:true agent in
      let history = simulate sim policy in

      let max_delta, exp_vals, visits = List.fold_left 
        (fun (max_delta, acc_vals, acc_visits) step ->
          let st, act, r_st = step.state, step.action, step.result_state in
          let reward = step.after_score -. step.before_score in
          (* find the score of the max action for the result state *)
          let max_next = List.fold_left
            (fun max action -> 
              match FunMap.lookup_val_m (r_st, action) acc_vals, max with
              | None, _        -> max
              | Some x, Some m -> if x > m then Some x else Some m
              | Some x, None   -> Some x)
            None legal_actions in
          let max_next = match max_next with None -> 0. | Some x -> x
          in
          let cur_val = FunMap.lookup_val (st, act) acc_vals in
          let learning = reward +. (discount_fac *. max_next) in
          let num_visits = sam_lookup_int (st, act) acc_visits in
          (* calculate a learning factor: either const or with annealing *)
          let alpha = get_learn_factor learn_fac anneal num_visits in
          let exp_vals' = 
            FunMap.update_val (st,act) alpha cur_val learning acc_vals in
          let visits' = SAM.add (st, act) (num_visits+1) acc_visits in
          let delta = abs_float @: learning -. cur_val in

          (* debug *)
          (*Printf.printf "r:%f, c:%f, l:%f, a:%f, v:%d\n" *)
            (*reward cur_val learning alpha num_visits;*)
          let new_max_d = if delta > max_delta
                          then delta else max_delta in
          (new_max_d , exp_vals', visits')
        )
        (0., exp_vals, visits)
        history
    in
    let conv = if max_delta <= a.Q.conv_tolerance then true else false in
    P.printf "conv: %f\n" max_delta;
    conv, QAgent({ a with 
            Q.max_change = max_delta; 
            expected_rewards = exp_vals;
            visit_events = visits
          })

let dump_agent = function
  | QAgent a ->
    FunMap.dump_funmap a.Q.visit_events a.Q.expected_rewards^
    P.sprintf "Max change: %.2f\n" a.Q.max_change

 | ValueIterAgent a ->
     StateMap.fold (fun s v acc -> acc^string_of_state s^":"^sof v^"\n")
     a.Value.expected_values
     ""
    

