(* Implements both learning agents *)
open Util
open Actions
open State
open SimTypes
open TransFunc
open WorldMap

module Value = struct
  type agent_t = {
      (* states in the world and their expected values *)
      expected_values : float StateMap.t;
      discount_factor : float;
      trans_fn : trans_fn_t;
      world : worldmap_t;
      conv_tolerance : float;
  }

  let new_agent w t = {
    expected_values = StateMap.empty;
    discount_factor = 0.5;
    world = w;
    trans_fn = t;
    conv_tolerance = 0.000000001;
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
    conv_tolerance = 0.000000001;
    max_change=0.;
    visit_events = StateActionMap.empty;
    expected_rewards = StateActionMap.empty;
    sim = None;
  }
end

type policy_t = int

type agent_t = ValueIterAgent of Value.agent_t
             | QAgent of Q.agent_t

(* choose an action based on the current state and a policy *)
let decide_action policy state = list_head legal_actions

(* single update *)
let iterate agent = true, agent

(* get a policy from an agent *)
let get_policy a = 1


