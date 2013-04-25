(* Implements all types of agents *)
open Util

module Metric = struct
  type agent_t = {
      learning_iter : int;      (* number of iters performed so far *)
      time : int64;             (* time in iters in ms *)
      converged : bool;
  }
end

module Value = struct
  type agent_t = {
      (* states in the world and their expected values *)
      expected_values : float state_map_t;
      world : worldmap_t option;
      discount_factor : float;
      transition_fn : transition_fn_t option;
      reward_fn : reward_fn_t option;
      conv_tolerance : float;
  }
end

module Q = struct
  type agent_t = {
    min_explore_count : int; (* num of times to explore state-action pair *)
    discount_factor : float;
    learning_factor : float;
    conv_tolerance : float;
    max_change : float; (* max delta perception of env during iter *)
    visit_events : int state_action_map_t;
    expected_rewards : float state_action_map_t;
    simulator : simulator_t; (* simulator of the environment *)
  }
end

type agent_t = MetricAgent of Metric.agent_t * agent_t
             | ValueIterAgent of Value.agent_t
             | QAgent of Q.agent_t

let new_metric_agent a = 
  MetricAgent({learning_iter = 0; time = 0; converged = false}, a)

(* functions for metric agent only *)
let metric_only f = function MetricAgent(a,t) -> Some(f a t) | _ -> None
let get_time = metric_only (fun a _ -> a.time)
let get_iter_count = metric_only (fun a _ -> a.learning_iter)
let is_converged = metric_only (fun a _ -> a.converged)

let set_simulator s = function
  | MetricAgent(a,t) -> MetricAgent(a, set_simulator s t)
  | QAgent(a) -> QAgent({a with simulator = s})
  | x -> x (* can't set it for this *)

                  

(* single update *)
let iterate agent =
