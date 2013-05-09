open Util
open Agent
open Environment
open TransFunc

type metric_agent_t = {
    agent : agent_t;
    learning_iter : int;      (* number of iters performed so far *)
    time : float;             (* time in iters in ms *)
    converged : bool;
}

let new_metric_agent a = 
 {learning_iter = 0; time = 0.; converged = false; agent = a}

let new_agent world env typ = 
  new_metric_agent @:
    match typ with
    | true  -> (* vi *)
      let t_fn = new_trans_fn env.hard_crash in
      let a = Value.new_agent world t_fn in
      ValueIterAgent ({a with
        Value.conv_tolerance = env.epsilon;
        discount_factor = env.gamma;
      })

    | false -> (* q *)
      let a = Q.new_agent env.use_basis env.basis_max_dist env.basis_width in
      QAgent ({a with 
          Q.learning_factor = env.alpha;
          conv_tolerance = env.epsilon;
          discount_factor = env.gamma;
          annealing = env.annealing;
          min_explore_count = env.min_explore;
        })

let set_simulator metric s = 
  let agent = match metric.agent with
    | QAgent(a) -> QAgent({a with Q.sim = Some s})
    | x -> x (* can't set it for this *)
  in
  {metric with agent = agent}

let get_policy a = Agent.get_policy a.agent

let iterate a = 
  let time = time_millis () in
  let conv, agent = Agent.iterate a.agent in
  let delta = time_millis () -. time in
  let total = a.time +. delta in
  let iter = a.learning_iter + 1 in
  conv, {agent=agent; time=total; learning_iter = iter; converged = conv}

let dump_agent a = Agent.dump_agent a.agent

