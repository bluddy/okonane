open Util
open Agent

type metric_agent_t = {
    agent : agent_t;
    learning_iter : int;      (* number of iters performed so far *)
    time : int64;             (* time in iters in ms *)
    converged : bool;
}

let new_metric_agent a = 
 {learning_iter = 0; time = Int64.of_int 0; converged = false; agent = a}

let set_simulator metric s = 
  let agent = match metric.agent with
    | QAgent(a) -> QAgent({a with Q.sim = Some s})
    | x -> x (* can't set it for this *)
  in
  {metric with agent = agent}

let get_policy a = Agent.get_policy a.agent


