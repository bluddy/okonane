open Util
open Agent

type metric_agent_t = {
    agent : agent_t;
    learning_iter : int;      (* number of iters performed so far *)
    time : int64;             (* time in iters in ms *)
    converged : bool;
}

let new_metric_agent a = 
  MetricAgent({learning_iter = 0; time = 0; converged = false}, a)

let set_simulator metric s = 
  let agent = match metric.agent with
    | QAgent(a) -> QAgent({a with simulator = s})
    | x -> x (* can't set it for this *)
  in
  {metric with agent = agent}


