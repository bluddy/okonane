open Util

type env_t = {
  epsilon : float;   (* for convergance tolerance *)
  alpha : float;     (* control learning rate *)
  gamma : float;     (* discount factor *)
  min_explore : int; (* min times to explore a state *)
  hard_crash : bool; (* whether hard crashing is used *)
  verbose_sim : bool; (* simulation causes printed updates *)
  verbose_sim_delay : int; (* delay time (millis) btw steps *)
  report_delay : int; (* approx interval for progress reports *)
}

let default_env = {
  epsilon = 0.;
  alpha = 0.5;
  gamma = 1.;
  min_explore = 1;
  hard_crash = false;
  verbose_sim = false;
  verbose_sim_delay = 0;
  report_delay = 0;
}

(* convert string to boolean *)
let bos s = match String.lowercase s with
  | "true" | "t"  -> true
  | "false" | "f" -> false
  | _ -> failwith "Not true or false"

(* string functions for getting/setting the env *)
let get_epsilon_s env = sof env.epsilon
let set_epsilon_s env s = {env with epsilon = fos s}
let get_alpha_s e = sof e.alpha
let set_alpha_s e s = {e with alpha = fos s}
let get_gamma_s e = sof e.gamma
let set_gamma_s e s = {e with gamma = fos s}
let get_min_explore_s e = soi e.min_explore
let set_min_explore_s e s = {e with min_explore = ios s}
let get_hard_crash_s e = sob e.hard_crash
let set_hard_crash_s e s = {e with hard_crash = bos s}
let get_verbose_sim_s e = sob e.verbose_sim
let set_verbose_sim_s e s = {e with verbose_sim = bos s}
let get_verbose_sim_delay_s e = soi e.verbose_sim_delay
let set_verbose_sim_delay_s e s = {e with verbose_sim_delay = ios s}
let get_report_delay_s e = soi e.report_delay
let set_report_delay_s e s = {e with report_delay = ios s}

let env_funcs = [
  "epsilon", (get_epsilon_s, set_epsilon_s); 
  "alpha", (get_alpha_s, set_alpha_s);
  "gamma", (get_gamma_s, set_gamma_s);
  "min_explore", (get_min_explore_s, set_min_explore_s);
  "hard_crash", (get_hard_crash_s, set_hard_crash_s);
  "verbose_sim", (get_verbose_sim_s, set_verbose_sim_s);
  "verbose_sim_delay", (get_verbose_sim_delay_s, set_verbose_sim_delay_s);
  "report_delay", (get_report_delay_s, set_report_delay_s);
]

let string_of_all_values e =
  List.fold_left (fun acc (str, (get, _)) -> 
    acc^str^" = "^get e^"\n") "" env_funcs

let get_setter n = try Some(snd @: List.assoc n env_funcs) with Not_found -> None
let get_getter n = try Some(fst @: List.assoc n env_funcs) with Not_found -> None



  
