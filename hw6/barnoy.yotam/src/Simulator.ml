open Util
open Pos
open State
open Actions
open SimTypes
open WorldMap
open Environment
open Thread


type sim_step_t = {
  state : state_t;         (* state agent was in *)
  action : action_t;       (* decision agent made at this step *)
  result_state : state_t;  (* state agent reached as result of action *)
  before_score : float;    (* total score for agent before taking action *)
  after_score : float;
}


(* retrieves a string representing a full simulation step *)
let render_simulation_step sim step last =
  let world = sim.world in
  let str = string_of_mapdata world.data in
  let lines = string_lines str in
  let min_rows = 5 in
  let cols, rows = fst world.size, snd world.size in
  let lines' = 
    if rows < min_rows then 
      (* make a blank line *)
      let line = iterate (fun a -> a^" ") "" cols in
      iterate (fun acc -> acc@[line]) lines (min_rows - rows)
    else lines in
  let get_line = List.nth lines' in (* curry *)
  let pos_line = 
    let l = get_line 0 in
    l^"         Position: "^string_of_pos @: fst step.state in
  let vel_line =
    let l = get_line 1 in
    l^"         Velocity: "^string_of_pos @: snd step.state in
  let acc_line = 
    let l = get_line 2 in
    if last then l else
    l^"         Accel:    "^string_of_pos @: step.action in
  let score_line = 
    let l = get_line 3 in
    l^"         Score:    "^sof @: step.before_score in
  let lines = pos_line::vel_line::acc_line::score_line::(list_drop 4 lines') in
  let lines' = map_str_write lines (fst step.state) "@" in
  string_unlines lines'^"\n"

let log_message (sim:sim_t) step last = 
  let delta = (sim.last_display +. foi sim.output_delay) -. time_millis () in
  if delta > 0. then Thread.delay (delta /. 1000.);
  let s = render_simulation_step sim step last in
  print_endline s;
  {sim with last_display = time_millis ()}

let execute_transition world trans_fn state action =
  let outcomes = TransFunc.transition world trans_fn state action in
  if list_null outcomes then failwith "Malformed probability model!" else
  let prob = Random.float 1. in
  let result_state = snd @: 
    foldl_until 
      (fun (accp,_) (x,p) -> 
        if accp <= p then Left (accp,x)
        else Right(accp -. p, x))
      (prob, fst @: list_head outcomes)
      outcomes in
  result_state, Reward.get_reward world result_state

(* runs a sim. takes policy, returns simulation steps in reverse order *)
let simulate sim policy : sim_step_t list =
  let world, trans_fn = sim.world, sim.trans_fn in
  let start_state = get_random_start_state world in
  let score, history = 
    let rec loop (score, ((pos,_) as state), history, sim, policy) =
      if in_finish world pos then 
        if sim.verbose then 
          (* dummy step *)
          let step = {state=state; action=list_head legal_actions; 
            result_state=state; before_score = score; after_score=score} in
          ignore(log_message sim step true); score, history
        else score, history
      else
        let action, policy' = Policy.decide_action policy state in
        let new_state, score' = execute_transition world trans_fn state action in
        let new_score = score +. score' in
        let step = {state=state; action=action; result_state=new_state;
          before_score = score; after_score=new_score} in
        let sim' = if sim.verbose then log_message sim step false else sim in
        loop (new_score, new_state, step::history, sim', policy')
    in
    loop (0., start_state, [], sim, policy) in
  history



  

  

