open Util
module P = Printf

exception CommandFailure of string

type subtype_t = EnvCommand
            | HelpCommand
            | IterateCommand (* iterate through one step of learning *)
            | LoadCommand
            | LoadWorldCommand
            | MakeCommand
            | MetricsCommand
            | QuitCommand
            | SaveCommand
            | SetCommand
            | SimulateCommand
            | VarHelpCommand

type last_report_t = float
type command_t = last_report_t ref * subtype_t

(* General command utilities -------- *)
let new_command c = 0, c

let check_arg_count args min max =
  let len = List.length args in
  if len < min || len > max then
    if min == max 
    then raise @: CommandFailure("This command expects "^soi min^" arguments")
    else raise @: CommandFailure(
      "This command expects "^soi min^" to "^soi max" arguments.")
  else ()

let check_agent shell = match shell.agent with Some _ -> () 
  | None -> raise @: CommandFailure("No agent loaded.")

let check_world_map shell = match shell.worldmap with Some _ -> () 
  | None -> raise @: CommandFailure("No worldmap loaded.")

let parse_pos_int s desc = 
  try let i = soi s in
      if i < 0 
      then raise @: CommandFailure("Invalid "^desc^": must be positive")
      else i
  with Failure x -> CommandFailure("Invalid "^desc^": not a positive integer")

let build_sim shell =
  let env = shell.env in
  let trans_fn = new_trans_fn shell.worldmap shell.env.hard_crash in
  let sim = new_sim env.verbose_sim env.verbose_sim_delay trans_fn in
  sim

(* generate progress report if timing is correct or if forced, and if active *)
let progress_report command shell force form =
  if shell.env.report_delay = 0 then ();
  let last_time = fst command in
  if time_millis () < !last_time +. foi shell.env.report_delay && not force 
  then ()
  else
    P.printf form;
    last_time := time_millis ()

(* Specific command types related stuff -------- *)

let execute_iterate command shell args =
  let iterations = (match args with
    | []      -> Some 1
    | "oo"::_ -> None
    | x::_    -> Some (parse_pos_int x "iteration count"))
  in
  let sim = build_sim shell in

  (* TODO: simulator listeners *)

  let a = Agent.set_simulator sim shell.agent in

  (* Iterate until we run out of iteration or converge *)
  let _,_,agent = iterate_until
    (fun (i, converge, agent) ->
      let i' = i+1 in
      let conv, a = Agent.iterate agent in
      begin match iterations with
      | None (* infinity *) -> 
        fst command := progress_report 
          command shell false "Iteration %d complete\n" (i+1)
      | Some _ -> 
        fst command := progress_report
          command shell (i' >= iterations) 
          "Iteration %d/$d (%.2f) complete" i' iterations @:
          foi @: i' / iterations * 100
      end;
      i', conv, a)
    (fun (i, converge, agent)  ->
      if converge then 
        (P.printf 
          "Detected convergence at agent iteration %d.\n" agent.iter;
        true)
      else if i >= iterations then true
      else false)
    (0, false, a) in
  {shell with agent=agent}


let execute command shell args = function
  | EnvCommand -> (* display all contents of the environment *)
      check_arg_count shell args 0 0;
      print_string Environment.string_of_all_values shell.environment;
      shell
  | IterateCommand -> 
      check_arg_count args 0 1;
      check_world_map shell;
      check_agent shell;
      execute_iterate command shell args

  | LoadCommand ->
      check_arg_count shell args 1 1;
      let file = list_head args in
      let data = read_file file in
      let agent = (Marshal.from_string data : metric_agent_t) in
      {shell with agent = agent}

  | LoadWorldCommand ->
      check_arg_count shell args 1 1;
      let file = list_head args in
      let data = read_file file in
      let world = try map_of_string data 
                  with Failure e -> raise CommandFailure b in
      {shell with world = world}

  | MakeCommand ->
      let env = shell.env in
      let world = shell.world in
      check_arg_count shell args 1 1;
      check_world_map shell;
      let agent_type = list_head args in
      let agent = new_metric_agent @: begin match agent_type with
      | "vi" -> let t_fn = new_trans_fn world env.hard_crash in
        let a = Agent.Value.new_agent shell.world t_fn in
        ValueIterAgent {a with
          conv_tolerance = env.epsilon;
          discount_factor = env.gamma;
        }
      | "q"  -> let a = Agent.Q.new_agent () in
        QAgent {a with 
          learning_factor = env.alpha;
          conv_tolerance = env.epsilon;
          discount_factor = env.gamma;
          min_explore_count = env.min_explore;
        }
      | _ -> raise CommandFailure ("unrecorgnized agent type "^agent_type)
      end in
      {shell with agent=agent}

  | MetricsCommand ->
      let agent = shell.agent in
      check_arg_count shell args 0 0;
      check_agent shell;
      printf "Iterations: %d\n" agent.learning_iter;
      printf "Time: %d ms\n"    agent.time;
      printf "Converged? %s\n"  (if agent.converged then "yes" else "no");
      shell

  | QuitCommand ->
      check_arg_count shell args 0 0;
      { shell with terminate=true }

  | SaveCommand ->
      check_arg_count shell args 1 1;
      let data = Marshal.to_string shell.agent in
      let file = list_head args in
      write_file file data;
      shell

  | SetCommand ->
      check_arg_count shell args 1 1;
      let set_str = list_head args in
      if not String.contains set_str '=' then 
        raise CommandFailure("The set expression must contain an equals sign.")
      else
        let i = String.index set_str '=' in
        let name = string_take i set_str in
        let value = string_drop (i+1) set_str in
        begin match Environment.get_setter name with
        | None    -> raise CommandFailure("Unrecognized variable "^name)
        | Some fn -> 
            try let new_env = fn shell.env value in
                         {shell with env=new_env}
            with Failure _ -> raise CommandFailure("Invalid format for "^name)
        end

  | SimulateCommand -> 
      check_arg_count shell args 0 1;
      check_world_map shell;
      check_agent shell;
      let sim_count = (match list_head args with 
        | [x] -> parse_pos_int x "simulation count"
        | _   -> 1) in
      let sim = build_sim shell in
      let total_score, scores' = iterate
        (fun (total_score, acc) -> let policy = get_policy shell.agent in
          let steps = simulate sim policy in
          let score = (list_end steps).after_score in
          if sim_count > 1 then
            progress_report (i+1 = sim_count) 
              "Simulation: %d/%d (%.2f%%) complete" (i+1) sim_count 
              ((foi @: i+1) /. (foi @: sim_count * 100));
          total_score +. score, score::acc
        )
        (0.,[]) sim_count in
      let scores = List.rev scores' in
      if sim_count > 1 then 
        (printf "Simulation scores: \n" List.iter (printf "%.2f") scores;
        printf "Average simulation score: %f\n" (total_score /. foi sim_count))
      else
        printf "Simulation score: %f\n" total_score


  | VarHelpCommand ->
      check_arg_count args 0 0;
      print_string 
       "alpha                  - Controls learning rate in RL agents.\n\
        epsilon                - Controls convergence tolerance in RL agents.\n\
        gamma                  - Controls discount factor in RL agents.\n\
        minExplorations        - Encourages RL agents to explore by setting a \
        minimum on the number of times they will visit a given state before \
        optimizing for expected reward.\n\
        hardCrash              - If true, crashes reset the agent to the \
        starting line.  If false, crashes simply set agent to position of \
        collision with zero velocity.\n\
        verboseSimulation      - If true, each simulation time step \
        results in a printout of the world state.  If false, \
        simulation is quiet.\n\
        verboseSimulationDelay - The duration between displays of the \
        simulation state in milliseconds. This value has no impact \
        unless verboseSimulation is set.\n\
        progressReportDelay    - The minimum duration between progress \
        reports provided by some shell commands  in milliseconds."












let getLongHelp c name = match c with
  | EnvCommand -> "Usage: "^name^"\n\n"^
    "Displays each environment variable and its current value."

  | IterateCommand -> "Usage: "^name^" [cycles]\n\n"^
    "Iterates the loaded agent's learning algorithm.  If a number of cycles \
    is provided, it must be a positive integer; otherwise, one is assumed.  \
    If the algorithm converges, the remaining iterations are not performed.\
    \n\n\
    If the number of cycles provided is \"oo\" (which looks vaguely like an\
    infinity symbol if one squints just so), the algorithm is run until it \
    converges."

  | LoadCommand -> "Usage: "^ name^ " <pathname>\n\n"^
    "Loads a persisted agent from disk.  Note that the agent's world must be loaded separately."

  | LoadWorldCommand -> 
			"Usage: " ^ name ^ " <map filename>\n\n" ^ 
			"Loads a map.  The specified path indicates the location of the map file."

  | MakeCommand -> 
			"Usage: " ^ name ^ " <type>\n\n" ^
			"Creates a reinforcement learning agent using the defined environment variables.  Type may be one of " ^
			"\"vi\" for value-iterating or \"q\" for Q-learning.\n\n"^
			"NOTE: the agent will be configured with values from the environment WHEN IT IS CREATED.  Further " ^
			"changes to environment values after the agent has been created will have no effect unless the " ^ name ^
			" command is invoked again."

  | MetricCommand ->
			"Usage: " ^ name ^ "\n\n" ^
			"Displays metrics on the current agent.  Note that CPU time accuracy is dependent upon the accuracy of " ^
			"the underlying OS's system timer.  For Linux and Mac, this is typically 1ms.  For Windows, this is " ^
			"usually between 10ms and 15ms.\n\n" ^
			"Note that the \"time\" metric refers to wall time, not CPU time.  Thus, this value becomes " ^
			"uninteresting if the agent is iterated when verboseSimulation is set or any other factor separates wall " ^
			"and CPU time significantly."

  | QuitCommand -> "Terminates the shell"

  | SaveCommand      ->
		"Usage: " ^ name ^ " <pathname>\n\n"
				^ "Saves the agent in memory.  This operation persists the agent's knowledge base, allowing an agent which "
				^ "took significant amounts of time to generate to be preserved and recalled quickly."

  | SetCommand       ->
			"Usage: " ^ name ^ " <name>=<value>\n\n" ^
			"Assigns the provided value to the specified variable.  The name provided must be that of an existing " ^
			"variable; for a list of appropriate variables, see the env command."

  | SimulateCommand  -> 
			"Usage: " ^ name ^ " [count]\n\n" ^
			"Runs a simulation of the policy that the current agent is using.  If a count is specified, more than " ^
			"one simulation is executed."

  | VarHelpCommand   -> 
      "Usage: " ^ name ^ "\n\n" ^ "Displays help information for variables in the environment."


let getShortHelp c name = match c with
  | EnvCommand       -> "displays contents of environment variables"
  | IterateCommand   -> "iterates the agent's learning cycle"
  | LoadCommand      -> "loads a saved agent"
  | LoadWorldCommand -> "loads a saved world"
  | MakeCommand      -> "creates a reinforcement learning agent"
  | MetricCommand    -> "displays metrics on the current agent"
  | QuitCommand      -> "terminates the shell"
  | SaveCommand      -> "saves the current agent"
  | SetCommand       -> "sets environment variable values"
  | SimulateCommand  -> "runs a simulation"
  | VarHelpCommand   -> "displays information about environment variables"

