open Util
open WorldMap
open Environment
open Simulator
open TransFunc
module P = Printf
module MA = MetricAgent

exception CommandFailure of string

(* commands *)
type subtype_t = EnvCommand
            (* | HelpCommand *)
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

module StringMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

(* shell *)
type shell_t = {
    command_map : command_t StringMap.t;
    (* registrations seen by this shell *)
    reg_set : (command_t * string list) list; 
    terminate : bool;
    env: env_t;
    world : worldmap_t option;
    agent : MA.metric_agent_t option;
    simulation : sim_step_t list;
}
    
let new_shell width =
  { command_map = StringMap.empty;
    reg_set = [];
    terminate = false;
    env = default_env;
    world = None;
    agent = None;
    simulation = [];
  }

let prompt = "RLASH:> "
    
(* expects a list of (command, namelist) *)
let register shell commands =
  (* flatten the commands to make them easier to add *)
  let cs = List.flatten @: list_map 
     (fun (c, ls) -> 
        list_map (fun x -> c,x) ls
     ) commands in
  let new_map = List.fold_left 
    (fun acc (c,s) -> StringMap.add s c acc) shell.command_map cs in
  {shell with command_map = new_map; reg_set = commands}

let check_arg_count args min max =
  let len = List.length args in
  if len < min || len > max then
    if min == max 
    then raise @: CommandFailure("This command expects "^soi min^" arguments")
    else raise @: CommandFailure(
      "This command expects "^soi min^" to "^soi max^" arguments.")
  else ()

let check_agent shell = match shell.agent with Some _ -> () 
  | None -> raise @: CommandFailure("No agent loaded.")

let check_world_map shell = match shell.world with Some _ -> () 
  | None -> raise @: CommandFailure("No worldmap loaded.")


(* General command utilities -------- *)
let new_command c = ref 0., c

let parse_pos_int s desc = 
  try let i = ios s in
      if i < 0 
      then raise @: CommandFailure("Invalid "^desc^": must be positive")
      else i
  with Failure x -> raise @: CommandFailure("Invalid "^desc^": not a positive integer")

(* generate progress report if timing is correct or if forced, and if active *)
let progress_report command shell force str =
  if shell.env.report_delay = 0 then ();
  let last_time = fst command in
  if time_millis () < !last_time +. foi shell.env.report_delay && not force 
  then ()
  else
    print_endline str; 
    last_time := time_millis ()

let build_sim shell =
  let env = shell.env in
  let trans_fn = TransFunc.new_trans_fn env.hard_crash in
  let sim = {SimTypes.world = unwrap_maybe shell.world; trans_fn = trans_fn; verbose = env.verbose_sim; 
    output_delay = env.verbose_sim_delay; last_display = 0.} in
  sim

(* Specific command types related stuff -------- *)

let execute_iterate command shell args =
  let iterations = (match args with
    | []      -> Some 1
    | "oo"::_ -> None
    | x::_    -> Some (parse_pos_int x "iteration count"))
  in
  let sim = build_sim shell in
  let a = MA.set_simulator (unwrap_maybe shell.agent) sim in

  (* Iterate until we run out of iteration or converge *)
  let agent = iterate_until
    (fun (i, converge, agent) -> let i' = i + 1 in
      match iterations with
      | _ when converge ->
          P.printf "Detected convergence at agent iteration %d.\n"
          agent.MA.learning_iter;
          Left agent

      | Some iter when i >= iter -> Left agent

      | None ->  (* infinity *)
          let conv, a = MA.iterate agent in
          progress_report 
            command shell false (P.sprintf "Iteration %d complete" i');
          Right(i', conv, a)
          
      | Some iter -> 
          let conv, a = MA.iterate agent in
          progress_report
            command shell (i' >= iter) 
            (P.sprintf "Iteration %d/%d (%2d%%) complete" i' iter
            (i' * 100 / iter));
          Right (i', conv, a))
    (0, false, a) in
  {shell with agent=Some agent}

(* executes a command and returns a new shell *)
let execute command shell args = match snd command with
  | EnvCommand -> (* display all contents of the environment *)
      check_arg_count args 0 0;
      print_string @: Environment.string_of_all_values shell.env;
      shell
  | IterateCommand -> 
      check_arg_count args 0 1;
      check_world_map shell;
      check_agent shell;
      execute_iterate command shell args

  | LoadCommand ->
      check_arg_count args 1 1;
      let file = list_head args in
      let data = try read_file file 
                 with Sys_error e -> raise @: CommandFailure e in
      let agent = (Marshal.from_string data 0 : MA.metric_agent_t) in
      {shell with agent = Some agent}

  | LoadWorldCommand ->
      check_arg_count args 1 1;
      let file = list_head args in
      let data = try read_file file 
                 with Sys_error e -> raise @: CommandFailure e in
      let world = try map_of_string data 
                  with Failure e -> raise @: CommandFailure e in
      {shell with world = Some world}

  | MakeCommand ->
      let env = shell.env in
      check_arg_count args 1 1;
      check_world_map shell;
      let agent_type = list_head args in
      let agent = MA.new_agent (unwrap_maybe shell.world) env
        begin match agent_type with
        | "vi" -> true | "q"  -> false | _ -> raise @: 
          CommandFailure ("unrecorgnized agent type "^agent_type)
        end in
        {shell with agent=Some agent}

  | MetricsCommand ->
      check_arg_count args 0 0;
      check_agent shell;
      let a = unwrap_maybe shell.agent in
      P.printf "Iterations: %d\n" a.MA.learning_iter;
      P.printf "Time: %f ms\n"    a.MA.time;
      P.printf "Converged? %s\n"  (if a.MA.converged then "yes" else "no");
      shell

  | QuitCommand ->
      check_arg_count args 0 0;
      { shell with terminate=true }

  | SaveCommand ->
      check_arg_count args 1 1;
      check_agent shell;
      let data = Marshal.to_string (unwrap_maybe shell.agent) [] in
      let file = list_head args in
      write_file file data;
      shell

  | SetCommand ->
      check_arg_count args 1 1;
      let set_str = list_head args in
      if not @: String.contains set_str '=' then 
        raise @: CommandFailure("The set expression must contain an equals sign.")
      else
        let i = String.index set_str '=' in
        let name = string_take i set_str in
        let value = string_drop (i+1) set_str in
        begin match Environment.get_setter name with
        | None    -> raise @: CommandFailure("Unrecognized variable "^name)
        | Some fn -> 
            try let new_env = fn shell.env value in
                         {shell with env=new_env}
            with Failure _ -> raise @: CommandFailure("Invalid format for "^name)
        end

  | SimulateCommand -> 
      check_arg_count args 0 1;
      check_world_map shell;
      check_agent shell;
      let sim_count = (match args with 
        | [x] -> parse_pos_int x "simulation count"
        | _   -> 1) in
      let sim = build_sim shell in
      let _, total_score, scores' = Util.iterate
        (fun (i, total_score, acc) -> 
          let policy = MA.get_policy (unwrap_maybe shell.agent) in
          let steps = simulate sim policy in
          let score = (list_head steps).after_score in
          if sim_count > 1 then
            progress_report command shell (i+1 = sim_count) 
              (P.sprintf "Simulation: %d/%d (%.2f%%) complete" (i+1) sim_count
                @: (foi @: i+1) /. (foi @: sim_count * 100));
          i+1, total_score +. score, score::acc)
        (0, 0.,[]) 
        sim_count in
      let scores = List.rev scores' in
      if sim_count > 1 then 
        (P.printf "Simulation scores: \n"; List.iter (P.printf "%.2f ") scores;
        P.printf 
        "\nAverage simulation score: %f\n" (total_score /. foi sim_count);
        shell)
      else (P.printf "Simulation score: %.2f\n" total_score; shell)

  | VarHelpCommand ->
      check_arg_count args 0 0;
      print_string 
       "alpha                  - Controls learning rate in RL agents.\n\
        epsilon                - Controls convergence tolerance in RL agents.\n\
        gamma                  - Controls discount factor in RL agents.\n\
        use_basis              - Use radial basis functions for Q agent.\n\
        basis_max_dist         - Maximum distance before planting another basis
        function\n\
        basis_width            - Width of each gaussian radial basis function\n\
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
        reports provided by some shell commands  in milliseconds.";
        shell

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

  | MetricsCommand ->
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
  | MetricsCommand    -> "displays metrics on the current agent"
  | QuitCommand      -> "terminates the shell"
  | SaveCommand      -> "saves the current agent"
  | SetCommand       -> "sets environment variable values"
  | SimulateCommand  -> "runs a simulation"
  | VarHelpCommand   -> "displays information about environment variables"

(* interpret the provided string *)
let rec interpret shell command_str =
  let m = shell.command_map in
  let parts = string_words command_str in
  match parts with
  | []   -> shell
  | c::args -> 
    match find StringMap.find c m with
    | None ->  (* shortcut to set *)
              if String.contains command_str '=' &&
                 not @: String.contains command_str ' ' 
              then interpret shell @: "set "^command_str
              else (P.printf "Unrecognized command: %s\n" c; shell)
    | Some command -> 
              try
                execute command shell args
              with CommandFailure e -> P.printf "%s\n" e; shell

(* execute shell. first execute init_str *)
let execute_sh shell init_str =
  let init_sh = foldl_until
    (fun sh str -> 
      if sh.terminate then Left sh
      else Right (interpret sh str))
    shell 
    init_str 
  in
  iterate_until
    (fun sh ->
      if sh.terminate then Left sh
      else
        (print_string prompt;
        let s = read_line () in
        if s = "" then Right sh
        else Right (interpret sh s)))
    init_sh
