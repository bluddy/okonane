open Util

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

type last_report_t = int
type command_t = last_report_t * subtype_t

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

let execute shell args = function
  | EnvCommand -> (* display all contents of the environment *)
      check_arg_count args 0 0;
      print_string Environment.string_of_all_values shell.environment;
      shell
  | IterateCommand -> 
      check_arg_count args 0 1;
      check_world_map shell;


let getLongHelp c name = match c with
  | EnvCommand -> "Usage: "^name^"\n\n"^
    "Displays each environment variable and its current value."

let getShortHelp c name = match c with
  | EnvCommand -> "displays contents of environment variables"

