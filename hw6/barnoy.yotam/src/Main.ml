open Util

let commands = [
  EnvCommand, ["e"; "env"];
  HelpCommand, ["h"; "help"; "?"];
  IterCommand, ["i"; "iterate"];
  LoadCommand, ["l"; "load"];
  LoadWorldCommand, ["map"];
  MakeCommand, ["make"];
  MetricsCommand, ["metrics"];
  QuitCommand, ["quit"];
  SaveCommand, ["save"];
  SetCommand, ["set"; "s"];
  SimulateCommand, ["sim"; "simulate"];
  VarHelpCommand, ["vhelp"]
]

let main () =
  let s = Shell.new_shell 79 in
  let s' = Shell.register s commands in
  let args = list_tail @: array_map id_fn Sys.argv in
  Shell.execute s' args



