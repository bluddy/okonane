open Util
open Shell

let commands = [
  new_command EnvCommand, ["e"; "env"];
  (*(new_command HelpCommand), ["h"; "help"; "?"];*)
  new_command IterateCommand, ["i"; "iterate"];
  new_command LoadCommand, ["l"; "load"];
  new_command LoadWorldCommand, ["map"];
  new_command MakeCommand, ["make"];
  new_command MetricsCommand, ["metrics"];
  new_command QuitCommand, ["quit"];
  new_command SaveCommand, ["save"];
  new_command SetCommand, ["set"; "s"];
  new_command SimulateCommand, ["sim"; "simulate"];
  new_command VarHelpCommand, ["vhelp"]
]

let main () =
  let s = Shell.new_shell 79 in
  let s' = Shell.register s commands in
  let args = list_tail @: array_map id_fn Sys.argv in
  ignore(Shell.execute_sh s' args)

let _ = if !Sys.interactive then () else main ()


