open Util
open Board
open Game

let debug = ref false

let param_specs = Arg.align 
    [
        "-d", Arg.Set  debug, " Show debug information";
    ]

let parse_cmd_line () = Arg.parse param_specs (fun str -> ()) ""

let main () = 
  parse_cmd_line ();
  set_debug !debug;
  start_game ()

let _ = main ()

