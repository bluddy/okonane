open Util
open Board
open Game

let debug = ref false
let silent = ref false

let param_specs = Arg.align 
    [
        "-d", Arg.Set  debug, " Show debug information";
        "-s", Arg.Set  silent, " Silent mode (batch games)"; 
    ]

let parse_cmd_line () = Arg.parse param_specs (fun str -> ()) ""

let main () = 
  parse_cmd_line ();
  Game.set_debug !debug;
  Random.self_init ();
  start_game ()

let _ = main ()

