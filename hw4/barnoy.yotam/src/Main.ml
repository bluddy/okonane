open Util
open Data
open Tree

let error s = prerr_endline s; exit 1

let debug = ref false
let train_file = ref ""
let test_file = ref ""

let param_specs = Arg.align 
    [
        "-d", Arg.Set  debug, " Show debug information";
        "-t", Arg.Set_string train_file, " File with which to train";
        "-e", Arg.Set_string test_file, " File with which to test";
    ]

let parse_cmd_line () = Arg.parse param_specs (fun str -> ()) ""

let usage_msg = ""

let main () = 
  parse_cmd_line ();
  if !train_file = "" then
    (Arg.usage param_specs usage_msg; error "\nNo input files specified");
  let d = load_data true "," !train_file in
  let t = tree_of_data ~print:true d in
  let s = string_of_tree t in
  print_string s

let _ = main ()

