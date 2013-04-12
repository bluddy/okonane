open Util
open Entropy

let error s = prerr_endline s; exit 1

let debug = ref false
let use_gr = ref false
let file = ref ""
let k = ref 10 (* k-folding *)

let param_specs = Arg.align 
    [
        "-k", Arg.Set_int k, " Number of folds for k-folds";
        "-g", Arg.Set use_gr, " Use gain ratio";
        "-d", Arg.Set  debug, " Show debug information";
    ]

let usage_msg = ""

let parse_cmd_line () = Arg.parse param_specs (fun str -> file := str) ""

let main () = 
  parse_cmd_line ();
  if !file = "" then
    (Arg.usage param_specs usage_msg; error "\nNo input files specified");
  let d = load_data true "," !file in
  let results = k_fold !k d (Entropy.tree_of_data !debug !use_gr) test in
  print_results_all results

let _ = main ()

