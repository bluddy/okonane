open Util
open Entropy

let error s = prerr_endline s; exit 1

let use_gr = ref false
let use_chi = ref false
let chi_filter = ref false
let file = ref ""
let k = ref 8 (* k-folding *)
let print_tree = ref false

let param_specs = Arg.align 
    [
        "-k", Arg.Set_int k, " Number of folds for k-folds (default: 8)";
        "-g", Arg.Set use_gr, " Use gain ratio (default: no)";
        "-t", Arg.Set print_tree, " Print the tree(s)";
        "-d", Arg.Set debug, " Show debug information (default: no)";
        "-c", Arg.Set use_chi, " Use chi-square tests (default: no)";
    ]

let usage_msg = ""

let parse_cmd_line () = Arg.parse param_specs (fun str -> file := str) ""

let main () = 
  parse_cmd_line ();
  if !file = "" then
    (Arg.usage param_specs usage_msg; error "\nNo input files specified");
  let d = Data.load_data "," !file in
  let results = Test.k_fold !k d 
      (Entropy.tree_of_data !use_gr !use_chi) Test.test in
  Test.print_results_all !print_tree results

let _ = main ()

