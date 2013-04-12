open Util
open Data
open Test
open Genetic
open MainCommon

let error s = prerr_endline s; exit 1

let debug = ref false
let file = ref ""
let k = ref 10 (* k-folding *)
let params = ref default_params

(* gets the option of any option with an assoc list *)
let get_opt opts s = 
  try Some(List.assoc s)
  with Not_found -> None

(* sets an option of a list *)
let set_optl opts s fn =
  match get_opt opts s with
   | None -> print_endline @: s^" option not found"
   | Some x -> params := fn !params x

(* sets an option for a value *)
let set_optv x fn = params := fn !params x

(* setters for our options *)
let set_build_p f = set_optv f @: fun p x -> {p with build_p = x}

let set_fitness_fn s = set_optl fitness_opts s @:
  fun p x -> {p with fitness_fn = x}

let set_filter_p f = set_optv f @: fun p x -> {p with filter_p = x}

let set_pop_size i = set_optv i @: fun p x -> {p with pop_size = x}

let set_selection_fn s = set_optl selection_opts s @:
  fun p x -> {p with selection_fn = x}

let set_t_size i = set_optv i @: fun p x -> 
  {p with tournament_size = x}

let set_t_winners i = set_optv i @: fun p x -> 
  {p with tournament_winners = x}

let set_mutation_p f = set_optv f @: fun p x -> {p with mutation_p = x}

let set_crossover_p f = set_optv f @: fun p x -> {p with crossover_p = x}

let set_replacement_fn s = set_optl replacement_opts s @:
  fun p x -> {p with replacement_fn = x}

let set_generations i = set_optv i @: fun p x ->
  {p with termination = Generations x}

let set_delta f = set_optv f @: fun p x ->
  match p.termination with
  | Delta(_,g) -> {p with termination = Delta(x,g)}
  | _ -> {p with termination = Delta(x,default_delta_gen)}

let set_delta_gen i = set_optv i @: fun p x ->
  match p.termination with
  | Delta(d,_) -> {p with termination = Delta(d,x)}
  | _ -> {p with termination = Delta(default_delta,x)}

let param_specs = Arg.align 
    [
        "-k", Arg.Set_int k, " Number of folds for k-folds (default: 10)";
        "-d", Arg.Set  debug, " Show debug information";
        "-size", Arg.Int set_pop_size, " Population Size (default: 1000) ";
        "-pbuild", Arg.Float set_build_p, " Initial tree building probability (default: 0.3)";
        "-fit", Arg.String set_fitness_fn, " Fitness function (default: precision)";
        "-filter", Arg.Float set_filter_p, " Data filter percentage (default: 1.0)";
        "-select", Arg.String set_selection_fn, " Selection function (default: fitprop)";
        "-pmut", Arg.Float set_mutation_p, " Mutation probability (default: 0.01) ";
        "-pcross", Arg.Float set_crossover_p, " Cross-over probability (default: 0.4)";
        "-replace", Arg.String set_replacement_fn, " Replacement function (default: replacement)";
        "-gen", Arg.Int set_generations, " Termination criterion: number of generations (default:no)  ";
        "-delta", Arg.Float set_delta, " Termination criterion: minimum delta to continue (default: 0.01)";
        "-deltagen", Arg.Int set_delta_gen, " Termination criterion: num of generations to wait for delta (default: 100)";
        "-tsize", Arg.Int set_t_size, " Tournament size (default: 40)";
        "-twin", Arg.Int set_t_win, " Tournament winners (default: 5)";
    ]

let usage_msg = 
  let disp opts = String.concat "\n" @: List.map fst opts in
  "genetic file [parameters]"^
     "\nFitness functions:\n"^ disp fitness_opts^
     "\nSelection functions:\n"^disp selection_opts^
     "\nReplacement functions:\n"^disp replacement_opts^
     "\nTermination criteria:\n\
       either use -gen and number of generations\n\
       or use e.g. -delta 0.01 -deltagen 100 to determine the minimum delta\
       before terminating"

let parse_cmd_line () = Arg.parse param_specs (fun str -> file := str) usage_msg

let main () = 
  parse_cmd_line ();
  if !file = "" then
    (Arg.usage param_specs usage_msg; error "\nNo input files specified");
  let d = load_data true "," !file in
  let results = k_fold !k d (genetic_run !debug) test in
  print_results_all results

let _ = main ()

