open Util
open Data
open Tree

(* initialize random generator *)
let _ = Random.self_init ()

type selection_t = FitnessProp | RankBased | Tournament
type replacement_t = Elitism | Replacement
type fitness_t = Precision | Recall | PrecRecall 
type termination_t = Generations of int | Delta of float * int

type parameters_t = {
    build_p : float;       (* prob used to build initial trees *)
    fitness_f : fitness_t;
    fitness_p : float option; (* prob for filtering application of fitness *)
    pop_size : int;
    selection : selection_t;
    mutation_p : float;
    crossover_p : float;
    replacement : replacement_t;
    termination : termination_t;
  }

let default_params = {
    build_p = 0.3;
    fitness_fn = Precision;
    fitness_p = None;
    pop_size = 1000;
    selection = FitnessProp;
    mutation_p = 0.01;
    crossover_p = 0.4;
    replacement = Replacement;
    termination = Delta (0.01, 100);
}

(* carry out a random roll and compare it to a given probability *)
let roll_f prob = Random.float 1. < prob

(* convert a list of lists to an array matrix for random access *)
let matrix_of_lists l = 
  let l' = list_map Array.of_list l in
  Array.of_list l'

(* build random trees *)
(* p is the chance of making a node *)
let random_tree labels values p =
  let l_num, a_num = Array.length labels, Array.length values in
  let loop () : string tree_t = 
    let attr = Random.int a_num in (* random attribute *)
    let attr_vals = array_map (fun v ->
        if roll_f p then Node(v, loop ())
        else Leaf(v, labels.(Random.int l_num)))
      values.(attr) in
    attr, attr_vals
  in
  loop ()

(* build all the starting members of our population *)
let random_trees labels values pop_size p =
  build_list_from_index (fun _ -> random_tree labels values p) 0 pop_size

(* evaluate the fitness for all trees on the data, and return the trees with
 * normalized fitness values *)
let eval_fitness_all fitness_fn filter_p data trees =
  let labels = fst @: List.split data in
  (* optionally filter out some of the data *)
  let filtered_data = match filter_p with
    | Some p -> List.filter (roll_f p) data
    | None   -> data in
  (* for each tree, classify the data and apply the fitness function *)
  List.fold_left (fun acc tree ->
      let classes = list_map (fun datum -> classify tree datum) data in
      let fitness = fitness_fn tree @: list_zip labels classes in
      (Some fitness, tree)::acc
    ) 
    [] trees

(* normalize the fitness *)
(*let normalize_fitness trees = *)
  (*List.fold_left *)
    (*(fun acc (f, t) -> (f /. total_fitness, t)::acc)*)
    (*[] trees*)
  
(* start a run of the genetic algorithm *)
let genetic_run params data =
  let p = params in
  (* calculate the number of trees we need to keep and to recombine *)
  let keep_num = iof @: (1. -. p.crossover_p) *. (foi p.pop_size) in
  let breed_num = p.pop_size - keep_num in
  (* curry a shortcut *)
  let add_fitness = eval_fitness p.fitness_fn p.fitness_p data in
  let labels = Array.of_list @: uniq_labels data in
  let values = matrix_of_lists @: all_uniq_values data in
  (* populate our initial crop with random trees *)
  let st_pop = random_trees labels values p.pop_size in
  let start_pop = add_fitness st_pop
  in
  let loop pop gen old_fitness =
    (* get a group of trees that'll make it to the next round *)
    let selected_pop, rest = p.selection_fn keep_num pop in
    (* get a group of parents *)
    let parents, _ = p.selection_fn breed_num rest in
    let children = crossover parents in
    let new_gen = p.replacement_fn selected_pop parents children in
    let mutated = mutate new_gen in
    let new_gen_f = add_fitness mutated in
    (* do different things depending on our termination criterion *)
    match p.termination, old_fitness with
      | Generations(g), _ when gen >= g  -> best_fitness new_gen_f
      | Generations(g), _ -> loop new_gen_f (gen + 1) old_fitness
      (* if we're going by delta, check if enough gens have passes since a large
       * delta was seen. If so, stop *)
      | Delta(d, max_g), (old_g, old_f) -> 
          let best, best_tree = best_fitness new_gen_f in
          let delta = best - old_f in
          if delta > d 
          then loop new_gen_f (gen + 1) (gen, best)
          else (* delta small *)
            if gen - old_g >= max_g then best, best_tree (* we're done *)
            else loop new_gen_f (gen + 1) (old_g, old_f)
  in
  loop start_pop 1 (0,0.)
  



