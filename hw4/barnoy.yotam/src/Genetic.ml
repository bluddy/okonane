open Util
open Data
open Tree
open Test
open MyRandom

(* type of tree with fitness data attached *)
type fit_tree_t = float option * tree_t
type pop_t = fit_tree_t list

(* termination criteria type *)
type termination_t = Generations of int | Delta of float * int

(* function types *)
type fitness_t = fit_tree_t -> (string * string) list -> fit_tree_t
type replacement_t = int -> pop_t -> pop_t
type selection_t = parameters_t -> int -> pop_t -> pop_t

and parameters_t = {
    build_p : float;       (* prob used to build initial trees *)
    fitness_fn : fitness_t;
    filter_p : float option; (* prob for filtering application of fitness *)
    pop_size : int;
    selection_fn : selection_t;
    tournament_size : int;
    tournament_winners : int;
    mutation_p : float;
    crossover_p : float;
    replacement_fn : replacement_t;
    termination : termination_t;
  }




(* shortcuts to sorting functions *)
let sort_ascend_fn fn = List.sort 
  (fun a b -> if fn a -. fn b > 0. then 1 else (-1))
let sort_descend_fn fn = List.sort 
  (fun a b -> if fn a -. fn b > 0. then (-1) else 1)
let sort_ascend_fst = sort_ascend_fn fst
let sort_descend_fst = sort_descend_fn fst

(* ---------- tree building functions --------- *)

(* p is the chance of making a node *)
let random_tree labels values p : fit_tree_t =
  let l_num, a_num = Array.length labels, Array.length values in
  let rec loop () : tree_t = 
    let attr = Random.int a_num in (* random attribute *)
    let attr_vals = array_map (fun v ->
        if roll_f p then Node(v, loop ())
        else Leaf(v, labels.(Random.int l_num)))
      values.(attr) in
    attr, attr_vals
  in
  let tree = loop ()
  in (None, tree)

(* build all the starting members of our population *)
let random_trees labels values pop_size p =
  list_populate (fun _ -> random_tree labels values p) 0 pop_size

(* -------- fitness functions ------- *)

let precision_fn tree l = 
  let prec = fst @: avg_prec_recall l in
  prec

let recall_fn tree l =
  let recall = snd @: avg_prec_recall l in
  recall

let prec_recall_fn tree l = 
  let prec, recall = avg_prec_recall l in
  prec + recall

(* evaluate the fitness for all trees on the data, and return the trees with
 * normalized fitness values *)
let eval_fitness_all fitness_fn filter_p data trees =
  let labels = fst @: List.split data in
  (* optionally filter out some of the data *)
  let filtered_data = match filter_p with
    | 1. -> data
    | p  -> List.filter (roll_f p) data in
  (* for each tree, classify the data and apply the fitness function *)
  List.fold_left (fun acc (_,tree) ->
      let classes = list_map (fun datum -> classify tree datum) filtered_data in
      let fitness = fitness_fn tree @: list_zip labels classes in
      (Some fitness, tree)::acc
    ) 
    [] trees

(* Selection functions ------ *)
(* fitness proportionate function *)
let fitprop_fn _ num pop =
  let total_fitness = 
    List.fold_left (fun acc (fit, _) -> acc + fit) 0. pop in
  let rec loop i taken rest = 
    let i', taken', rest' = 
      foldl_until 
        (fun (j, t, r) ((fit, _) as tree) ->
          if roll_f @: fit /. total_fitness 
          then (j+1, tree::t, r)
          else (j, t, tree::r))
        (fun (j, _, _) -> j >= num) (* stop condition *)
        (i, taken, [])
        rest in
    if i' >= num then taken', rest' (* we have as many as we need *)
    else loop i' taken' rest' (* do another run *)
  in
  loop 0 [] pop

(* rank-based: just take the top num members *)
let rank_fn _ num pop =
  let sorted = sort_descend_fst pop in
  list_take num sorted, list_drop num sorted

(* tournament: take a certain number of members, then take the best ones *)
let tournament_fn p num pop =
  let tourn_size, win_num = p.tournament_size, p.tournament_winners in
  iterate_until
    (fun (i, (take, leave)) ->
      let tourn, rest = select_random tourn_size leave in
      let to_take = if win_num > num - i 
                    then num - i 
                    else win_num in
      let winners, rest' = rank_fn to_take tourn in
      i + to_take, (winners@take, rest'@rest)
    )
    (fun (i, _) -> i >= num)
    (0, [], pop)

(* ---- do crossover ----- *)
let crossover (_,tree1) (_,tree2) = 
  let size1, size2 = tree_size tree1, tree_size tree2 in
  let pt1, pt2 = Random.int size1, Random.int size2 in
  let z1, z2 = zipper_at tree1 pt1, zipper_at tree2 pt2 in
  let n1, n2 = zipper_get_node z1, zipper_get_node z2 in
  let z1', z2' = zipper_set_node z1 n2, zipper_set_node z2 n1 in
  let t1', t2' = zipper_top z1', zipper_top z2' in
  (None, t1'), (None, t2') (* fitness is invalid *)

let crossover_all parents =
  let len = List.length parents / 2 in
  let p1, p2 = list_take len parents, list_drop len parents in
  let p = list_zip p1 p2 in
  List.fold_left (fun acc (t1,t2) -> 
      let t1', t2' = crossover t1 t2 in
      t1'::t2'::acc)
    []
    p

(* ------- selection functions -------- *)

(* replacement: throw away all parents *)
let replacement_fn _ singles parents children = children@singles

(* elitism: go by rank *)
let elitism_fn num singles parents children =
  let adults = parents@singles in
  let sorted = sort_ascend_fst adults in
  let best = list_take num sorted in
  children@best

(* ------- mutation ------- *)

(* attrib_sets: num of values in attrib, list of attributes with that number *)
let mutate labels values attrib_sets (_,tree) = 
  let subset = random_subset [0;1;2] in
  let modify tree = function
    (* change a decision variable in a node *)
    (* we can only change to another variable of equal arity *)
    | 0 -> let size = tree_size tree in
      (* the legal attributes to switch *)
      let pos_attribs = List.flatten attrib_sets in
      let tree_attribs = attribs_of_tree tree in
      let tree_attribs' = 
        List.filter (fun (_,attr) -> List.mem attr pos_attribs) attribs in
      let node_num, attr = random_select_one tree_attribs' in
      let set = List.find (fun set -> List.mem attr set) attrib_sets in
      let other_attr = random_select_one @: list_remove attr set in
      let z = zipper_at tree node_num in
      let z' = modify_zipper 
        (fun (a, nlist) -> 
          if a <> attr then failwith @: 
            "attribute "^soi a^" doesn't match expected attribute "^soi attr
          else
            let v = values.(other_attr) in
            let nlist' = insert_idx_fst nlist in
            let new_nlist = list_map (function 
                | i, Leaf (_, label) -> Leaf(v.(i), label)
                | i, Node (_, tree)  -> Node(v.(i), tree))
              nlist' in
            (other_attr, new_nlist))
        z' in
      zipper_top z' (* get the modified tree back *)
    (* add a node *)
    | 1 -> let nodes_leaves = tree_nodes_with_leaves tree in
      let chosen_node = random_select_one nodes_leaves in
      let z = zipper_at tree chosen_node in
      let z' = modify_zipper (fun (a, nlist) ->
          let leaves = List.fold_left (fun acc -> function 
            | Leaf _ -> acc+1 | _ -> acc) 0 nlist in
          let choice = Random.int leaves in
          let newlist = List.fold_right
            (fun node (i,acc) ->
              if i = choice 
              then let new_node = match node with
                    | Leaf(s, _) -> Node(s, random_tree labels values 0.)
                    | Node _ -> failwith "error" in
                  (i+1, new_node::acc)
              else (i+1, node::acc))
            (0,[]) nlist in
          (a, nlist))
        z in
      zipper_top z'
    (* delete a node *)
    | _ -> let tree_size = size_of_tree tree in
      if tree_size = 1 then tree (* can't delete any more *)
      else 
        (* choose any node but the first *)
        let choice = 1 + Random.int (tree_size-1) in
        let z = zipper_at tree choice in
        let chosen_label = random_select_from_arr labels in
        match zipper_delete chosen_label z with
        | None -> failwith "failed to delete at zipper"
        | Some z' -> zipper_top z'
  in
  (* carry out any subset of mutations *)
  (* make sure to zero out the fitness which is no longer valid *)
  List.fold_left (fun t i -> (None, modify t i)) tree subset

(* mutate a chunk of the population according to a probability *)
let mutate_all labels values attrib_sets prob pop =
  let subjects, rest = select_random_prob prob pop in
  let mutants = list_map (mutate labels values attrib_sets) mutants in
  mutants, rest

(* get the best fitness tree we have *)
let best_fitness pop = list_take 1 @: sort_descend_fst pop

(* ------- main function -------- *)

(* start a run of the genetic algorithm *)
let genetic_run print params data =
  let p = params in
  (* calculate the number of trees we need to keep and to recombine *)
  let keep_num = 
    let n = iof @: (1. -. p.crossover_p) *. (foi p.pop_size) in
    if n mod 2 = 1 then n-1 else n in
  let breed_num = p.pop_size - keep_num in
  (* curry a shortcut *)
  let add_fitness = eval_fitness_all p.fitness_fn p.filter_p data in

  (* make arrays for fast random access *)
  let labels = Array.of_list @: uniq_labels data in
  let values_l = all_uniq_values data in
  let values = matrix_of_lists values_l in

  (* do some advanced legwork for mutation *)
  (* calculate sets of attribs and the number of values *)
  let attrib_sets = 
    let s = list_map (fun (values,i) -> List.length values,i) @:
        insert_idx_snd 0 values_l in
    let assoc = List.fold_left (fun acc (num, i) -> 
        assoc_modify 
          (function None -> [i] | Some xs -> i::xs)
          num acc)
      [] s in
    (* keep only the sets that have more than 1 member *)
    let sets = List.filter (fun (_,l) -> List.length > 1) in
    list_map snd sets
  in
    
  (* populate our initial crop with random trees *)
  let start_pop = add_fitness @: random_trees labels values p.pop_size
  in
  let loop pop gen old_fitness =
    (* get a group of trees that'll make it to the next round *)
    let selected_pop, rest = p.selection_fn keep_num pop in
    (* get a group of parents *)
    let parents, _ = p.selection_fn breed_num rest in
    let children = crossover parents in
    let new_gen = p.replacement_fn selected_pop parents children in
    let mutated, rest = 
      mutate_all labels values attrib_sets p.mutation_p new_gen in
    let new_gen_f = add_fitness (mutated@rest) in
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
  snd @: loop start_pop 1 (0,0.)
  
(* options for choosing from main file *)
let selection_opts = 
  ["fitprop", fitprop_fn; "rank", rank_fn; "tournament", tournament_fn]

let replacement_opts = 
  ["elitism", elitism_fn; "replacement", replacement_fn]

let fitness_opts =
  ["precision", precision_fn; "recall", recall_fn; "mix", prec_recall_fn]

(* default values -------- *)
let default_delta_gen = 100
let default_delta = 0.01

let default_params = {
    build_p = 0.3;
    fitness_fn = precision_fn;
    filter_p = 1.0;
    pop_size = 1000;
    selection = fitprop_fn;
    tournament_size = 40;
    tournament_winners = 5;
    mutation_p = 0.01;
    crossover_p = 0.4;
    replacement = replacement_fn;
    termination = Delta (default_delta, default_delta_gen);
}
