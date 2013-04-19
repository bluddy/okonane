open Util
open Data
open Tree
open Test
open MyRandom

let debug = ref false

let unwrap_fit = function None -> failwith "missing fitness" | Some f -> f
let no_none_f = function None -> 0. | Some x -> x

(* type of tree with fitness data attached *)
type fit_tree_t = float option * tree_t
type pop_t = fit_tree_t list

(* termination criteria type *)
type termination_t = Generations of int | Delta of float * int

(* function types *)
type fitness_t = tree_t -> (label_t * label_t) list -> float
type replacement_t = int -> pop_t -> pop_t -> pop_t -> pop_t
type selection_t = parameters_t -> int -> pop_t -> pop_t

and parameters_t = {
    build_p : float;       (* prob used to build initial trees *)
    fitness_fn : fitness_t;
    filter_p : float; (* prob for filtering application of fitness *)
    pop_size : int;
    selection_fn : selection_t;
    tournament_size : int;
    tournament_winners : int;
    mutation_p : float;
    crossover_p : float;
    replacement_fun : replacement_t;
    termination : termination_t;
  }

(* shortcuts to sorting functions *)
let sort_ascend_fst l = 
  List.sort (fun (ma,_) (mb,_) -> 
        match ma, mb with
        | None, _ | _, None -> failwith "missing labels"
        | Some a, Some b    -> if a -. b > 0. then 1 else (-1)) l

let sort_descend_fst l = 
  List.sort (fun (ma,_) (mb,_) -> 
        match ma, mb with
        | None, _ | _, None -> failwith "missing labels"
        | Some a, Some b    -> if a -. b > 0. then (-1) else 1) l

(* ---------- tree building functions --------- *)

(* p is the chance of making a node *)
let random_tree labels values p : fit_tree_t =
  let max_nodes = 2000 in
  let n = ref 0 in
  let l_num, a_num = Array.length labels, Array.length values in
  let inv_p = 1. -. p in
  let rec loop value = 
    n := !n + 1;
    (*Printf.printf "n = %d" !n; print_newline ();*)
    let attr = Random.int a_num in (* random attribute *)
    let attr_vals = 
      array_map (fun v ->
        if roll_f inv_p || !n > max_nodes then Leaf(v, labels.(Random.int l_num))
        else loop v)
      values.(attr) in
    Node(value, (attr, attr_vals))
  in
  let tree = match loop "" with Node(_,t) -> t | _ -> failwith "error" in
  None, tree

(* build all the starting members of our population *)
let random_trees labels values pop_size p : fit_tree_t list =
  let ts = list_populate (fun _ -> random_tree labels values p) 0 pop_size in
  if !debug then (print_endline "Generating random trees with sizes:";
    List.iter (fun (_,t) -> Printf.printf " %d" (size_of_tree t)) ts;
    print_endline "\n");
  ts

(* -------- fitness functions ------- *)

(* try to reward smaller trees *)
let size_penalty = 0.0001

let precision_fn tree l = 
  let size = size_of_tree tree in
  let prec = no_none_f @: fst @: avg_prec_recall l in
  prec -. (foi size) *. size_penalty

let recall_fn tree l =
  let size = size_of_tree tree in
  let recall = no_none_f @: snd @: avg_prec_recall l in
  recall -. (foi size) *. size_penalty

let prec_recall_fn tree l = 
  let size = size_of_tree tree in
  let prec, recall = avg_prec_recall l in
  let prec', recall' = no_none_f prec, no_none_f recall in
  let res = (prec' +. recall') *. 0.5 in
  res -. (foi size) *. size_penalty

(* evaluate the fitness for all trees that don't already have 
 * a fitness value *)
let eval_fitness_all fitness_fn filter_p data (trees:fit_tree_t list) =
  (* optionally filter out some of the data *)
  let filtered_data = match filter_p with
    | 1. -> data
    | p  -> List.filter (fun _ -> roll_f p) data in
  (* for each tree, classify the data and apply the fitness function *)
  let labels = fst @: List.split filtered_data in
  let new_pop = List.fold_left (fun acc (old_fit,tree) ->
      match old_fit with 
      | Some f -> (old_fit, tree)::acc (* skip fitted trees *)
      | None   -> let classes = 
          list_map (fun datum -> classify tree datum) filtered_data in
        let fitness = fitness_fn tree @: list_zip labels classes in
        (Some fitness, tree)::acc
    ) 
    [] trees in
  if !debug then (print_endline "Fitnesses: ";
    List.iter (fun (mf,_) -> Printf.printf " %f" (unwrap_fit mf)) new_pop;
    print_endline "\n"); 
  new_pop

(* get the avg fitness of a whole population *)
let avg_fitness pop =
  let total_fit, num = List.fold_left (fun (acc, i) (mf,_) ->
      match mf with None -> failwith "missing fitness" | Some f ->
      (acc +. f, i+1)
    )
    (0., 0)
    pop in
  total_fit /. (foi num)

(* Selection functions ------ *)
(* fitness proportionate function *)
let fitprop_fn _ num pop =
  (*print_endline "in fitprop";*)
  let total_fitness, acc_pop = List.fold_left (fun (sum, acc) ((mfit, _) as t) ->
      match mfit with None -> failwith "missing fitness" | Some fit ->
      sum +. fit, (sum +. fit, t)::acc)
    (0.,[]) 
    pop in
  let pop' = List.rev acc_pop in
  (*List.iter (fun (i,_) -> Printf.printf "%f " i) pop'; print_newline ();*)
  (*Printf.printf "total: %f\n" total_fitness; print_newline ();*)
  let pop_arr = Array.of_list pop' in (* array for bin search *)
  (* pick with replacement *)
  let res = snd @: iterate_until (fun (j,acc) -> 
      let rand = Random.float total_fitness in
      (*Printf.printf "rand: %f" rand;*)
      match binary_search pop_arr fst rand with 
      | Found x | NotPresent x ->
        let _, t = pop_arr.(x) in
        j+1, t::acc)
    (fun (j,_) -> j >= num)
    (0, []) in
  res
  (*print_endline "out"; res*)


(* rank-based: we go by rank *)
let rank_fn _ num pop =
  let sorted_pop = sort_ascend_fst pop in
  (* add up rank in one go *)
  let add,total_rank', acc_pop = List.fold_left (fun (add, sum, acc) t ->
      add + 1, sum + add, (sum, t)::acc)
    (2,0,[]) 
    sorted_pop in
  let total_rank = total_rank' - add + 1 in (* correct for too much adding *)
  (*List.iter (fun (i,_) -> Printf.printf "%d " i) acc_pop; print_newline ();*)
  (*Printf.printf "total: %i\n" total_rank; print_newline ();*)
  let pop_arr = Array.of_list (List.rev acc_pop) in (* array for bin search *)
  (* pick with replacement *)
  snd @: iterate_until (fun (j,acc) -> 
      let rand = Random.int total_rank in
      match binary_search pop_arr fst rand with Found x | NotPresent x ->
        let _, t = pop_arr.(x) in
        j+1, t::acc)
    (fun (j,_) -> j >= num)
    (0, [])

(* tournament: take a certain number of members, then take the best ones *)
let tournament_fn p num pop =
  let tourn_size, win_num = p.tournament_size, p.tournament_winners in
  (*Printf.printf "num: %d, pop: %d, tsize: %d\n" num (List.length pop) tourn_size;*)
  let pop_arr = Array.of_list pop in
  snd @: iterate_until
    (fun (i, take) ->
      let tourn = select_random_replace_arr tourn_size pop_arr in
      let to_take = if win_num > num - i then num - i else win_num in
      let winners = rank_fn () to_take tourn in
      i + to_take, winners@take
    )
    (fun (i, _) -> i >= num)
    (0, [])

(* ---- do crossover ----- *)
let crossover (_,tree1) (_,tree2) = 
  let size1, size2 = size_of_tree tree1, size_of_tree tree2 in
  let pt1, pt2 = Random.int size1, Random.int size2 in
  (* Printf.printf "size1: %d, size2: %d\n" size1 size2;
     Printf.printf "pt1: %d, pt2: %d\n" pt1 pt2;*)
  let z1, z2 = zipper_at tree1 pt1, zipper_at tree2 pt2 in
  let n1, n2 = zipper_get_node z1, zipper_get_node z2 in
  let z1', z2' = zipper_set_node z1 n2, zipper_set_node z2 n1 in
  let t1', t2' = tree_of_zipper z1', tree_of_zipper z2' in
  (None, t1'), (None, t2') (* remove old fitness *)

(* randomly do crossover *)
let do_crossover prob candidates =
  let rec loop ((children, parents, rest) as s) = function
    | []       -> s
    | [x]      -> children, parents, x::rest
    | x::y::xs ->  
        if roll_f prob then (* add children and parents *)
          let w, z = crossover x y in
          loop (w::z::children, x::y::parents, rest) xs
        else
          loop (children, parents, x::y::rest) xs
  in loop ([], [], []) candidates

(* ------- replacement functions -------- *)

(* replacement: throw away all parents *)
let replacement_fn _ children parents rest = children@rest

(* elitism: go by rank *)
let elitism_fn num children parents rest =
  let sorted = sort_descend_fst children@parents@rest in
  list_take num sorted

(* ------- mutation ------- *)

(* TODO debug: disabled mutation *)
(* attrib_sets: num of values in attrib, list of attributes with that number *)
let mutate labels values attrib_sets (tree':fit_tree_t) = 
  let subset = [0;1;2] (* random_subset [0;1;2] *) in 
  let modify (tree:tree_t) = function
    (* change a decision variable in a node *)
    (* we can only change to another variable of equal arity *)
    | 0 -> 
      (* the legal attributes to switch *)
      let pos_attribs = List.flatten attrib_sets in
      let tree_attribs = attribs_of_tree tree in
      let tree_attribs' = 
        List.filter (fun (_,attr) -> List.mem attr pos_attribs) tree_attribs in
      if list_null tree_attribs' then tree else
      (* choose one of the allowable nodes *)
      let node_num, attr = random_select_one tree_attribs' in
      let set = List.find (fun set -> List.mem attr set) attrib_sets in
      let other_attr = random_select_one @: list_remove attr set in
      let z = zipper_at tree node_num in
      let z' = modify_zipper 
        (fun (a, nlist) -> 
          if a <> attr then failwith @:  (* sanity check *)
            "attribute "^soi a^" doesn't match expected attribute "^soi attr
          else
            let v = values.(other_attr) in
            let nlist' = insert_idx_fst 0 nlist in
            let new_nlist = list_map (function 
                | i, Leaf (_, label) -> Leaf(v.(i), label)
                | i, Node (_, tree)  -> Node(v.(i), tree))
              nlist' in
            (other_attr, new_nlist))
        z in
      tree_of_zipper z' (* get the modified tree back *)

    (* add a node *)
    | 1 -> let nodes_leaves = tree_nodes_with_leaves tree in
      let chosen_node = random_select_one nodes_leaves in
      let z = zipper_at tree chosen_node in
      let z' = 
        modify_zipper 
          (fun (a, nlist) ->
            let leaves = List.fold_left (fun acc -> function 
              | Leaf _ -> acc+1 | _ -> acc) 0 nlist in
            let choice = Random.int leaves in
            (* modify the leaf we want to change *)
            let newlist = List.rev @: snd @: List.fold_left
              (fun (i,acc) node -> match node with
                  (* use random_tree with a chance of 0 to make only leaves *)
                  | Leaf(s, _) when i = choice -> 
                          i+1, Node(s, snd @: random_tree labels values 0.)::acc
                  | Leaf _ -> i+1, node::acc
                  | Node _ -> i,   node::acc)
              (0,[]) 
              nlist 
            in
            (a, newlist)
          )
          z in
      tree_of_zipper z'

    (* delete a node *)
    | _ -> let tree_size = size_of_tree tree in
      if tree_size = 1 then tree (* can't delete any more *)
      else 
        (* choose any node but the first *)
        let choice = 1 + Random.int (tree_size-1) in
        let z = zipper_at tree choice in
        let chosen_label = random_select_from_arr labels in
        match zipper_delete chosen_label z with
        | None    -> failwith "failed to delete at zipper"
        | Some z' -> tree_of_zipper z'
  in
  (* carry out any subset of mutations *)
  (* make sure to zero out the fitness which is no longer valid *)
  List.fold_left (fun (_,t) i -> None, modify t i) tree' subset

(* mutate a chunk of the population according to a probability *)
let mutate_all labels values attrib_sets prob pop =
  let subjects, rest = select_random_prob prob pop in
  let mutants = list_map (mutate labels values attrib_sets) subjects in
  mutants, rest

(* get the best fitness tree we have *)
let best_fitness pop = 
  let mbest, best_fit = list_head @: sort_descend_fst pop in
  match mbest with None -> failwith "missing label"
   | Some best -> best, best_fit


(* ------- main function -------- *)

(* attributes that can be swapped *)
let calc_attrib_sets uniq_vals =
    let s = list_mapi (fun (i, values) -> List.length values,i) uniq_vals in
    let assoc = List.fold_left (fun acc (num, i) -> 
        assoc_modify 
          (function None -> [i] | Some xs -> i::xs)
          num acc)
      [] s in
    (* keep only the sets that have more than 1 member *)
    let sets = List.filter (fun (_,l) -> List.length l > 1) assoc in
    list_map snd sets

(* start a run of the genetic algorithm *)
let genetic_run params (data:vector_t list) =
  let p = params in
  (* curry a shortcut *)
  let add_fitness = eval_fitness_all p.fitness_fn p.filter_p data in

  (* make arrays for fast random access *)
  let labels = Array.of_list @: uniq_labels data in
  let values_l = all_uniq_values data in
  let values = matrix_of_lists values_l in

  (* do some advanced legwork for mutation *)
  (* calculate sets of attribs and the number of values *)
  let attrib_sets = calc_attrib_sets values_l in

  (* populate our initial crop with random trees *)
  let start_pop = 
    add_fitness @: random_trees labels values p.pop_size p.build_p in

  let rec loop pop gen old_fitness =
    if !debug then (Printf.printf "Generation %d" gen; print_newline ());
    (* get a group of trees that'll make it to the next round *)
    let candidates = p.selection_fn p p.pop_size pop in
    let children,parents,rest = do_crossover p.crossover_p candidates in
    let children' = add_fitness children in
    let new_gen =
      p.replacement_fun p.pop_size children' parents rest in
    let mutated, rest = 
      mutate_all labels values attrib_sets p.mutation_p new_gen in
    let new_gen_f = (add_fitness mutated)@rest in
    (* do different things depending on our termination criterion *)
    if !debug then 
      (Printf.printf "best: %f, avg: %f" 
        (fst @: best_fitness new_gen_f) (avg_fitness new_gen_f); print_newline ()); 
      match p.termination, old_fitness with
      | Generations g, _ when gen >= g  -> best_fitness new_gen_f
      | Generations(g), _ -> loop new_gen_f (gen + 1) old_fitness
      (* if we're going by delta, check if enough gens have passes since a large
       * delta was seen. If so, stop *)
      | Delta(d, max_g), (old_g, old_f) -> 
          let best, best_tree = best_fitness new_gen_f in
          let delta = best -. old_f in
          if abs_float delta >= d
          then loop new_gen_f (gen + 1) (gen, best)
          else (* delta small *)
            if gen - old_g > max_g then 
              (Printf.printf "Converged to %f in %d generations.\n" best gen;
              best, best_tree (* we're done *))
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
let default_delta_gen = 20
let default_delta = 0.002

let default_params = {
    build_p = 0.2;
    fitness_fn = precision_fn;
    filter_p = 1.0;
    pop_size = 100;
    selection_fn = fitprop_fn;
    tournament_size = 40;
    tournament_winners = 5;
    mutation_p = 0.01;
    crossover_p = 0.4;
    replacement_fun = replacement_fn;
    termination = Delta (default_delta, default_delta_gen);
}
