(* Entropy based algorithm *)
open Util
open Tree
open Data

let debug = ref false

(* get the entropy based on label counts *)
(* this is an optimization. We modify the forumla of entropy from:
  * \sum (- n_i / N * log (n_i / N) ) to
  * \sum (- n_i / N * (log n_i - log N))
  * log N - 1/N * \sum (n_i * log n_i) *)
let e_of_counts counts =
  let total = float_of_int @: sum_counts counts in
  let sub_calc = List.fold_left 
    (fun acc (_,count) -> 
      let c = float_of_int count in 
      let elem = c *. (log2 c) in 
      acc +. elem) 
    0.
    counts in
  let log_n = log2 total in
  let inverse_n = 1. /. total in
  log_n -. (inverse_n *. sub_calc)

(* get the entropy of a list of data *)
let e_of_data l = 
  let counts = get_label_counts l in 
  e_of_counts counts

(* calculate the entropy reduction from selecting an attribute *)
let e_remain_of_counts len data_counts =
  (* loop over attribute values *)
  List.fold_left (fun acc (value, label_counts) ->
      let total = float_of_int @: sum_counts label_counts in
      let weight = total /. len in
      let entropy = e_of_counts label_counts in
      acc +. (weight *. entropy))
    0.
    data_counts 

let e_remain_of_attrib l index =
  let len = foi @: List.length l in
  let counts = get_data_counts l index in
  e_remain_of_counts len counts

(* calculate the info gain *)
let info_gain_of_counts l len counts = 
  e_of_data l -. e_remain_of_counts len counts

let info_gain_of_attrib l index = e_of_data l -. e_remain_of_attrib l index

(* find the attribute with the highest info gain *)
let max_info_gain attributes l = list_max attributes @: info_gain_of_attrib l

(* calculate intrinsic value of counts *)
let intrinsic_value_of_counts counts =
  let summed_counts = sum_over_data_counts counts in
  e_of_counts summed_counts

(* calculate intrinsic value *)
let intrinsic_value_of_attrib l index = 
  let counts = get_data_counts l index in
  intrinsic_value_of_counts counts

(* find the lowest entropy attribute
 * we don't need to calculate actual entropy here -- we only care about the
 * delta *)
let min_entropy attributes l =
  list_min attributes (e_remain_of_attrib l)

(* calculate info gain ratio *)
let info_gr_of_attrib_len len l index =
  let counts = get_data_counts l index in
  let intrinsic_val = intrinsic_value_of_counts counts in
  let info_gain = info_gain_of_counts l len counts in
  info_gain /. intrinsic_val

let info_gr_of_attrib l index =
  let len = float_of_int @: List.length l in
  info_gr_of_attrib_len len l index

(* find the attribute with the highest info gain ratio *)
let max_info_gr attributes l =
  let len = foi @: List.length l in
  list_max attributes (info_gr_of_attrib_len len l)

(* calculate the deviation *)
let calc_dev len label label_count data_counts = 
  let p_div = foi label_count /. foi len in
  let n_div = foi (len - label_count) /. foi len in
  List.fold_left (fun sum (v, label_counts) ->
    let v_count = foi @: sum_counts label_counts in
    (* expected numbers *)
    let exp_p, exp_n = p_div *. v_count, n_div *. v_count in
    (* get positive label count for this values *)
    let plabel_count = foi @: 
      try List.assoc label label_counts with Not_found -> 0 in
    let nlabel_count = v_count -. plabel_count in
    let p, n   = plabel_count -. exp_p, nlabel_count -. exp_n in
    let p', n' = (p *. p) /. exp_p, (n *. n) /. exp_n in
    sum +. p' +. n')
  0. data_counts

(* calculate the deviations for multiple labels. For n labels, we only do n-1
 * calculations *)
let calc_devs len label_counts data_counts : float list =
    list_map (fun (label, lcount) -> calc_dev len label lcount data_counts)
      (list_tail label_counts)

(* debug function *)
let calc_all_devs l =
  let len = List.length l in
  let lc,dcs = get_label_counts l, get_all_data_counts l in
  list_map (calc_devs len lc) dcs

(* calculate chi-square probability *)
let calc_chi label_counts data_counts =
    let num_labels = List.length label_counts in
    (* degrees of freedom *)
    let dof = (num_labels - 1) * (List.length data_counts - 1) in
    let len = sum_counts label_counts in
    let devs = calc_devs len label_counts data_counts in
    let total_p = List.fold_left (fun acc dev ->
        let p = ChiSquare.chi2prob dof dev in
        if !debug then Printf.printf "chi dev: %f, dof: %i, p: %f\n" dev dof p;
        acc +. p)
      0. devs in
    let res = total_p /. (foi @: num_labels - 1) in
    if !debug then Printf.printf "chi avg_p: %f\n" res;
    if res = nan then 0. else res

(* debug function *)
let calc_all_chis l =
  let lc,dcs = get_label_counts l, get_all_data_counts l in
  list_map (calc_chi lc) dcs

(* convert data to a tree using entropy *)
let tree_of_data use_gr use_chi list_data : tree_t =
  if !debug then Printf.printf "Read %d entries\n" (List.length list_data);
  let len = get_num_attribs list_data in

  let rec loop l attribs =
    let len = List.length l in
    if len = 0 then None else
    let best_attrib, best_e = 
      if use_gr then max_info_gr attribs l
      else max_info_gain attribs l in
    if !debug then 
      Printf.printf "Length %d. Best attrib is %d with %f\n" len best_attrib best_e;
    let rem_attrib = List.filter ((<>) best_attrib) attribs in

    let do_split () =
      let split = split_data l best_attrib in
      let vals = 
        list_map (fun (value, data) ->
          match get_label_counts data, rem_attrib with
          | [], _   -> invalid_arg "Problem in tree_of_data"
          (* case of only one label type remaining *)
          | [l,_],_ -> Leaf(value, l)
          (* case of no attributes left: take majority *)
          | xs, []  -> let maxl, _ = fst @: list_max xs snd in
                       Leaf(value, maxl)
          (* more attributes to choose: loop *)
          (* if we get a None, it means we couldn't split any more *)
          | xs, _   -> match loop data rem_attrib with
                       | None -> let maxl, _ = fst @: list_max xs snd in
                                 Leaf(value, maxl)
                       | Some t -> Node(value, t)
        ) split
      in Some(best_attrib, vals)
    in

    (* check if we should use chi-square *)
    if use_chi
    then let p = 
      calc_chi (get_label_counts l) (get_data_counts l best_attrib) in
      if p <= 0.05 then do_split () else None
    else do_split () (* always split if no chi-square test *)
  in
  let r = create_range 0 len in
  match loop list_data r with 
  | None -> Printf.printf ("Couldn't find a decent tree\n");
      (* create a dummy tree *)
      let min_attr = get_min_val_count_attrib list_data in
      let majority_label = 
        fst @: fst @: list_max (get_label_counts list_data) snd in
      min_attr, list_map (fun v -> Leaf(v, majority_label)) @: 
                  uniq_values list_data min_attr
  | Some t -> t

