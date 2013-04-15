(* Entropy based algorithm *)
open Util
open Tree
open Data

(* get the entropy based on label counts *)
let e_of_counts counts =
  let total = float_of_int @: sum_counts counts in
  let sub_calc = List.fold_left 
    (fun acc (_,count) -> 
      let c = float_of_int count in 
      let elem = c *. (log c) in 
      acc +. elem) 
    0.
    counts in
  let log_n = log total in
  let inverse_n = 1. /. total in
  log_n -. (inverse_n *. sub_calc)

(* get the entropy of a list of data *)
let e_of_data l = 
  let counts = get_label_counts l in 
  e_of_counts counts

(* calculate the entropy reduction from selecting an attribute *)
let e_delta_of_counts len counts =
  (* loop over attribute values *)
  List.fold_left (fun acc (value, label_counts) ->
      let total = float_of_int @: sum_counts label_counts in
      let weight = total /. len in
      let entropy = e_of_counts label_counts in
      acc +. (weight *. entropy))
    0.
    counts 

let e_delta_of_attrib l index =
  let len = float_of_int @: List.length l in
  let counts = get_data_counts l index in
  e_delta_of_counts len counts

(* calculate the info gain *)
let info_gain_of_counts l len counts = 
  e_of_data l -. e_delta_of_counts len counts

let info_gain_of_attrib l index = e_of_data l -. e_delta_of_attrib l index

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
  list_min attributes (e_delta_of_attrib l)

(* calculate info gain ratio *)
let info_gr_of_attrib_len len l index =
  let counts = get_data_counts l index in
  let intrinsic_val = intrinsic_value_of_counts counts in
  let info_gain = info_gain_of_counts l len counts in
  info_gain /. intrinsic_val

let info_gr_of_attrib l index =
  let len = float_of_int @: List.length l in
  info_gr_of_attrib_len len

(* find the attribute with the highest info gain ratio *)
let max_info_gr attributes l =
  let len = float_of_int @: List.length l in
  list_max attributes (info_gr_of_attrib_len len l)

(* calculate chi-square probability *)
let calc_chi label_counts data_counts attrib =
    (* for 2 labels, we only do one calculations (pos and neg). For 3, we do 2
     * and average them out etc *)
    let num_labels = List.length label_counts in
    (* degrees of freedom *)
    let dof = (num_labels - 1) * (List.length data_counts - 1) in
    let total_count = sum_counts label_counts in

    let total_p = List.fold_left (fun acc (label, count) ->
        let p_div = foi count /. foi total_count in
        let n_div = foi (total_count - count) /. foi total_count in

        (* calculate the deviation *)
        let dev = List.fold_left (fun sum (v, label_counts) ->
            let v_count = foi @: sum_counts label_counts in
            let exp_p, exp_n = p_div *. v_count, n_div *. v_count in
            let plabel_count = foi @: 
              try List.assoc label label_counts with Not_found -> 0 in
            let nlabel_count = v_count -. plabel_count in
            let p,n = plabel_count -. exp_p, nlabel_count -. exp_p in
            let p',n' = (p *. p) /. exp_p, (n *. n) /. exp_n in
            sum +. p' +. n')
          0. data_counts
        in
        Printf.printf "dev: %f, dof: %i\n" dev dof;
        let p = ChiSquare.chi2prob dof dev in
        acc +. p)
      0.
      (list_tail label_counts)
   in
   let res = total_p /. (foi @: num_labels - 1) in
   if res = nan then 0. else res

(* convert data to a tree using entropy *)
let tree_of_data print use_gr use_chi list_data : tree_t =
  if print then Printf.printf "Read %d entries\n" (List.length list_data);
  let len = Array.length @: snd @: list_head list_data in

  let rec loop l attribs =
    let min_attrib, min_e = 
      if use_gr then max_info_gr attribs l
      else min_entropy attribs l in
    let rem_attrib = List.filter 
      (function i when i=min_attrib -> false | _ -> true) attribs in
    let label_counts = get_label_counts l in

    let do_split () =
      let split = split_data l min_attrib in
      let vals = 
        list_map (fun (value, data) ->
          match label_counts, rem_attrib with
          | [], _   -> invalid_arg "Problem in tree_of_data"
          (* case of only one label type remaining *)
          | [l,x],_ -> Leaf(value, l)
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
      in min_attrib, vals
    in

    (* check if we should use chi-square *)
    if use_chi
    then let p = 
      calc_chi label_counts (get_data_counts l min_attrib) min_attrib in
      Printf.printf "p: %f\n" p;
      if p <= 0.05 then Some(do_split ())
      else None
    else Some(do_split ())
  in
  let r = create_range 0 len in
  match loop list_data r with 
  | None -> failwith "couldn't find a decent tree"
  | Some t -> t

