(* File for data representation *)
open Util
open Array

type datum_t = string
type label_t = string
type vector_t = label_t * datum_t array

(* sum up the counts in a list *)
let sum_counts l = List.fold_left (fun a (_, n) -> a + n) 0 l

let load_data sep file : vector_t list =
  let reg = Str.regexp sep in
  let lines = read_file_lines file in
  let proc l = 
    let l' = Str.split reg l in
    let ar = Array.of_list @: list_tail l' in
    let lab = list_head l' in
    lab, ar in
  List.map proc lines

(* get counts for entropy *)
let get_data_counts l (index:int) =
  let do_fold map full_vec =
    let label = fst full_vec in
    let dat = snd full_vec in
    let vec = dat.(index) in
    assoc_modify (function 
      | Some inner_assoc ->
          assoc_modify (function
              | Some num -> num + 1
              | None     -> 1)
            label
            inner_assoc
      | None -> [label, 1])
      vec 
      map
  in
  List.fold_left do_fold [] l

(* get unique values for an attribute *)
let uniq_values (l:vector_t list) (index:int) = 
  List.fold_left (fun acc (_,vec) -> 
      let v = vec.(index) in
      if List.mem v acc 
      then acc else v::acc)
    [] l

(* get all unique values for all attributes *)
let all_uniq_values (l:vector_t list) =
  let (_, vec) = list_head l in
  let len = Array.length vec in
  let r = create_range 0 len in
  list_map (uniq_values l) r 

(* sum over labels *)
let sum_over_data_counts counts =
  list_map 
    (fun (value, label_counts) -> value, sum_counts label_counts)
    counts

(* get the counts of labels in the data into a list of (label; count) *)
(* must have labels *)
let get_label_counts l =
  List.fold_left 
    (fun lab_list x ->
      assoc_modify 
        (function Some i -> i+1 | None -> 1)
        (fst x)
        lab_list)
    [] l

(* extract the labels from the data *)
let uniq_labels l = fst @: List.split @: get_label_counts l

(* split data according to the values of an attribute *)
let split_data l index =
  List.fold_left
    (fun acc x ->
      let v = (snd x).(index) in
      assoc_modify (function None -> [x] | Some xs -> x::xs) v acc)
    [] l

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


