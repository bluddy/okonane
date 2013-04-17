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

(* sum over labels *)
let sum_over_data_counts counts =
  list_map 
    (fun (value, label_counts) -> value, sum_counts label_counts)
    counts

let get_num_attribs (l:vector_t list) : int = Array.length @: snd @: list_head l

let get_all_data_counts l = 
  list_populate (get_data_counts l) 0 @: get_num_attribs l

(* get unique values for an attribute *)
let uniq_values (l:vector_t list) index = 
  nubf (fun (_,vec) -> vec.(index)) l

(* get all unique values for all attributes *)
let all_uniq_values (l:vector_t list) =
  let len = get_num_attribs l in
  list_populate (uniq_values l) 0 len
  
(* find an attrib with the smallest number of values *)
let get_min_val_count_attrib (l:vector_t list) : int=
  let vals = insert_idx_fst 0 @: all_uniq_values l in
  fst @: fst @: list_min vals snd

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
let uniq_labels l = nubf fst l

(* split data according to the values of an attribute *)
let split_data l index =
  List.fold_left
    (fun acc x ->
      let v = (snd x).(index) in
      assoc_modify (function None -> [x] | Some xs -> x::xs) v acc)
    [] l


