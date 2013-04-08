(* File for data representation *)
open Util
open Array

type datum_t = string
type label_t = string
type vector_t = label_t option * datum_t array

let load_data has_label sep file : vector_t list =
  let reg = Str.regexp sep in
  let lines = read_file_lines file in
  let proc l = 
    let l' = Str.split reg l in
    let ar = Array.of_list @: list_tail l' in
    let lab = if has_label then Some (list_head l') else None in
    lab, ar in
  List.map proc lines

(* get counts for entropy. Must have labels *)
let get_data_counts l (index:int) =
  let do_fold map full_vec =
    let label = match fst full_vec with | Some l -> l | None ->
       invalid_arg "Must have labels in data" in
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

(* must have labels *)
let get_label_counts l =
  List.fold_left 
    (fun lab_list x ->
      match fst x with None -> invalid_arg "Must have labels" | Some label ->
        assoc_modify 
          (function Some i -> i+1 | None -> 1)
          label
          lab_list)
    [] l

let split_data l index =
  List.fold_left
    (fun acc x ->
      let v = (snd x).(index) in
      assoc_modify (function None -> [x] | Some xs -> x::xs) v acc)
    [] l

let get_entropy l index =
  let len = float_of_int @: List.length l in
  let counts = get_data_counts l index in
  List.fold_left (fun acc (choice, label_list) ->
      let total = float_of_int @: 
        List.fold_left (fun a (_, n) -> a + n) 0 label_list in
      List.fold_left (fun acc' (_, n) ->
          let frac = (float_of_int n) /. total in
          let res = (-1.) *. frac *. (log frac) in
          let weight = total /. len in
          let weighted = weight *. res in
          acc' +. weighted)
        acc
        label_list)
    0.
    counts 
  
let find_min_entropy len l =
  let r = create_range 0 len in
  list_minmax (<) r (get_entropy l)



