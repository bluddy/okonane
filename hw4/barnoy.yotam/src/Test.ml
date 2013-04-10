(* Everything to do with testing *)

open Util
open Tree
open Data

(* classify a vector according to a tree *)
let classify tree vector =
  let vec = snd vector in (* ignore label *)
  let rec loop (attrib, val_l) =
    let v = vec.(attrib) in
    let m = List.find (function
        | Node(x, _) when x = v -> true
        | Leaf(x, _) when x = v -> true
        | _ -> false) 
      val_l in
    match m with
     | Leaf (_, label)   -> label
     | Node (_, subtree) -> loop subtree
  in
  loop tree

let classify_data tree l = list_map (classify tree) l

(* calculate the accuracy for a set of labels and classification outputs *)
(* l is a zipped list of labels, outputs *)
let accuracy l =
  let correct = List.fold_left (fun acc (label, out) ->
      if label = out then acc+1 else acc) 
    0 l in
  (foi correct) /. (foi @: List.length l)

(* calculate precision and recall for a particular class *)
(* l is a zipped list of labels, outputs *)
let stats_of class_label l =
  let t_pos, f_pos, pos = List.fold_left 
    (fun (t_pos, f_pos, pos) (label, out) ->
      match label = class_label, out = class_label with
      | true, true  -> t_pos+1, f_pos,   pos+1
      | true, false -> t_pos  , f_pos,   pos+1
      | false, true -> t_pos  , f_pos+1, pos
      | _, _        -> t_pos  , f_pos,   pos)
    (0,0,0) l in
  let recall = (foi t_pos) /. (foi pos) in
  let precision = (foi t_pos) /. (foi @: t_pos + f_pos) in
  recall, precision

(* calculate average stats for whole dataset *)
(* l is a zipped list of labels, outputs *)
let avg_prec_recall l =
  let uniq_labels = nub @: fst @: List.split l in
  let tot_r, tot_p = List.fold_left
    (fun (tot_r, tot_p) label ->
      let r, p = stats_of label l in
      tot_r +. r, tot_p +. p)
    (0.,0.) uniq_labels in
  let len = foi @: List.length uniq_labels in
  tot_r /. len, tot_p /. len

(* calculate confusion matrix *)
(* l is a zipped list of labels, outputs *)
let confusion_matrix l =
  let uniq_labels = nub @: fst @: List.split l in
  let uniq = List.length uniq_labels in
  let arr = Array.make_matrix uniq uniq (ref 0) in
  List.iter (fun (label, out) ->
    let p = arr.(out).(label) in
    p := !p + 1) l;
  uniq, arr

(* print out the confusion matrix *)
let print_conf_matrix (labels, matrix) =
  (* shorten labels to fit as necessary *)
  let labels' = List.map (string_take 9) labels in
  List.iter (fun l -> Printf.printf "%10s" l) labels';
  print_newline ();
  let numbered = insert_idx_fst 0 labels' in
  List.iter (fun (i, label) ->
      Printf.printf "%10s" label;
      List.iter (fun (j, _) ->
          Printf.printf "%10d" matrix.(i).(j))
        numbered;
      print_newline ())
    numbered;

