(* Everything to do with testing *)

open Util
open Tree
open Data

let filter_none l = 
  let no_none = List.fold_left 
    (fun acc -> function None -> acc | Some x -> x::acc) 
    [] l in
  List.rev no_none

(* classify a vector according to a tree *)
let classify tree vector =
  let vec = snd vector in (* ignore label *)
  let rec loop (attrib, val_l) =
    let v = vec.(attrib) in
    let m = try List.find (function
        | Node(x, _) when x = v -> true
        | Leaf(x, _) when x = v -> true
        | _ -> false) 
      val_l 
          with (* if we can't find the value, just take the first choice *)
             Not_found -> list_head val_l
      in
    match m with
     | Leaf (_, label)   -> label
     | Node (_, subtree) -> loop subtree
  in
  loop tree

let classify_all tree l = list_map (classify tree) l

(* calculate the accuracy for a set of labels and classification outputs *)
(* l is a zipped list of labels, outputs *)
let accuracy l =
  let len = List.length l in
  let correct = List.fold_left (fun acc (label, out) ->
      if label = out then acc+1 else acc) 
    0 l in
  if len <> 0 then Some ((foi correct) /. foi len)
  else None

(* calculate precision and recall for a particular class *)
(* l is a zipped list of labels, outputs *)
let stats_of l class_label =
  let t_pos, f_pos, pos = List.fold_left 
    (fun (t_pos, f_pos, pos) (label, out) ->
      match label = class_label, out = class_label with
      | true, true  -> t_pos+1, f_pos,   pos+1
      | true, false -> t_pos  , f_pos,   pos+1
      | false, true -> t_pos  , f_pos+1, pos
      | _, _        -> t_pos  , f_pos,   pos)
    (0,0,0) l in
  let recall = if pos <> 0 then Some ((foi t_pos) /. (foi pos)) else None in
  let precision = if t_pos + f_pos <> 0 
    then Some ((foi t_pos) /. (foi @: t_pos + f_pos)) else None in
  recall, precision

(* calculate average stats for whole dataset *)
(* l is a zipped list of labels, outputs *)
let avg_prec_recall l =
  (* choose classes to operate over *)
  let uniq_labels = nub @: fst @: List.split l in
  let r_ps = List.split @: list_map (stats_of l) uniq_labels in
  let rs, ps = fst r_ps, snd r_ps in
  let rs, ps = filter_none rs, filter_none ps in
  let r, len_r = List.fold_left (+.) 0. rs, List.length rs in
  let p, len_p = List.fold_left (+.) 0. ps, List.length ps in
  let r_res = if len_r = 0 then None else Some (r /. foi len_r) in
  let p_res = if len_p = 0 then None else Some (p /. foi len_p) in
  r_res, p_res

(* calculate confusion matrix *)
(* l is a zipped list of labels, outputs *)
let confusion_matrix (l:(label_t * label_t) list) =
  let split = List.split l in
  let all_labels = (fst split)@(snd split) in
  let uniq_labels = nub all_labels in
  let num = List.length uniq_labels in
  let labels = insert_idx_snd 0 uniq_labels in
  let arr = create_2d_array num num (fun _ -> ref 0) in
  List.iter (fun (label, out) ->
    let p = arr.(List.assoc out labels).(List.assoc label labels) in
    p := !p + 1) l;
  uniq_labels, arr

(* print out the confusion matrix *)
let print_conf_matrix (labels, matrix) =
  (* shorten labels to fit as necessary *)
  let labels' = List.map (string_take 8) labels in
  Printf.printf "%10s" " ";
  List.iter (fun l -> Printf.printf "%10s" l) labels';
  print_newline ();
  Printf.printf "%10s" " ";
  List.iter (fun _ -> Printf.printf "----------") labels'; 
  print_newline ();
  let numbered = insert_idx_fst 0 labels' in
  List.iter (fun (i, label) ->
      Printf.printf "%10s" @: label^" |";
      List.iter (fun (j, _) ->
          Printf.printf "%10d" !(matrix.(i).(j)))
        numbered;
      print_newline ())
    numbered

(* do k-fold cross validation *)
let k_fold k data train_fn test_fn =
  let len = List.length data in
  let size = len / k in
  Printf.printf "len = %i, size = %i, k = %i\n" len size k;
  let rec loop i before fold after results =
    match fold with 
    | [] -> results
    | _  ->
      print_endline @: "Fold "^soi i^": training... ";
      let train_data = before@after in
      let tree = train_fn train_data in
      print_endline "testing on training data... ";
      let res1 = test_fn tree train_data in
      print_endline "testing on test data... ";
      let res2 = test_fn tree fold in
      (* take care of small last fold *)
      let next_size = 
          let s = List.length after in
          if s - size < size then s else size in
      loop (i+1) (before@fold) (list_take next_size after) 
        (list_drop next_size after) ((tree,res1,res2)::results) in
  List.rev @: loop 1 [] (list_take size data) (list_drop size data) []

(* run a single test of the data *)
let test tree data = 
  let classes = classify_all tree data in
  let labels = list_map fst data in
  let l = list_zip labels classes in
  let acc = accuracy l in
  let prec, recall = avg_prec_recall l in
  let confm = confusion_matrix l in
  (acc, prec, recall, confm)

(* print a maybe float *)
let somf = function None -> "nan" | Some f -> string_of_float f

(* print test results to the screen *)
let print_results (acc, prec, recall, _) =
  Printf.printf "Acc:%5s, Prec:%5s, Recal:%5s" 
    (somf acc) (somf prec) (somf recall)

let std_dev (data:float option list) : (float option * float option) =
  let some_data = filter_none data in
  let total = List.fold_left ((+.)) 0. some_data in
  let len = List.length some_data in
  let mean = if len <> 0 then Some(total /. foi len) else None in
  let dev = match mean with None -> None
    | Some m -> Some(
        List.fold_left (fun acc x ->
        let d = x -. m in
        acc +. (d *. d))
      0.
      some_data) in
  let std_dev = 
    if len <> 0 then 
      match dev with None -> None
      | Some d -> Some(sqrt (d /. foi len)) 
    else None in
  mean, std_dev

let avg_results rs = 
  let a = std_dev @: list_map (fun (a,_,_,_) -> a) rs in
  let b = std_dev @: list_map (fun (_,b,_,_) -> b) rs in
  let c = std_dev @: list_map (fun (_,_,c,_) -> c) rs in
  a,b,c

(* prints the output of a k-fold *)
let print_results_all print_tree rs =
  print_endline "Results:\n";
  let rs' = insert_idx_fst 1 rs in
  List.iter (fun (i,(tree, ((_,_,_,tm) as train), ((_,_,_,em) as test))) -> 
      Printf.printf "Fold %i: ----------------------------- \n" i  ;
      print_string "Train data: ";
      print_results train;
      print_newline ();
      print_string "Test  data: ";
      print_results test;
      print_string "\n\n";
      print_conf_matrix em; (* only print conf matrix for test data *)
      print_newline ();
      if (print_tree) then print_endline @: string_of_tree tree;
      print_newline ();
    ) rs';
  let test_res = list_map (fun (_, _, t) -> t) rs in
  let (a_a,s_a),(a_p,s_p),(a_r,s_r) = avg_results test_res in
  Printf.printf "avg_acc %s, avg_prec %s, avg_recall %s\n" 
    (somf a_a) (somf a_p) (somf a_r);
  Printf.printf "std_acc %s, std_prec %s, std_recall %s\n" 
    (somf s_a) (somf s_p) (somf s_r)



