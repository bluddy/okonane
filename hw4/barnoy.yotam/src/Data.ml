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

let data_counts (index:int) l =
  List.fold_left 
    (fun map vec ->
      let dat = snd vec in
      let v = dat.(index) in
      try 
        let n = List.assoc v map in
        let map' = List.remove_assoc v map in
        (v, n+1)::map'
      with Not_found -> (v, 1)::map)
    [] 
    l



    




