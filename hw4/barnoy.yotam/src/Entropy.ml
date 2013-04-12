(* Entropy based algorithm *)
open Util
open Tree
open Data

(* convert data to a tree using entropy *)
let tree_of_data print use_gr list_data : string tree_t =
  if print then Printf.printf "Read %d entries\n" (List.length list_data);
  let len = Array.length @: snd @: list_head list_data
  in
  let rec loop l attribs =
    let min_i, min_e = 
      if use_gr then max_info_gr attribs l
      else min_entropy attribs l in
    let rem_attrib = List.filter 
      (function i when i=min_i -> false | _ -> true) attribs in
    let split = split_data l min_i in
    let vals = 
      list_map (fun (value, data) ->
        match get_label_counts data, rem_attrib with
        | [], _   -> invalid_arg "Problem in tree_of_data"
        (* case of only one label type remaining *)
        | [l,x],_ -> Leaf(value, l)
        (* case of no attributes left: take majority *)
        | xs, []  -> let maxl, _ = fst @: list_max xs snd in
                     Leaf(value, maxl)
        (* more attributes to choose: loop *)
        | xs, _   -> Node(value, loop data rem_attrib)
      ) split
    in min_i, vals
  in
  let r = create_range 0 len in
  loop list_data r

