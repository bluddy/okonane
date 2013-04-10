(* Decision Tree Representation *)

open Util
open Str
open Data

type attrib_t = int
type 'a val_t = Node of 'a * ('a tree_t)
              | Leaf of 'a * label_t
and 'a tree_t = attrib_t * ('a val_t) list

(* stringification functions *)
let rec string_of_val = function
  | Node(s, t) -> "Node("^s^", "^string_of_tree t^")"
  | Leaf(s, l) -> "Leaf("^s^": "^l^")"

and string_of_vals = function
  | []  -> ""
  | [v] -> string_of_val v
  | vl  -> 
    List.fold_left (fun acc v -> acc^"; "^string_of_val v) 
      (string_of_val @: list_head vl) @:
      list_tail vl

and string_of_tree (a, vl) =
  "Tree("^string_of_int a^", ["^string_of_vals vl^"])"

(* read a portion of the string relating to val. 
 * Return val and the rest of the string *)
(* let rec val_of_string s : (string val_t * string) = 
  let r =  Str.regexp("Node(\\([^,]*\\), \\(.*\\)") in
  if Str.string_match r s 0 then 
    let v, s' = Str.matched_group 1 s, Str.matched_group 2 s in
      let t, s'' = tree_of_string s' in
      match string_take 1 s'' with
      | ")" -> (*let i = Str.group_end 2 in
               Node(v, t), string_drop i s *)
               Node(v, t), string_drop 1 s''
      | _   -> invalid_arg "val_of_string: missing closing paren"
  else 
    let r = Str.regexp("Leaf(\\([^)]*\\))") in
    if Str.string_match r s 0 then
      let v, i = Str.matched_group 1 s, Str.group_end 1
      in Leaf(v), string_drop (i+1) s
    else invalid_arg "val_of_string: Leaf and Node mismatch"

and vals_of_string s : (string val_t list * string) =
  try 
    let v, s' = val_of_string s in
    match string_take 2 s' with
    | "; " -> let s'' = string_drop 2 s' in
              let v', s''' = vals_of_string s'' in
              v::v', s'''
    | _    -> [v], s'
  with Invalid_argument _ -> [], s

and tree_of_string s : (string tree_t * string) =
  let r = Str.regexp("Tree(\\([^,]*\\), \\[\\(.*\\)") in
  if Str.string_match r s 0 then
    let a, s' = Str.matched_group 1 s, Str.matched_group 2 s in
    let vs, s'' = vals_of_string s' in
    match string_take 2 s'' with 
    | "])" -> (int_of_string a, vs), string_drop 2 s''
    | _    -> invalid_arg "tree_of_string: Missing ])"
  else invalid_arg "tree_of_string: Tree mismatch"
*)

(* convert data to a tree using entropy *)
let tree_of_data ?(print=false) use_gr list_data : string tree_t =
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


