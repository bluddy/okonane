(* Decision Tree Representation *)

open Util
open Str

type attrib_t = string
type 'a val_t = Node of 'a * ('a tree_t)
              | Leaf of 'a
and 'a tree_t = attrib_t * ('a val_t) list

(* stringification functions *)
let rec string_of_val = function
  | Node(s, t) -> "Node("^s^", "^string_of_tree t^")"
  | Leaf s     -> "Leaf("^s^")"

and string_of_vals = function
  | []  -> ""
  | [v] -> string_of_val v
  | vl  -> 
    List.fold_left (fun acc v -> acc^"; "^string_of_val v) 
      (string_of_val @: list_head vl) @:
      list_tail vl

and string_of_tree (a, vl) =
  "Tree("^a^", ["^string_of_vals vl^"])"

(* read a portion of the string relating to val. 
 * Return val and the rest of the string *)
let rec val_of_string s : (string val_t * string) = 
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
    | "])" -> (a, vs), string_drop 2 s''
    | _    -> invalid_arg "tree_of_string: Missing ])"
  else invalid_arg "tree_of_string: Tree mismatch"





