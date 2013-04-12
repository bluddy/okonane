(* Decision Tree Representation *)

open Util
open Str
open Data

type attrib_t = int
type 'a val_t = Node of 'a * ('a tree_t)
              | Leaf of 'a * label_t
and 'a tree_t = attrib_t * ('a val_t) list

(* zipper type to move around the tree *)
type 'a loc_t = Top 
        | Path of 'a loc_t * attrib_t * 'a * ('a val_t) list * ('a val_t) list
type 'a zipper_t = 'a loc_t * 'a tree_t

(* connect the lists of the zipper efficiently *)
let connect_nodes left n right =
  let right' = n::right in
  List.fold_left (fun acc node -> node::acc) right' left

(* split the nodes to get a certain number *)
let split_nodes nodes num =
  if List.length nodes <= num then invalid_arg "invalid node "^soi num
  else
    let rec loop i left l = begin match l, i with
      | [], _               -> invalid_arg "list too short"
      | xs , j when j = num -> (left, list_head xs, list_tail xs)
      | x::xs, j            -> loop (j+1) (x::left) xs
    end
    in loop 0 [] nodes

(* flip the node list to the right *)
let flip_right = function
  | left,n,[]    -> None
  | left,n,right -> Some(n::left, list_head right, list_tail right)

(* flip the node list to the left *)
let flip_left = function
  | [],n,right   -> None
  | left,n,right -> Some(list_tail left, list_head left, n::right)

(* get a zipper for a tree *)
let zipper_of_tree t : zipper_t = (Top, t)

(* zipper movement functions *)
let zipper_up = function
  | Top, _ -> None
  | Path(next, attr, v, ll, lr), t -> 
      let new_node = Node(v,t) in
      Some(next, (attr, connect_nodes ll new_node lr))

let zipper_down_i i = ((p, (attr, xs)) : zipper_t) =
  let l, n, r = split_nodes xs i in
  match v with 
  | Leaf _ -> None (* can't go down *)
  | Node(v, t) -> Some( Path(p, attr, v, l, r), t)

(* find node on right/left and return a zipper for it *)
let rec node_on_fn fn path attr ((l,n,r) as p) = 
  match n with
    | Leaf -> begin match fn p with
              | None -> None
              | Some p' -> node_on_fn p' end
    | Node(v,t) -> Some( Path(path, attr, v, l, r), t) 

(* find node on right from a place (l,n,r) and return a zipper for it *)
let node_on_right = node_on_fn flip_right 
let node_on_left = node_on_fn flip_left

(* find the first place we can go down *)
let zipper_down ((path, (attr, xs)):zipper_t) = 
  let place = split_nodes xs 0 in
  node_on_right path attr place

(* move zipper right *)
let zipper_right = function
  | Top, _ -> None
  | Path(path, attr, v, l, r), t -> 
      node_on_right path attr (l,Node(v,t),r)

(* move zipper left *)
let zipper_left = function
  | Top, _ -> None
  | Path(path, attr, v, l, r), t -> 
      node_on_left path attr (l,Node(v,t),r)

(* get the tree from the zipper *)
let zipper_top (zipper:zipper_t) = 
  let rec loop z = match z with
    | Top, _ -> z
    | _ -> zipper_up z
  in loop zipper

(* modify the tree at the zipper *)
let modify_zipper f (path,t) : zipper_t = (path, f t)

(* get the node from a zipper *)
let zipper_get_node (z:zipper_t) = snd z

(* get a zipper for a particular node number in the tree *)
(*let zipper_at tree i =*)
  (*let n = ref 0 in*)
  (*let zipper = zipper_of_tree tree*)
  (*in*)
  (*let rec move_down z =*)
    (*let rec move_right z =*)
      (*if !n >= i then Some z*)
      (*else*)
        (*match zipper_right z with*)
        (*| None    -> None*)
        (*| Some z' -> n := !n + 1;*)
            (*match move_down z' with*)
            (*| None -> move_right z'*)
            (*| x    -> x*)
    (*in*)
    (*if !n >= i then z*)
    (*else*)
      (*match zipper_down z with*)
      (*| None    -> None*)
      (*| Some z' -> n := !n + 1;*)
         (*move_right z'*)
  (*in*)
  (*let z_out = move_down z in*)
  (*if !n <> i then invalid_arg "node "^soi i^" not found in tree"*)
  (*else z_out*)

(* get a zipper for a particular node number in the tree *)
let zipper_fold_until f p init tree =
  let zipper = zipper_of_tree tree
  in
  let rec move_down acc z =
    let rec move_right a z =
      if p a z then Some a
      else
        let a' = f a z in
        match zipper_right z with
        | None    -> None
        | Some z' -> 
            match move_down a' z' with
            | None -> move_right a' z'
            | x    -> x
    in
    if p acc z then Some acc
    else
      let acc' = f acc z in
      match zipper_down z with
      | None    -> None
      | Some z' -> move_right acc' z'
  in
  move_down init @: zipper_of_tree tree

let zipper_fold f init tree =
  zipper_fold_until f (fun _ -> None) init tree

(* get a zipper at a location in the tree *)
let zipper_at tree i =
  let n = ref 0 in
  zipper_fold_until
    (fun _ _ -> n := !n + 1)
    (fun _ _ -> n >= i)
    () tree

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

(* fold over a tree *)
let tree_fold f init (tree:'a tree_t) =
  let rec loop t a =
    let a' = f a t in
    List.fold_left (fun acc -> function
        | Leaf _ -> acc
        | Node(_, subtree) -> loop subtree acc)
      a' @:
      snd t
  in loop tree init

(* fold over a tree until a predicate is matched *)
(* we also pass the last node value chosen so the function knows where it is *)
let tree_fold_until f p init (tree:'a tree_t) =
  let rec loop t a =
    if p a t then a
    else
      let a' = f m_a a' t in
      foldl_until (fun acc -> function
          | Leaf _           -> acc
          | Node(_, subtree) -> loop subtree acc)
        p
        a' @:
        snd t
  in loop tree init

(* get the number of attribute nodes of a tree *)
let tree_size tree =
  let num = ref 0 in
  tree_fold (fun _ _ -> num := !num + 1) 0 tree

(* get the path and node of a certain position in the tree *)
let tree_get_path_and_node tree i =
  let num = ref 0 in
  let path, m_node = 
    tree_fold_until
      (fun m_v (path, found_node) n ->
        num := !num + 1;
        match m_v with 
        | None   -> path, n
        | Some v -> v::path, n)
      (fun _ _ _ -> !num > i)
      ([], None)
      tree
  in
  List.rev path, match m_node with None -> invalid_arg "node not in tree"
                                 | Some n -> n

(* replace a path in the tree with a node *)
let tree_replace_node tree node path =
  let rec loop ((a, kids) as t) = function
    | []    -> node
    | x::xs -> 
        let new_t = List.fold_right (fun v acc ->
            match v with
              | Node(str,child) when str=x -> 
                  let old_node, new_t = loop child xs in
                  (Node(str, new_t))::acc
              | v -> v::acc)
          (snd t)
          (None, [])
        in
        a, new_t
  in
  loop tree path

(* turn tree attributes into a list *)
let attribs_of_tree tree =
  let n = ref 0 in
  tree_fold (fun acc node -> 
      let v = (!n, fst node) in
      n := !n + 1) 
    [] tree

(* modify a tree at a certain point *)
let tree_modify f tree i =
  let num = ref 0 in
  let rec loop ((a, kids) as t) 
    | x::xs -> 
        let new_t = List.fold_right (fun v acc ->
            match v with
              | Node(str,child) when str=x -> 
                  let old_node, new_t = loop child xs in
                  (Node(str, new_t))::acc
              | v -> v::acc)
          (snd t)
          (None, [])
        in
        a, new_t
  in
  loop tree path

let tree_modify f init (tree:'a tree_t) =
  let rec loop t acc' =
    let v = f acc' t in
    List.fold_left (fun acc -> function
        | Leaf _ -> acc
        | Node(_, subtree) -> loop subtree acc)
      acc' @:
      snd t
  in loop tree init

