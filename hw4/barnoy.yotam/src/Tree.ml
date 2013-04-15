(* Decision Tree Representation *)

open Util
open Str
open Data

type attrib_t = int
type val_t = Node of string * tree_t
           | Leaf of string * label_t
and tree_t = attrib_t * val_t list

(* zipper type to move around the tree in constant time *)
type loc_t = Top 
        | Path of loc_t * attrib_t * string * val_t list * val_t list
type zipper_t = loc_t * tree_t

(* connect the lists of the zipper efficiently *)
let connect_nodes left n right =
  let right' = n::right in
  List.fold_left (fun acc node -> node::acc) right' left

(* get a zipper for a tree *)
let zipper_of_tree t : zipper_t = (Top, t)

(* zipper movement functions *)
let zipper_up = function
  | Top, _ -> None
  | Path(next, attr, v, ll, lr), t -> 
      let new_node = Node(v,t) in
      Some(next, (attr, connect_nodes ll new_node lr))

(* delete the node at the zipper *)
let zipper_delete label = function
  | Top, _ -> None
  | Path(next, attr, v, ll, lr), t -> 
      let new_node = Leaf(v,label) in
      Some(next, (attr, connect_nodes ll new_node lr))

(* flip the node list to the right *)
let flip_right = function
  | left,n,[]    -> None
  | left,n,right -> Some(n::left, list_head right, list_tail right)

(* flip the node list to the left *)
let flip_left = function
  | [],n,right   -> None
  | left,n,right -> Some(list_tail left, list_head left, n::right)

(* find node on right/left and return a zipper for it *)
let rec node_on_side fn path attr p = 
  match fn p with
  | None -> None
  | Some ((l, n, r) as p') -> match n with
    | Leaf _    -> node_on_side fn path attr p'
    | Node(v,t) -> Some(Path(path, attr, v, l, r), t)

(* find node on right from a place (l,n,r) and return a zipper for it *)
let node_on_right = node_on_side flip_right 
let node_on_left = node_on_side flip_left

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

(* find the first place we can go down *)
let zipper_down ((path, (attr, xs)):zipper_t) = 
  let ((l,n,r) as p) = [], list_head xs, list_tail xs in
  match n with
  | Leaf _    -> node_on_right path attr p
  | Node(v,t) -> Some(Path(path, attr, v, l, r), t)

(* get the tree from the zipper *)
let zipper_top (zipper:zipper_t) = 
  let rec loop z = match zipper_up z with
    | None    -> z
    | Some z' -> loop z'
  in loop zipper

(* modify the tree at the zipper *)
let modify_zipper f (path,t) : zipper_t = (path, f t)

(* get the node from a zipper *)
let zipper_get_node (z:zipper_t) = snd z

(* set the node in a zipper *)
let zipper_set_node ((p,t):zipper_t) node : zipper_t = (p,node)

(* get tree from zipper *)
let tree_of_zipper = zipper_get_node |- zipper_top

(* fold over a tree using a zipper *)
let zipper_fold_until f p init tree =
  let rec loop acc z =
    (*print_endline "here";*)
    let rec loop_children a z =
      (*print_endline "there";*)
        let a' = loop a z in
        if p a' z then a' else
          match zipper_right z with
          | None    -> a'
          | Some z' -> loop_children a' z' (* loop over children *)
    in
    (*Printf.printf "at %d" (fst @: snd z); print_newline ();*)
    if p acc z then acc
    else
      let acc' = f acc z in
      match zipper_down z with (* does this node have children *)
      | None    -> acc'
      | Some z' -> loop_children acc' z'
  in
  loop init @: zipper_of_tree tree

let zipper_fold f init tree =
  zipper_fold_until f (fun _ _ -> false) init tree

(* get a zipper at a location in the tree *)
let zipper_at tree i : zipper_t =
  let n = ref 0 in
  let z = zipper_fold_until
    (fun _ z -> n := !n + 1; z)
    (fun _ _ -> !n > i)
    (zipper_of_tree tree) tree in
  if !n < i then invalid_arg @: "node "^soi i^" not found in tree"
  else z
   
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

(* fold over a tree until a predicate is matched *)
(* we also pass the last node value chosen so the function knows where it is *)
let tree_fold_until f p init (tree:tree_t) =
  let rec loop t a =
    if p a t then true, a
    else
      let a' = f a t in
      foldl_until (fun (stop, acc) -> function
          | Leaf _           -> (stop, acc)
          | Node(_, subtree) -> loop subtree acc)
        (fun (stop, _) _ -> stop)   
        (false, a') 
        (snd t)
  in snd @: loop tree init

(* fold over a tree *)
let tree_fold f init (tree:tree_t) =
  tree_fold_until f (fun _ _ -> false) init tree

(* get the number of attribute nodes of a tree *)
let size_of_tree tree =
  let num = ref 0 in
  let _ = tree_fold (fun _ _ -> num := !num + 1) () tree in
  !num

(* get the node numbers that have leaves *)
let tree_nodes_with_leaves tree = 
  let num = ref 0 in
  tree_fold (fun acc (_,nlist) ->
      let oldnum = !num in 
      num := !num + 1;
      if List.exists (function Leaf _ -> true | _ -> false) nlist
      then oldnum::acc else acc)
    [] tree
  
(* turn tree attributes into a list *)
let attribs_of_tree (tree: tree_t) =
  let n = ref 0 in
  tree_fold (fun acc node -> 
      let n_old = !n in
      n := !n + 1;
      (n_old, fst node)::acc)
    [] tree

(* unit test for zipper *)
let test_tree : tree_t = 
          (0,  [Node("a", 
                      (1, [Leaf("b", "t")])
                    );
                Leaf("c","f");
                Node("d",
                      (2, [Leaf("e", "t"); Leaf("f", "t"); Leaf("g","f")])
                    );
                Node("h",
                      (3, [Node("i",
                             (4, [Leaf("j","f")]))
                          ]
                      )
                    )
               ]
            )
let zipper_test () = 
  let z = zipper_of_tree test_tree in
  match zipper_down z with | None -> failwith "zipper error 1"
    | Some z'' -> match zipper_right z'' with None -> failwith "zipper error 2"
        | Some w -> match zipper_left w with None -> failwith "zipper error 3"
            | Some x -> match zipper_up w with None -> failwith "zipper error 4"
                | Some y -> let t = tree_of_zipper y in
                  if t = test_tree then true else false
                           
