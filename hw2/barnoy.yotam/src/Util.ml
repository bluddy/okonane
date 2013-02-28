(* Utilities that are useful *)

(* low precedence function application allows us to remove 
 * many () from our code *)
let (@:) f x = f x;;

let flip f a b = f b a

let compose f g = fun x -> f (g x)

let id_fn a = a

let (|-) = compose

type ('a,'b) either_t = Left of 'a | Right of 'b

(* take the first x elements of a list *)
let list_take len li =
  let rec take len2 li2 acc_list =
    if len2 >= len then acc_list
    else match li2 with 
    | [] -> acc_list
    | head::tail -> take (len2+1) tail (head::acc_list)
  in
  List.rev (take 0 li [])

(* drop the first x elements of a list *)
let rec list_drop len li = match li with
  | [] -> []
  | x::xs when len = 0 -> li
  | x::xs -> list_drop (len-1) xs

(* take the last x elements of a list *)
let list_take_end len li = list_drop (List.length li - len) li

(* drop from the end of a list *)
let list_drop_end len li = list_take (List.length li - len) li

let list_zip list1 list2 = List.map2 (fun i j -> (i,j)) list1 list2

let list_head l = match l with 
  | x::_ -> x 
  | _ -> invalid_arg "empty list"

let list_last xs = list_head @: list_take_end 1 xs

(* will only remove one instance of x in xs (as opposed to filter) *)
let list_remove r l = 
  let rec loop acc = function
    | x::xs when x = r -> (List.rev acc)@xs
    | x::xs -> loop (x::acc) xs
    | []    -> List.rev acc
  in loop [] l

let compose_fn f g x = f(g x)

(* function that folds until a predicate is true *)
let rec foldl_until f p acc = function
    | x::_ when p acc x -> acc 
    | x::xs -> foldl_until f p (f acc x) xs 
    | []    -> acc

(* I/O helpers *)
let read_file_lines file = 
  let in_chan = open_in file in
  let rec read_lines acc = 
      try 
        let acc' = input_line in_chan :: acc
        in read_lines acc'
      with End_of_file -> acc in
  let ls = List.rev @: read_lines [] in
  close_in in_chan; ls 

let read_file file = String.concat "\n" @: read_file_lines file

(* make a range from first to last. Tail recursive *)
let create_range ?(step=1) first length =
    let last = first + ((length-1) * step) in
    let rec range_inner index acc =
        if index > last then acc
        else range_inner (index+step) (index::acc)
    in
    List.rev(range_inner first [])

(* make a range that corresponds to a given list *)
let create_corr_range first xs = create_range first @: List.length xs

let insert_idx_fst first xs = 
    let is = create_corr_range first xs in
    list_zip is xs

let insert_idx_snd first xs = 
    let is = create_corr_range first xs in
    list_zip xs is

(* tail recursive, so more efficient than mapping alone *)
let list_map f l = List.rev @: List.rev_map f l

(* transform a list into a list of lists of i elements *)
(* if there aren't enough elements to fill the last list, it's filled as much as
 * possible *)
let list_bunch i l = 
  let rec loop acc l = 
    match l with
    | [] -> acc
    | _  -> let taken = list_take i l in
      match list_drop i l with
      | []    -> taken::acc
      | xs    -> loop (taken::acc) xs
  in List.rev @: loop [] l

(* intersperse 2 lists together. When one runs out, continue with the other *)
let list_intersperse la lb =
  let rec loop acc l1 l2 = match l1, l2 with
    | x::xs, y::ys -> loop (y::x::acc) xs ys
    | x::xs, []    -> loop (x::acc) xs []
    | [],    y::ys -> loop (y::acc) [] ys
    | [], []       -> acc
  in List.rev @: loop [] la lb

(* find the maximum element of a list according to a transformation function (to int) *)
let list_minmax op l f = match l with
  | [x]   -> (x, f x)
  | x::xs -> 
    List.fold_left 
      (fun acc m -> let n = f m in 
                    if op n (snd acc) then (m,n) else acc) 
      (x, f x) xs
  | _     -> invalid_arg "Empty list"

let list_max l f = list_minmax (>) l f
let list_min l f = list_minmax (<) l f
    
(* flatten a list, removing option elements *)
let flatten_option l = List.filter (function None -> false | Some _ -> true) l

(* --- Array function --- *)

let array_find pred arr =
    let l = Array.length arr and index = ref 0 and found = ref false in
    while not !found && !index < l do
        found := pred arr.(!index);
        index := !index + 1;
    done;
    index := !index - 1;
    if not !found then raise Not_found
    else !index, arr.(!index)

(* a form of array find that allows a predicate to return any data *)
let array_find_return pred arr =
    let l = Array.length arr and index = ref 0 and found = ref false in
    let data = ref None in
    let eval () = 
        data := pred arr.(!index);
        found := match !data with
          | None   -> false
          | Some x -> true;
    in while not !found && !index < l do
        eval ();
        index := !index + 1;
    done;
    index := !index - 1;
    !index, !data

(* map an array to a list *)
let array_map f arr = 
  List.rev @: Array.fold_left (fun acc x -> (f x)::acc) [] arr

(* split a string into lines *)
let string_lines s = Str.split (Str.regexp "\n") s

(* maybe function. def is for None, f is given the value on Some *)
let maybe def f = function
  | None -> def
  | Some x -> f x

