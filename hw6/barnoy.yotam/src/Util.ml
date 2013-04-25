(* Utilities that are useful *)

(* base 2 logarithm *)
let log_conv = 1. /. log 2.
let log2 x = log x *. log_conv

(* abbreviations for annoyingly long functions *)
let foi = float_of_int
let iof = int_of_float
let soi = string_of_int
let sof = string_of_float
let ios = int_of_string
let fos = float_of_string
let bos = bool_of_string
let sob = string_of_bool

(* low precedence function application allows us to remove 
 * many () from our code *)
let (@:) f x = f x;;

let flip f a b = f b a

let compose f g = fun x -> f (g x)

let id_fn a = a

let (|-) = compose

type ('a,'b) either_t = Left of 'a | Right of 'b

let list_null = function [] -> true | _ -> false

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

let list_tail l = match l with 
  | _::x -> x 
  | _ -> invalid_arg "empty list or singleton"

let list_last xs = list_head @: list_take_end 1 xs

(* will only remove one instance of x in xs (as opposed to filter) *)
let list_remove r l = 
  let rec loop acc = function
    | x::xs when x = r -> (List.rev acc)@xs
    | x::xs -> loop (x::acc) xs
    | []    -> List.rev acc
  in loop [] l

let compose_fn f g x = f(g x)

(* function that folds until a predicate is true, which point the output is
 * finalized. The fin function takes the remaining list *)
let rec foldl_until_fin f p fin acc = function
    | ((x::_) as l) when p acc x -> fin acc l
    | x::xs -> foldl_until_fin f p fin (f acc x) xs 
    | []    -> fin acc [] 

(* function that folds until a predicate is true *)
let rec foldl_until f p acc = function
    | x::_ when p acc x -> acc 
    | x::xs -> foldl_until f p (f acc x) xs 
    | []    -> acc

(* I/O helpers *)
(* read a file and convert it into lines *)
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

(* get an index with every item in a map *)
let list_mapi f l = List.rev @: snd @: List.fold_left
  (fun (i,acc) x -> i+1, (f (i,x))::acc) (0,[]) l

(* calls f on its output over and over, num times *)
let iterate f init num = 
  let rec loop acc = function
    | i when i<= 0 -> acc
    | i -> loop (f acc) (i-1)
  in loop init num

(* calls f on its output over and over again until p is true *)
let iterate_until f p init =
  let rec loop acc = 
    if p acc then acc
    else loop (f acc)
  in loop init

(* repeat a function many times, building a list from indices *)
(* do this without instantiating the index list *)
let list_populate f init num = 
  List.rev @: snd @: iterate 
    (fun (i, acc) -> i+1, (f i)::acc)
    (init, [])
    num

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

(* functions without exceptions *)
let list_find f l = try Some(List.find f l) with Not_found -> None
let map_find k m = try Some(Map.find k m) with Not_found -> None

(* modify/add to an association list generically *)
let assoc_modify f item l =
  try
    let a = List.assoc item l in
    let rest = List.remove_assoc item l in
    (item, f (Some a))::rest
  with 
    Not_found -> (item, f None)::l


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

(* make a 2 dimensional array *)
(* init func allows for refs *)
let create_2d_array dimx dimy init_f =
  let rx = create_range 0 dimx in
  let ry = create_range 0 dimy in
  let l = list_map (fun _ ->
    let l' = list_map (fun _ -> init_f ()) rx in
    Array.of_list l') ry in
  Array.of_list l

(* fold over a 2d array *)
(* a (i,j) index indicates colum, row, and the user function needs to handle it
 *)
let matrix_fold f init arr =
  Array.fold_left (fun (j, acc) inner_arr ->
      j+1, snd @: Array.fold_left 
        (fun (i, acc') x -> i+1, f acc' ((i,j),x))
        (0, acc)
        inner_arr)
    init
    arr

(* convert a list of lists to an array matrix for random access *)
let matrix_of_lists l = 
  let l' = list_map Array.of_list l in
  Array.of_list l'

type 'a bin_search_t = Found of int | NotPresent of int

(* do a binary search over an array. Search for a value greater than the one
 * we're looking for *)
(* requires an ascending list *)
let rec binary_search a fn value : 'a bin_search_t =
  let len = Array.length a in
  (*Printf.printf "in binsearch : len: %d" len; print_newline ();*)
  let rec loop low high =
    (*Printf.printf "low: %d high: %d" low high; print_newline ();*)
    if high = low then
      if fn (a.(low)) = value then Found low
      else if fn(a.(low)) < value && low < len - 1 then NotPresent(low+1)
      else NotPresent low
    else let mid = (low + high) / 2 in
      if fn(a.(mid)) > value then
        if low = mid then loop low mid
        else loop low (mid - 1)
      else if fn(a.(mid)) < value then
        if high = mid then loop mid high
        else loop (mid + 1) high
      else
        Found mid
  in loop 0 (len-1)


(* --- String functions --- *)
(* split a string into lines *)
let string_lines s = Str.split (Str.regexp "\(\n\|\n\r\)+") s

let string_unlines l = String.concat "\n" l

let string_words s = Str.split (Str.regexp "[\t ]+") s

let string_unwords l = String.concat " " l

let string_take i s = let l = String.length s in
  let i' = if i > l then l else i in
  Str.first_chars s i'

let string_drop i s = let l = String.length s in
  let i' = if i > l then l else i in
  Str.string_after s i'

(* maybe function. default is for None, f is given the value on Some *)
let maybe def_fn fn = function
  | None -> def_fn ()
  | Some x -> fn x

(* efficient way to get unique values from a function on a list *)
let nubf fn xs =
    let blank = Hashtbl.create (List.length xs) in
    List.iter (fun x -> Hashtbl.replace blank (fn x) ()) xs;
    Hashtbl.fold (fun h () t -> h :: t) blank []

(* efficient function to get unique entities *)
let nub xs = nubf id_fn xs

(* bound a value by a max absolute *)
let abs_bound v max =
  if v > max then max else
  if v < -max then -max else v
  
