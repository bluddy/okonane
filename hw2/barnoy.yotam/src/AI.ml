open Util
open Board
open GameState

type player_t = Human 
           | RandomAI
           | MinimaxAI

let is_human = function Human -> true | _ -> false

let string_of_player = function
  | Human    -> "Human"
  | RandomAI -> "Random AI"
  | MinimaxAI -> "Minimax AI"

(* evaluate the board by the number of possible actions each player has *)
let evaluate_by_moves s max =
  if s.turn <= 2 then 0 (* can't evaluate such an early turn *)
  else
    let moves = List.length @: expand !(s.board) s.turn in
    let moves' = List.length @: expand !(s.board) @: s.turn+1 in
    let e = moves - moves' in
    if max then e else -e

let ai_random_fn s moves =
  let choice = Random.int @: List.length moves in
  List.nth moves choice, s

(* Simple minimax algorithm *)
let ai_minimax eval_f max_depth s _ =
  let b = s.board in
  
  let rec loop max turn depth move =
    maybe () (fun m -> play !b (turn-1) m) move;
    let moves = expand !b turn in
    match moves, depth with
     | [], _ -> maybe () (fun m -> rewind !b (turn-1) m) move; 
             if max then Right (-100) else Right 100

     | _, d when d >= max_depth -> 
         let e = eval_f s max in (* forced to use heuristic *)
         maybe () (fun m -> rewind !b (turn-1) m) move; 
         Right (e)

     | _, _ -> 
         (* loop over all possible moves *)
         let options = list_map 
            (fun m -> loop (not max) (turn+1) (depth+1) @: Some m, m) moves in 
         (* find the best option *)
         let op = match max with true -> (>) | false -> (<) in
         let get_minmax a b = match a,b with
            (((Right y),_) as best), (((Right x),_) as m) -> 
              if op x y then m else best
            | _,_ -> failwith "Error in minimax" in
         let minmax = 
           List.fold_left get_minmax (List.hd options) (List.tl options) in
         match move, minmax with
         | None, (Right i, m) -> Left (m, i) (* return the move we chose *)
         | Some m, _          -> rewind !b (turn-1) m; fst minmax
         | _, _               -> failwith "Error2 in minimax"
  in
  match loop true s.turn 0 None with
   | Right _     -> failwith "minimax error"
   | Left (m, i) -> print_endline @: "move has value "^string_of_int i;
                  m, s

let rec get_depth () = 
  print_string "Please enter a maximum depth: ";
  try read_int ()
  with Failure _ -> get_depth ()

let ai_list = [1, RandomAI; 2, MinimaxAI]

let get_ai_fn = function
  | MinimaxAI -> 
      let d = get_depth () in
      ai_minimax evaluate_by_moves d
  | RandomAI -> ai_random_fn
  | _ -> failwith "unhandled AI"
  
let string_of_ai_list () = 
  List.fold_left 
    (fun acc (i, a) -> acc^"\n"^string_of_int i^" "^string_of_player a)
    "" ai_list


