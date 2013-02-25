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

let ai_random_fn s moves =
  let choice = Random.int @: List.length moves in
  List.nth moves choice, s

(* Simple minimax algorithm *)
let ai_minimax s _ =
  let b = s.board in
  let rec loop max turn move =
    maybe () (fun m -> play !b (turn-1) m) move;
    let moves = expand !b turn in
    match moves with
     | [] -> maybe () (fun m -> rewind !b (turn-1) m) move; 
             if max then Right (-1) else Right 1 (* Utility of winning/losing *)
     | _  -> 
         let options = list_map 
              (fun m -> loop (not max) (turn+1) @: Some m, m) moves in 
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
  match loop true s.turn None with
   | Right _     -> failwith "error, no move found by minimax"
   | Left (m, i) -> print_endline @: "move has value "^string_of_int i;
                  m, s

let ai_list = [
               1, (RandomAI, ai_random_fn);
               2, (MinimaxAI, ai_minimax)
              ]


let string_of_ai_list () = 
  List.fold_left 
    (fun acc (i, (a,_)) -> acc^"\n"^string_of_int i^" "^string_of_player a)
    "" ai_list


