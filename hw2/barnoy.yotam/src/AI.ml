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
    maybe () (fun m -> play !b turn m) move;
    let moves = expand !b turn in
    match moves with
     | [] -> maybe () (fun m -> rewind !b turn m) move; 
             if max then (-1,move) else (1,move) (* Utility of winning/losing *)
     | _  -> 
         let options = list_map 
              (fun m -> loop (not max) (turn+1) @: Some m) moves in 
         (* find the best option *)
         let op = match max with true -> (>) | false -> (<) in
         let minmax = List.fold_left 
                        (fun ((y,_) as best) ((x,_) as m) ->
                          if op x y then m else best) 
                        (List.hd options) 
                        (List.tl options) in
         maybe () (fun m -> rewind !b turn m) move;
         minmax
  in
  match loop true s.turn None with
   | _, None -> failwith "error, no move found by minimax"
   | _, Some x -> x, s

let ai_list = [
               1, (RandomAI, ai_random_fn);
               2, (MinimaxAI, ai_minimax)
              ]


let string_of_ai_list () = 
  List.fold_left 
    (fun acc (i, (a,_)) -> acc^"\n"^string_of_int i^" "^string_of_player a)
    "" ai_list


