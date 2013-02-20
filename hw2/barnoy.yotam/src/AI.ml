open Util
open Board
open GameState

type player_t = Human 
           | RandomAI

let is_human = function Human -> true | _ -> false

let string_of_player = function
  | Human    -> "Human"
  | RandomAI -> "Random AI"

let ai_random_fn s moves =
  let choice = Random.int @: List.length moves in
  List.nth moves choice, s

let ai_list = [1, (RandomAI, ai_random_fn)]

let string_of_ai_list () = 
  List.fold_left 
    (fun acc (i, (a,_)) -> acc^"\n"^string_of_int i^" "^string_of_player a)
    "" ai_list


