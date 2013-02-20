open Util
open Board
open GameState

type ai_t = RandomAI

let string_of_ai = function
  | RandomAI -> "Random AI"

let ai_random_fn s moves =
  let choice = Random.int @: List.length moves in
  play !(s.board) s.turn @: List.nth moves choice; 
  s

let ai_list = [1, (RandomAI, ai_random_fn)]

let string_of_ai_list () = 
  List.fold_left 
    (fun acc (i, (a,_)) -> acc^"\n"^string_of_int i^" "^string_of_ai a)
    "" ai_list


