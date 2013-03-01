open Util
open Board

type player_color_t = BlackP | WhiteP

type seen_all_t = bool
type move_value_t = int
type nodes_seen_t = int
type max_depth_t = int
type return_t = MoveChoice of move_t * move_value_t * nodes_seen_t * seen_all_t * max_depth_t
              | MoveValue of move_value_t * seen_all_t
              | TimeOut

type 'a playerf_t = 
  'a gamestate_t -> move_t list -> 'a gamestate_t * return_t

and 'a gamestate_t = {
                     board: board_t ref;
                     turn: int;
                     players: ('a * 'a playerf_t) list; (* white, black *)
                   }

let current_player s = List.nth s.players @: s.turn mod 2

let string_of_color = function BlackP -> "Black" | WhiteP -> "White"
let color_of_turn t = match t mod 2 with 0 -> WhiteP | _ -> BlackP
let other_color = function WhiteP -> BlackP | BlackP -> WhiteP

let rec loop_input_int prompt f =
  let fail () = print_endline "Bad input\n"; loop_input_int prompt f in
  print_string prompt;
  try 
    let i = read_int () in
    if f i then i else fail ()
  with Failure _ -> fail ()

let rec loop_input_float prompt f =
  let fail () = print_endline "Bad input\n"; loop_input_float prompt f in
  print_string prompt;
  try 
    let i = read_float () in
    if f i then i else fail ()
  with Failure _ -> fail ()
