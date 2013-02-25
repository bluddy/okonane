open Util
open Board

type player_t = BlackP | WhiteP

type 'a playerf_t = 
  'a gamestate_t -> move_t list -> move_t * 'a gamestate_t * int

and 'a gamestate_t = {
                     board: board_t ref;
                     turn: int;
                     players: ('a * 'a playerf_t) list; (* white, black *)
                   }

let current_player s = List.nth s.players @: s.turn mod 2

let string_of_color = function BlackP -> "Black" | WhiteP -> "White"
let color_of_turn t = match t mod 2 with 0 -> WhiteP | _ -> BlackP
let other_color = function WhiteP -> BlackP | BlackP -> WhiteP

