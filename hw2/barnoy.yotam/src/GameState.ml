open Board

type player_t = BlackP | WhiteP

type playerf_t = gamestate_t -> move_t list -> gamestate_t
and gamestate_t = {
                     board: board_t ref;
                     turn: int;
                     players: playerf_t list; (* white, black *)
                   }
