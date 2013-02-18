open Util
open Board

type player_t = BlackP | WhiteP

type gamestate_t = {
                     board: board_t;
                     player: player_t;
                     turn: int;
                   }

let debug = ref false

let string_of_color = function BlackP -> "Black" | WhiteP -> "White"
let color_of_turn t = match t mod 2 with 0 -> WhiteP | _ -> BlackP

let rec player_turn s =
  let moves = expand s.board s.turn in
  if !debug then List.iter (print_endline |- string_of_move) moves;
  let board_str = string_of_board_and_coords s.board in
  let rec loop_player () = 
    let invalid_move () = print_endline "Invalid move\n"; loop_player () in
    let color_str = string_of_color @: color_of_turn s.turn in
    print_endline @: "Turn:"^string_of_int s.turn^" "^color_str^" Player";
    print_endline board_str;
    let rec loop_input str =
      print_string str;
      let line = read_line () in
      begin match pos_of_str line with
       | None -> print_endline "Bad input\n"; loop_input str
       | Some pos -> pos
      end
    in
    let play_move m =
       if List.exists ((=) m) moves then 
         let new_s = {s with turn = s.turn + 1} in
         play s.board s.turn m;
         ai_turn new_s
       else invalid_move ()
    in match s.turn with
     | 1 | 2 ->
       let query = "Choose a piece to remove: " in
       let x,y = loop_input query in
       let m = Remove(x,y) in
       play_move m
     | _ ->
       let pos1 = loop_input "Choose a piece to move: " in
       let pos2 = loop_input "Choose where to move it: " in
       begin match move_of_2_pos pos1 pos2 with
        | None -> invalid_move ()
        | Some m -> play_move m
       end
  in loop_player ()

and ai_turn s = player_turn s
       
let start_game debug_flag =
  debug := debug_flag;
  print_endline "Welcome to Konane!\n";
  let rec board_loop () : board_t =
    print_string "How big of a board would you like? (4/6/8)";
    try
      let size = read_int () in
      begin match Board.make_default size with
      | None   -> print_endline "Bad size"; board_loop ()
      | Some b -> b 
      end 
    with Failure _ -> board_loop () in
  let board = board_loop () in
  let rec bw_loop () =
    print_string "Would you like to play black(1) or white(2)?";
    try 
      begin match read_int () with
      | 1 -> BlackP | 2 -> WhiteP | _ -> bw_loop ()
      end 
    with Failure _ -> bw_loop () in
  let bw_choice = bw_loop () in
  let f = match bw_choice with BlackP -> player_turn | WhiteP -> ai_turn in
  f {board=board; player=bw_choice; turn=1}

