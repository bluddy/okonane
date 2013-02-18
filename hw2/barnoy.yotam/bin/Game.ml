open Util
open Board

type gamestate_t = {
                     board: board_t;
                     player: square_t;
                     turn: int;
                   }


let player_turn s =
  let moves = expand b s.turn in
  let board_str = string_of_board_and_coords s.board in
  let loop_player () = 
    let invalid_move () = print_endline "Invalid move\n"; loop_player () in
    print_endline "Turn: Player";
    print_endline board_str;
    let loop_input str =
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
       let m = Remove(loop_input query) in
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
       
let start_game () =
  print_endline "Welcome to Konane!\n";
  let rec board_loop () =
    print_string "How big of a board would you like? (4/6/8)";
    let size = try read_int () with Failure _ -> board_loop () in
    match Board.make_default size with
    | None   -> print_endline "Bad size"; board_loop ()
    | Some b -> b in
  let board = board_loop () in
  let rec bw_loop () =
    print_string "Would you like to play black(1) or white(2)?";
    try 
      begin match read_int () with
      | 1 -> Black | 2 -> White | _ -> bw_loop ()
      end 
    with Failure _ -> bw_loop () in
  let bw_choice = bw_loop () in
  let f = match bw_choice with Black -> player_turn | White -> ai_turn in
  f {board=board; player=bw_choice; turn=1}

