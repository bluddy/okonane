open Util
open Board
open GameState

let debug = ref false
let set_debug debug_flag = debug := debug_flag; ()

let string_of_color = function BlackP -> "Black" | WhiteP -> "White"
let color_of_turn t = match t mod 2 with 0 -> WhiteP | _ -> BlackP
let other_color = function WhiteP -> BlackP | BlackP -> WhiteP

let rec loop_input_pos prompt =
  print_string prompt;
  let line = read_line () in
  match pos_of_str line with
   | None -> print_endline "Bad input\n"; loop_input_pos prompt
   | Some pos -> pos

let rec loop_input_int prompt f =
  let fail () = print_endline "Bad input\n"; loop_input_int prompt f in
  print_string prompt;
  try 
    let i = read_int () in
    if f i then i else fail ()
  with Failure _ -> fail ()

let rec main_loop olds =
  let new_turn = olds.turn + 1 in
  let s = {olds with turn = new_turn} in
  match expand !(s.board) s.turn with
   | [] -> let c = other_color @: color_of_turn olds.turn in
           print_endline @: (string_of_color c)^" Wins!!!\n";
           start_game ()
   | moves -> if !debug then List.iter (print_endline |- string_of_move) moves;
              let f = List.nth s.players @: new_turn mod 2 in
              let s' = f s moves in
              main_loop s'

and player_turn s moves =
  let board_str = string_of_board_and_coords !(s.board) in
  let rec loop_player () = 
    let invalid_move () = print_endline "Invalid move\n"; loop_player () in
    let color_str = string_of_color @: color_of_turn s.turn in
    print_endline @: "Turn:"^string_of_int s.turn^" "^color_str^" Player";
    print_endline board_str;
    let play_move m =
      if List.exists ((=) m) moves then (play !(s.board) s.turn m; s)
      else invalid_move ()
    in match s.turn with
     | 1 | 2 ->
       let x,y = loop_input_pos "Choose a piece to remove: " in
       let m = Remove(x,y) in
       play_move m
     | _ ->
       let pos1 = loop_input_pos "Choose a piece to move: " in
       let possib_moves = 
         List.filter (function Move (p,_) -> pos1 = p | _ -> false) moves in
       match possib_moves with
       | [] -> invalid_move ()
       | _  -> let dests = List.map dest_of_move possib_moves in
               let num_dests = insert_index_fst 1 dests in
               let strs = List.map 
                 (fun (i,p) -> string_of_int i^" "^abc_of_pos p) num_dests in
               let str = String.concat "\n" strs in
               print_endline str;
               let pos2_idx = loop_input_int "Choose where to move it: "
                 (fun i -> List.exists (fun (d,_) -> d=i) num_dests) in
               let pos2 = snd @: 
                 List.find (fun x -> (fst x) = pos2_idx) num_dests in
               begin match move_of_2_pos pos1 pos2 with
                | None   -> invalid_move ()
                | Some m -> play_move m
               end
  in loop_player ()

and start_game () =
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
  let rec player_loop () =
    print_string 
      "Would you like to play black(1), white(2), both(3), or neither(4)?";
    try 
      begin match read_int () with
        | 1 -> [None; Some player_turn]
        | 2 -> [Some player_turn; None] 
        | 3 -> [Some player_turn; Some player_turn]
        | 4 -> [None; None]
        | _ -> player_loop ()
      end 
    with Failure _ -> player_loop () in
  let players : playerf_t option list = player_loop () in
  let rec ai_loop c_str =
    print_endline @: AI.string_of_ai_list ();
    print_string @: "Please choose an AI for the "^c_str^" player:";
    try let choice = read_int () in
      snd @: List.assoc choice AI.ai_list
    with Failure _ -> ai_loop c_str in
  let players' = 
    List.map2 (fun p c_str -> match p with Some x -> x | None -> ai_loop c_str) 
      players ["white"; "black"] in
  main_loop {board=ref board; turn=0; players=players'}

