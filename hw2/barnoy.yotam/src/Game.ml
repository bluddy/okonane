open Util
open Board
open GameState
open AI

let debug = ref false
let set_debug debug_flag = debug := debug_flag; ()

let rec loop_input_int prompt f =
  let fail () = print_endline "Bad input\n"; loop_input_int prompt f in
  print_string prompt;
  try 
    let i = read_int () in
    if f i then i else fail ()
  with Failure _ -> fail ()

let modify_ai s =
  let players = s.players in
  let rec ai_loop c_str =
    print_endline @: AI.string_of_ai_list ();
    print_string @: "Please choose an AI for the "^c_str^" player:";
    try let choice = read_int () in
        let ai = List.assoc choice AI.ai_list in
        let ai_fn = get_ai_fn ai in
        (ai, ai_fn)
    with Failure _ -> ai_loop c_str in
  (* Fill in all non-human players *)
  let players' = List.map2 
    (fun p c_str -> match p with 
       | (Human,_) as x -> x 
       | _              -> ai_loop c_str) 
    players ["white"; "black"] in
  {s with players = players'}

(* parse options we may want to change during runtime *)
let parse_options s = function
  | "z" -> Some (modify_ai s)
  | _   -> None

let print_board s =
  let board_str = string_of_board_and_coords !(s.board) in
   print_endline board_str; ()

let print_turn_and_board s =
   let color_str = string_of_color @: color_of_turn s.turn in
   let (player, _) = current_player s in
   let turn_str = "Turn:"^string_of_int s.turn^" "^color_str^" Player"^
     "("^string_of_player player^")" in
   print_endline turn_str; print_board s; ()

let rec main_loop olds =
  let new_turn = olds.turn + 1 in
  let s = {olds with turn = new_turn} in
  match expand !(s.board) s.turn with
   | [] -> let color = string_of_color @: color_of_turn olds.turn in
           print_newline ();
           print_board s;
           print_endline @: "\n---- "^color^" Wins!!!\n";
           start_game ()
   | moves -> 
       if !debug then List.iter (print_endline |- string_of_move) moves;
       let color_str = string_of_color @: color_of_turn s.turn in
       let (player, player_fn) = current_player s in
       print_turn_and_board s;
       (* get ai/player's moves *)
       let move, new_s = player_fn s moves in
       (* double check that we got a legit move back *)
       if not @: List.exists ((=)move) moves then 
         (print_endline @: "Illegal move: "^
         string_of_move move; print_board new_s);
       play !(new_s.board) new_s.turn move;
       print_endline @: color_str^" "^string_of_move move;
       print_newline ();
       main_loop new_s 

and player_turn state moves =
  let turn = state.turn in
  let sref = ref state in
  let rec loop_input prompt =
    print_string @: "Choose a piece to "^prompt^" (z configures AI): ";
    let line = read_line () in
    match pos_of_str line with
     | None -> begin match parse_options !sref line with
                | None -> print_endline "Bad input\n"; loop_input prompt
                | Some s -> sref := s; 
                  print_turn_and_board s; loop_input prompt 
               end
     | Some pos -> pos in
  let rec loop_player () = 
    let invalid_move () = print_endline "Invalid move\n"; loop_player () in
    let play_move m =
      if List.exists ((=) m) moves then m,!sref else invalid_move ()
    in match turn with
     | 1 | 2 ->
       let x,y = loop_input "remove" in
       let m = Remove(x,y) in
       play_move m
     | _ ->
       let pos1 = loop_input "move" in
       let possib_moves = 
         List.filter (function Move (p,_) -> pos1 = p | _ -> false) moves in
       match possib_moves with
       | [] -> invalid_move ()
       | _  -> let dests = List.map dest_of_move possib_moves in
               let num_dests = insert_idx_fst 1 dests in
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
      let human = Human, player_turn in
      let default = RandomAI, get_ai_fn RandomAI in
      begin match read_int () with
        | 1 -> [default; human]
        | 2 -> [human; default] 
        | 3 -> [human; human]
        | 4 -> [default; default]
        | _ -> player_loop ()
      end 
    with Failure _ -> player_loop () in
  let players = player_loop () in
  let s = {board=ref board; turn=0; players=players} in
  main_loop (modify_ai s)

