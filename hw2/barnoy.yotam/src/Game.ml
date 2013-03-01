open Util
open Board
open GameState
open AI

type stat_t = {w_nodes : int; 
               b_nodes : int; 
               w_depth: int; 
               b_depth: int;
               w_wins : int;
               b_wins : int }

let empty_stat = {w_nodes=0; b_nodes=0; w_depth=0; b_depth=0; w_wins=0; b_wins=0}

let add_stat a b = {w_nodes = a.w_nodes + b.w_nodes;
                    b_nodes = a.b_nodes + b.b_nodes;
                    w_depth = a.w_depth + b.w_depth;
                    b_depth = a.b_depth + b.b_depth;
                    w_wins = a.w_wins + b.w_wins;
                    b_wins = a.b_wins + b.b_wins; }

let debug = ref false
let set_debug debug_flag = debug := debug_flag; ()

let print_stats s =
  let games = s.w_wins + s.b_wins in
  let p_win str x = print_endline @: str^" wins: "^string_of_int x in
  p_win "White" s.w_wins; p_win "Black" s.b_wins;
  let avg x = (float_of_int x) /. (float_of_int games) in
  let avg_w_nodes = avg s.w_nodes and avg_b_nodes = avg s.b_nodes and
      avg_w_depth = avg s.w_depth and avg_b_depth = avg s.b_depth in
  let p_nodes col str1 x y = 
    print_endline @: col^ str1^string_of_int x^", avg: "^string_of_float y in
  p_nodes "White" " nodes checked: " s.w_nodes avg_w_nodes; 
  p_nodes "Black" " nodes checked: " s.b_nodes avg_b_nodes;
  p_nodes "White" " total depth: " s.w_depth avg_w_depth; 
  p_nodes "Black" " total depth: " s.b_depth avg_b_depth;
  ()

(* Fill in all non-human players *)
let modify_ai players =
  let ai_loop str =
    let prompt = AI.string_of_ai_list ()^
        "\nPlease choose an AI for the "^str^" player:" in
    let choice = loop_input_int prompt (fun i -> List.mem_assoc i AI.ai_list) in
    let ai = List.assoc choice AI.ai_list in
    let f = get_ai_fn ai in
    (ai, f)
  in
  List.map2 (fun p c_str -> 
              match p with (Human,_) as x -> x | _ -> ai_loop c_str) 
    players ["white"; "black"]

(* parse options we may want to change during runtime *)
let parse_options s = function
  | "z" -> Some {s with players=(modify_ai s.players)}
  | _   -> None

let print_board s =
  let board_str = string_of_board_and_coords !(s.board) in
   print_endline board_str; ()

let print_turn_and_board s =
   let color_str = string_of_color @: color_of_turn s.turn in
   let (player,_) = current_player s in
   let turn_str = "Turn:"^string_of_int s.turn^" "^color_str^" Player"^
     "("^string_of_player player^")" in
   print_endline turn_str; print_board s; ()


(* handles a player turn *)
let rec player_turn state moves =
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
      if List.exists ((=) m) moves then !sref, MoveChoice(m,0,1,true,1) 
      else invalid_move ()
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

(* The main loop of a single game *)
let main_loop silent state : stat_t =
  let rec loop olds st : stat_t =
    let s = {olds with turn = olds.turn + 1} in
    match expand !(s.board) s.turn with
     | [] -> let color = color_of_turn olds.turn in (* color of last turn *)
             let color_s = string_of_color color in
             print_newline ();
             print_board s;
             print_endline @: "\n---- "^color_s^" Wins!!!\n";
             begin match color with
              | BlackP -> {st with b_wins = 1}
              | WhiteP -> {st with w_wins = 1}
             end
     | moves -> 
         if !debug then List.iter (print_endline |- string_of_move) moves;
         let color = color_of_turn s.turn in
         let color_str = string_of_color color in
         let player, player_fn = current_player s in
         if not silent then print_turn_and_board s;
         let time = Sys.time () in

         (* get ai/player's moves *)
         begin match player_fn s moves with
         | new_s, MoveChoice(move, value, num_seen, _, depth) ->
             (if not @: is_human player then 
               let delta = Sys.time () -. time in
               (print_endline @: "Considered "^string_of_int num_seen^
               " moves in "^string_of_float delta^" seconds. Move has value "^
                string_of_int value);
             (* double check that we got a legit move back *)
             if not @: List.exists ((=)move) moves then 
               (print_endline @: "Illegal move: "^
               string_of_move move; print_board new_s);
             play !(new_s.board) new_s.turn move;
             print_endline @: color_str^" "^string_of_move move;
             print_newline ());
             begin match color with
              | WhiteP -> loop new_s 
                {st with w_nodes = st.w_nodes + num_seen;
                         w_depth = max st.w_depth depth}
              | BlackP -> loop new_s 
                {st with b_nodes = st.b_nodes + num_seen; 
                         b_depth = max st.b_depth depth}
             end
         | _, _ -> failwith "player returned bad value"
         end
  in loop state empty_stat

(* This is where the game starts *)
let rec start_game silent =
  print_endline "Welcome to Konane!\n";
  let board_size = loop_input_int "How big of a board would you like? (4/6/8)"
    (function 4 | 6 | 8 -> true | _ -> false) in
  let player_input = loop_input_int 
    "Would you like to play black(1), white(2), both(3), or neither(4)?"
    (function 1 | 2 | 3 | 4 -> true | _ -> false) in
  let human = Human, player_turn and default = RandomAI, ai_random  in
  let players' = match player_input with
      | 1 -> [default; human]
      | 2 -> [human; default] 
      | 3 -> [human; human]
      | 4 -> [default; default]
      | _ -> failwith "start_game error" in
  (* get requested ai players *)
  let players = modify_ai players' in
  let num_games = if silent 
    then loop_input_int "How many games would you like? " (fun i -> i > 0)
    else 1 in

  (* actually execute the game(s) and add up stats *)
  let stats = List.fold_left 
    (fun statAcc _ -> 
      let board = Board.make_default board_size in
      let state = {board=ref board; turn=0; players=players} in
      (* call main loop *)
      let stat = main_loop silent state in
      add_stat statAcc stat)
    empty_stat @: create_range 0 num_games
  in
  print_stats stats; ()

