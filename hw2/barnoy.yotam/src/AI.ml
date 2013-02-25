open Util
open Board
open GameState

type player_t = Human 
           | RandomAI
           | MinimaxAI
           | AlphaBetaAI

let ai_list = [1, RandomAI; 2, MinimaxAI; 3, AlphaBetaAI]

let is_human = function Human -> true | _ -> false

let string_of_player = function
  | Human    -> "Human"
  | RandomAI -> "Random AI"
  | MinimaxAI -> "Minimax AI"
  | AlphaBetaAI -> "Alpha-beta AI"

(* evaluate the board by the number of possible actions each player has *)
let evaluate_by_moves s max =
  if s.turn <= 2 then 0 (* can't evaluate such an early turn *)
  else
    let moves = List.length @: expand !(s.board) s.turn in
    let moves' = List.length @: expand !(s.board) @: s.turn+1 in
    let e = moves - moves' in
    if max then e else -e

(* Simple random choice algorithm *)
let ai_random_fn s moves =
  let choice = Random.int @: List.length moves in
  List.nth moves choice, s

(* minimax algorithm *)
let ai_minimax eval_f max_depth s _ =
  let b = s.board in
  let scanned = ref 0 in
  let rec loop max turn depth move =
    scanned := !scanned + 1;
    maybe () (fun m -> play !b (turn-1) m) move;
    let moves = expand !b turn in
    match moves, depth with
     | [], _ -> maybe () (fun m -> rewind !b (turn-1) m) move; 
             if max then Right (-100) else Right 100

     | _, d when d >= max_depth -> 
         let e = eval_f s max in (* forced to use heuristic *)
         maybe () (fun m -> rewind !b (turn-1) m) move; 
         Right (e)

     | _, _ -> 
         (* loop over all possible moves *)
         let options = list_map 
            (fun m -> loop (not max) (turn+1) (depth+1) @: Some m, m) moves in 
         (* find the best option *)
         let op = match max with true -> (>) | false -> (<) in
         let get_minmax a b = match a,b with
            (((Right y),_) as best), (((Right x),_) as m) -> 
              if op x y then m else best
            | _,_ -> failwith "Error in minimax" in
         let minmax = 
           List.fold_left get_minmax (List.hd options) (List.tl options) in
         match move, minmax with
         | None, (Right i, m) -> Left (m, i, !scanned)
         | Some m, _          -> rewind !b (turn-1) m; fst minmax
         | _, _               -> failwith "Error2 in minimax"
  in
  match loop true s.turn 0 None with
   | Right _ -> failwith "minimax error"
   | Left (m, i, num) -> print_endline @: 
      "Considered "^string_of_int num^" moves. Move has value "^string_of_int i;
      m, s

(* Alpha-beta pruning algorithm *)
let ai_alpha_beta eval_f max_depth s _ =
  let brd = s.board in
  let scanned = ref 0 in

  let rec loop max turn depth (alpha, beta) move =
    scanned := !scanned + 1;
    let play_move () = maybe () (fun m -> play !brd (turn-1) m) move in
    let rewind_move () = maybe () (fun m -> rewind !brd (turn-1) m) move in

    play_move ();
    let moves = expand !brd turn in
    match moves, depth with
     | [], _ -> rewind_move ();
             if max then Right (-100) else Right 100

     | _, d when d >= max_depth -> 
         let e = eval_f s max in (* forced to use heuristic *)
         rewind_move ();
         Right (e)

     | _, _ -> 
       let (i, m) = if max then
           foldl_until
              (fun (a,n) m -> 
                match loop false (turn+1) (depth+1) (a,beta) (Some m) with
                | Right x -> if x > a then x,m else a,n
                | _ -> failwith "error")
              (fun (a,_) _ -> beta <= a)
              (alpha, List.hd moves) moves
         else 
           foldl_until
              (fun (b,n) m -> 
                match loop true (turn+1) (depth+1) (alpha,b) (Some m) with
                | Right x -> if x < b then x,m else b,n
                | _ -> failwith "error")
              (fun (b,_) _ -> b <= alpha)
              (beta, List.hd moves) moves
       in rewind_move (); 
       (* return either the move we chose or just the value *)
       maybe (Left (m, i, !scanned)) (fun _ -> Right i) move
  in
  match loop true s.turn 0 (-200,200) None with
   | Left (m, i, num) -> print_endline @: 
     "Considered "^string_of_int num^" moves. Move has value "^string_of_int i;
                  m, s
   | _ -> failwith "alphabeta error"

let rec get_depth () = 
  print_string "Please enter a maximum depth: ";
  try read_int ()
  with Failure _ -> get_depth ()

let get_ai_fn = function
  | MinimaxAI -> 
      let d = get_depth () in
      ai_minimax evaluate_by_moves d
  | AlphaBetaAI -> 
      let d = get_depth () in
      ai_alpha_beta evaluate_by_moves d
  | RandomAI -> ai_random_fn
  | _ -> failwith "unhandled AI"
  
let string_of_ai_list () = 
  List.fold_left 
    (fun acc (i, a) -> acc^"\n"^string_of_int i^" "^string_of_player a)
    "" ai_list


