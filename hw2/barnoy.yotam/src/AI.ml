open Util
open Board
open GameState

type player_t = Human | RandomAI | MinimaxAI | AlphaBetaAI | TimedMinimaxAI
           | TimedAlphaBetaAI

let ai_list = insert_idx_fst 1 
  [RandomAI; MinimaxAI; AlphaBetaAI; TimedMinimaxAI; TimedAlphaBetaAI]

let is_human = function Human -> true | _ -> false

let string_of_player = function
  | Human    -> "Human"
  | RandomAI -> "Random AI"
  | MinimaxAI -> "Minimax AI"
  | AlphaBetaAI -> "Alpha-beta AI"
  | TimedMinimaxAI -> "Time-limited Minimax AI"
  | TimedAlphaBetaAI -> "Time-limited Alpha-beta AI"

(* evaluate the board by the number of possible actions each player has *)
let evaluate_by_moves s max =
  if s.turn <= 2 then 0 (* can't evaluate such an early turn *)
  else
    let moves = List.length @: expand !(s.board) s.turn in
    let moves' = List.length @: expand !(s.board) @: s.turn+1 in
    let e = moves - moves' in
    if max then e else -e

(* more accurate evaluation: compares to avg of next turn's # moves *)
let evaluate_by_moves_detailed s max =
  if s.turn <= 2 then 0 (* can't evaluate such an early turn *)
  else
    let b = s.board and turn = s.turn in
    let moves = expand !b turn in
    let total = List.fold_left 
      (fun tot m -> play !b turn m; 
        let num_moves = List.length @: expand !b (turn + 1) in
        rewind !b turn m;
        tot + num_moves)
      0 moves in
    let num_m = List.length moves in
    let num_m' = if num_m = 0 then 100 else total / num_m in
    let e = num_m - num_m' in
    if max then e else -e

(* Simple random choice algorithm *)
let ai_random s moves =
  let choice = Random.int @: List.length moves in
  s, MoveChoice (List.nth moves choice, 0, 1, true)

(* minimax algorithm *)
let ai_minimax eval_f max_depth timeout s _ =
  let brd = s.board in
  let scanned = ref 0 in
  let rec loop max turn depth move =
    match timeout with
    | Some time when Sys.time () >= time -> TimeOut
    | _ ->
      scanned := !scanned + 1;
      let play_move () = maybe () (play !brd (turn-1)) move in
      let rewind_move () = maybe () (rewind !brd (turn-1)) move in

      play_move ();
      let moves = expand !brd turn in
      match moves, depth with
       | [], _ -> rewind_move ();
             if max then MoveValue (-100, true) else MoveValue (100, true)

       | _, d when d >= max_depth -> 
             let e = eval_f s max in (* forced to use heuristic *)
             rewind_move ();
             MoveValue (e, false)

       | _, _ -> 
           (* loop over all possible moves *)
           let op, def_val = match max with 
             true -> (>), -1000 | false -> (<), 1000 in
           let (i, m, last, seen_all_l) = foldl_until
             (fun (best_val, best_move, _, acc) move ->
               match loop (not max) (turn+1) (depth+1) (Some move) with
               | (MoveValue (r, all)) as ret -> 
                   if op r best_val then r, move, ret, all::acc 
                   else best_val, best_move, ret, all::acc
               | TimeOut -> best_val, best_move, TimeOut, [false]
               | _       -> failwith "error in minimax")
             (fun (_,_,last,_) _ -> last = TimeOut)
             (def_val, List.hd moves, MoveValue (def_val,true), []) 
             moves 
           in
           let seen_all = List.for_all id_fn seen_all_l in
           rewind_move ();
           match move, last with
            | _, TimeOut -> TimeOut
            | None, _    -> MoveChoice (m, i, !scanned, seen_all)
            | Some _, _  -> MoveValue (i, seen_all)
  in
  s, loop true s.turn 0 None 

(* Alpha-beta pruning algorithm *)
let ai_alpha_beta eval_f max_depth timeout s _ =
  let brd = s.board in
  let scanned = ref 0 in

  let rec loop max turn depth (alpha, beta) move =
    match timeout with
    | Some time when Sys.time () >= time -> TimeOut
    | _ ->
      scanned := !scanned + 1;
      let play_move () = maybe () (play !brd (turn-1)) move in
      let rewind_move () = maybe () (rewind !brd (turn-1)) move in

      play_move ();
      let moves = expand !brd turn in
      match moves, depth with
       | [], _ -> rewind_move ();
                  if max then MoveValue (-100, true) else MoveValue (100, true)
       | _, d when d >= max_depth -> 
              let e = eval_f s max in (* forced to use heuristic *)
              rewind_move ();
              MoveValue (e, false)
       | _, _ -> 
         let (i, m, last, seen_all_l) = if max then
             foldl_until
                (fun (a,n,_,acc) m -> 
                  match loop false (turn+1) (depth+1) (a,beta) (Some m) with
                  | MoveValue (x,all) as ret -> 
                      if x > a then x,m,ret,all::acc else a,n,ret,all::acc
                  | TimeOut              -> a,n,TimeOut,[false]
                  | _ -> failwith "error")
                (fun (a,_,last,_) _ -> beta <= a || last = TimeOut)
                (alpha, List.hd moves, MoveValue(-200,true), []) 
                moves
           else 
             foldl_until
                (fun (b,n,_,acc) m -> 
                  match loop true (turn+1) (depth+1) (alpha,b) (Some m) with
                  | MoveValue (x,all) as ret -> 
                      if x < b then x,m,ret,all::acc else b,n,ret,all::acc
                  | TimeOut              -> b,n,TimeOut,[false]
                  | _ -> failwith "error")
                (fun (b,_,last,_) _ -> b <= alpha || last = TimeOut)
                (beta, List.hd moves, MoveValue (200,true), []) 
                moves
         in 
         let seen_all = List.for_all id_fn seen_all_l in
         rewind_move (); 
         match move, last with
          | _, TimeOut -> TimeOut
          | Some _, _  -> MoveValue (i, seen_all)
          | None,   _  -> MoveChoice (m, i, !scanned, seen_all)
  in
  s, loop true s.turn 0 (-200,200) None

let ai_time_bounded ai_f time_limit eval_f s moves = 
  let scanned = ref 0 in
  let start_time = Sys.time () in
  let rec loop depth last_res =
    let time_limit = start_time +. (float_of_int time_limit) in
    let _, res = 
      ai_f eval_f depth (Some time_limit) s [] in
    match res with
     | TimeOut -> s, last_res
     | MoveChoice (move, value, num_scanned, true) -> 
         (* we've explored the whole tree *)
         scanned := !scanned + num_scanned; 
         s, MoveChoice (move, value, !scanned, true)
     | MoveChoice (move, value, num_scanned, false) -> 
         scanned := !scanned + num_scanned; 
         loop (depth+1) @: MoveChoice (move, value, !scanned, false)
     | _ -> failwith "error in ai_time_bounded"
  in loop 1 @: MoveChoice (List.hd moves, 0, 0, false) (* a crappy default choice *)

let rec get_depth () = 
  loop_input_int "Enter a maximum depth: " (fun i -> i > 0)

let rec get_time () =
  loop_input_int "Enter max time in seconds: " (fun i -> i > 0)

let get_ai_fn ai = 
  let eval_f = evaluate_by_moves_detailed in
  match ai with
  | MinimaxAI -> 
      let d = get_depth () in
      ai_minimax eval_f d None
  | AlphaBetaAI -> 
      let d = get_depth () in
      ai_alpha_beta eval_f d None
  | TimedMinimaxAI -> 
      let t = get_time () in
      ai_time_bounded ai_minimax t eval_f
  | TimedAlphaBetaAI -> 
      let t = get_time () in
      ai_time_bounded ai_alpha_beta t eval_f
  | RandomAI -> ai_random
  | _ -> failwith "unhandled AI"
  
let string_of_ai_list () = 
  List.fold_left 
    (fun acc (i, a) -> acc^"\n"^string_of_int i^" "^string_of_player a)
    "" ai_list


