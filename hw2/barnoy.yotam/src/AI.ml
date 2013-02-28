open Util
open Board
open GameState

(* The possible types of players *)
type player_t = Human | RandomAI | MinimaxAI | AlphaBetaAI | TimedMinimaxAI
           | TimedAlphaBetaAI

let is_human = function Human -> true | _ -> false

let string_of_player = function
  | Human    -> "Human"
  | RandomAI -> "Random AI"
  | MinimaxAI -> "Minimax AI"
  | AlphaBetaAI -> "Alpha-beta AI"
  | TimedMinimaxAI -> "Time-limited Minimax AI"
  | TimedAlphaBetaAI -> "Time-limited Alpha-beta AI"

let ai_list = insert_idx_fst 1 
  [RandomAI; MinimaxAI; AlphaBetaAI; TimedMinimaxAI; TimedAlphaBetaAI]

(* ----- evaluation stuf ----- *)

let win_score = 10000
let ortho_move_score = 60
let material_score = 20
let move_score = 2

(* evaluate the board by the number of possible actions each player has *)
let evaluate_by_moves s max moves =
  if s.turn <= 2 then 0  (* can't evaluate such an early turn *)
  else
    let mov_num = List.length @: moves in
    let mov_num' = List.length @: expand !(s.board) @: s.turn+1 in
    let e = mov_num - mov_num' in
    if max then e else -e

(* more accurate evaluation: compares to avg of next turn's # moves *)
let evaluate_by_moves_detailed s max moves =
  if s.turn <= 2 then 0 (* can't evaluate such an early turn *)
  else
    let b = s.board and turn = s.turn in
    let total = List.fold_left 
      (fun tot m -> play !b turn m; 
        let num_moves = List.length @: expand !b (turn + 1) in
        rewind !b turn m;
        tot + num_moves)
      0 moves in
    let num_m = List.length moves in
    let num_m' = if num_m = 0 then win_score else total / num_m in
    let e = num_m - num_m' in
    if max then e else -e


(* calculate the score of a position by the number of orthogonal moves, the
 * number of pieces, and the total number of available moves *)
let score_of_pos num_ortho_moves num_pieces num_moves =
  if num_moves = 0 then (-win_score)
  else
    num_ortho_moves * ortho_move_score + 
    num_pieces * material_score +
    num_moves * move_score

let eval3 s max moves =
  let report i = if max then i else -i in
  if s.turn <= 2 then 0 (* can't evaluate such an early turn *)
  else
    let b = s.board and turn = s.turn in
    let num_pieces = get_num_squares !b @: Board.color_of_turn turn in
    if list_null moves then report (-win_score)
    else
      let num_moves = List.length moves in
      let ortho_sets = ortho_move_sets moves in
      let max_set_size = snd @: list_max ortho_sets List.length in
      let my_score = score_of_pos max_set_size num_pieces num_moves in
      let his_score = snd @: list_min moves @: 
          fun m -> with_turn !b turn m @:
            fun () -> 
              let turn' = turn + 1 in
              let his_pieces = 
                get_num_squares !b @: Board.color_of_turn turn' in
              match expand !b turn' with
              | [] -> win_score (* we win *)
              | m ->
                let num_his_moves = List.length m in
                let his_ortho_sets = ortho_move_sets m in
                let max_set_size = snd @: list_max his_ortho_sets List.length in
                score_of_pos max_set_size his_pieces num_his_moves in
      report @: my_score - his_score

(* a cheaper version of the eval function *)
let eval3_cheap s max moves =
  let report i = if max then i else -i in
  if s.turn <= 2 then 0 (* can't evaluate such an early turn *)
  else
    let b = s.board and turn = s.turn in
    let num_pieces = get_num_squares !b @: Board.color_of_turn turn in
    if list_null moves then report (-win_score)
    else
      let num_moves = List.length moves in
      let ortho_sets = ortho_move_sets moves in
      let max_set_size = snd @: list_max ortho_sets List.length in
      let my_score = score_of_pos max_set_size num_pieces num_moves in
      let turn' = turn + 1 in
      (* pretend that it's his turn for simplicity *)
      let his_pieces = get_num_squares !b @: Board.color_of_turn turn' in
      let his_moves = expand !b turn' in
      let num_his_moves = List.length his_moves in
      let his_ortho_sets = ortho_move_sets his_moves in
      let max_set_size = snd @: list_max his_ortho_sets List.length in
      let his_score = score_of_pos max_set_size his_pieces num_his_moves in
      report @: my_score - his_score

(* Default move ordering function for a-b-pruning *)
let order_random _ _ l =
  let l' = match Random.int 2 with 0 -> l | _ -> List.rev l in
  let f, b = List.fold_left 
    (fun (front,back) x -> match Random.int 2 with 
      | 0 -> x::front, back | _ -> front, x::back)
    ([],[]) l'
  in f @ List.rev b

(* A more sophisticated move-ordering scheme *)
(* The order of evaluation is:
  * 1. Moves that give you material advantage (several captures)
  * 2. Moves along the border (tend to do better)
  * 3. All other moves
*)
let order_heuristic s max (l:move_t list) : (move_t list) =
  let b = s.board in
  let cap, border, other = List.fold_left
    (fun (cap, border, other) -> function
     | Move (_,(_,i)) as m when i > 2 -> (m::cap, border, other)
     | m when is_border_move !b m     -> (cap, m::border, other)
     | m                              -> (cap, border, m::other))
    ([],[],[]) l
  in cap@border@other

(* Simple random choice algorithm *)
let ai_random s moves =
  let choice = Random.int @: List.length moves in
  s, MoveChoice (List.nth moves choice, 0, 1, true)

(* minimax algorithm *)
let ai_minimax eval_f _ max_depth timeout s _ =
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
             if max then MoveValue (-win_score, true) 
             else MoveValue (win_score, true)

       | _, d when d >= max_depth -> 
             let e = eval_f s max moves in (* forced to use heuristic *)
             rewind_move ();
             MoveValue (e, false)

       | _, _ -> 
           (* loop over all possible moves *)
           let op, def_val = match max with 
             true -> (>), -win_score | false -> (<), win_score in

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
let ai_alpha_beta eval_f m_order_f max_depth timeout s _ =
  let order_f = match m_order_f with
    None -> failwith "no order_f" | Some f -> f in
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
      let moves = order_f s max @: expand !brd turn in
      match moves, depth with
       | [], _ -> rewind_move ();
                  if max then MoveValue (-win_score, true) 
                  else MoveValue (win_score, true)
       | _, d when d >= max_depth -> 
              let e = eval_f s max moves in (* forced to use heuristic *)
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
                (alpha, List.hd moves, MoveValue(-win_score,true), []) 
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
                (beta, List.hd moves, MoveValue (win_score,true), []) 
                moves
         in 
         let seen_all = List.for_all id_fn seen_all_l in
         rewind_move (); 
         match move, last with
          | _, TimeOut -> TimeOut
          | Some _, _  -> MoveValue (i, seen_all)
          | None,   _  -> MoveChoice (m, i, !scanned, seen_all)
  in
  s, loop true s.turn 0 (-win_score,win_score) None

let ai_time_bounded ai_f time_limit eval_f m_ord_f s moves = 
  let scanned = ref 0 in
  let start_time = Sys.time () in
  let rec loop depth last_res =
    let time_limit = start_time +. (float_of_int time_limit) in
    let _, res = 
      ai_f eval_f m_ord_f depth (Some time_limit) s [] in
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

let rec get_eval () =
  let c = loop_input_int "Fast evaluation function (1), slow (2), or old (3) ?"
    (function 1 | 2 | 3 -> true | _ -> false) in
  match c with 1 -> eval3_cheap | 2 -> eval3 | _ -> evaluate_by_moves_detailed 

let rec get_ordered () =
  let c = 
    loop_input_int "Random evaluation order (1) or heuristic order (2) ?"
      (function 1 | 2 -> true | _ -> false)
  in match c with 1 -> order_random | _ -> order_heuristic 

let get_ai_fn ai = 
  let eval_f = get_eval () in
  match ai with
  | MinimaxAI -> 
      let d = get_depth () in
      ai_minimax eval_f None d None
  | AlphaBetaAI -> 
      let d = get_depth () in
      let ord_f = get_ordered () in
      ai_alpha_beta eval_f (Some ord_f) d None
  | TimedMinimaxAI -> 
      let t = get_time () in
      ai_time_bounded ai_minimax t eval_f None
  | TimedAlphaBetaAI -> 
      let t = get_time () in
      let ord_f = get_ordered () in
      ai_time_bounded ai_alpha_beta t eval_f (Some ord_f)
  | RandomAI -> ai_random
  | _ -> failwith "unhandled AI"
  
let string_of_ai_list () = 
  List.fold_left 
    (fun acc (i, a) -> acc^"\n"^string_of_int i^" "^string_of_player a)
    "" ai_list


