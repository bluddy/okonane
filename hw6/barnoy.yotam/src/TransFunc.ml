(* Transition function *)
open Util
open Pos
open WorldMap
open State

module StateSet = Set.Make(
  struct 
    type t = state_t * float
    let compare = Pervasives.compare
  end)

type trans_fn_t = {
  hard_crash_b : bool;      (* hard crash resets to starting line *)
  outcomes : StateSet.t;
}

let new_trans_fn hard_crash : trans_fn_t = 
  {hard_crash_b=hard_crash; outcomes=StateSet.empty}

let max_speed = 5
let max_velocity = max_speed
let min_velocity = (-max_speed)

(* get the outcomes of a hard crash *)
(* we can end up in any start position *)
let hard_crash_outcomes world =
  let ps = world.pos_start in
  let prob = 1. /. (foi @: List.length ps) in
  list_map (fun pos -> (pos, (0,0)), prob) world.pos_start

(* oldstate -> newstate -> resulting states and probs *)
let crash_filter world tfunc oldstate ((newpos, (vx, vy)) as newstate) =
  let ((oldx, oldy) as oldpos) = fst oldstate in
  (* see if crash occurred *)
  if terrain_at world oldpos = Wall then hard_crash_outcomes world else
  let increments = max (abs vx) (abs vy) in
  (* Special case - not moving.  (i/increments==NaN) *)
  match increments, terrain_at world newpos with
  | 0, Wall -> hard_crash_outcomes world
  | 0, _    -> [newstate, 1.]
  | _, _    -> 
    let dx, dy = foi vx /. foi increments, foi vy /. foi increments in
    let rec loop i ((x,y) as p) =
      begin match i, terrain_at world (round_pos p) with
      | i, _ when i >= increments -> None
      | _, Wall -> Some (round_pos p)
      | i, _    -> loop (i+1) (x+.dx, y+.dy)
      end in
    match loop 0 (foi oldx, foi oldy) with
    | Some p when tfunc.hard_crash_b -> hard_crash_outcomes world
    | Some p -> (* stop agent at the crash site *)
                [(p, (0,0)), 1.]
    | None   -> [newstate, 1.]

(* return a list of states and probabilities *)
let transition world tfunc state action : (state_t * float) list =
  let t = WorldMap.terrain_at world (fst state) in
  if t = Wall then hard_crash_outcomes world
  else
    let slip_prob = match t with
    | Ground | Start | Finish -> 0.1
    | _ (* Rough *) -> 0.6 in

    (* calculate successful acceleration *)
    let pos, vel = fst state, snd state in
    let new_vel = add_pair_bound vel action max_velocity in
    let new_pos = add_pair pos new_vel in
    let no_slip_state = new_pos, new_vel in
    let slip_state = add_pair pos vel, vel in (* no change in vel *)
    
    (* reprocess for crash *)
    let modify_p p (a,b) = (a, b *. p) in
    let slip = crash_filter world tfunc state slip_state in
    let no_slip = crash_filter world tfunc state no_slip_state in 
    list_map (modify_p slip_prob) slip @
    list_map (modify_p (1. -. slip_prob)) no_slip
    
