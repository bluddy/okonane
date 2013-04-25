(* Implements the world map *)
open Util
open Pos

type terrain_t = Ground | Rough | Wall | Start | Finish

let string_of_terrain = function
  | Ground -> "."
  | Rough  -> ","
  | Wall   -> "#"
  | Start  -> "S"
  | Finish -> "F"

let terrain_of_string = function
  | "." -> Ground
  | "," -> Rough
  | "#" -> Wall
  | "S" -> Start
  | "F" -> Finish
  | x   -> failwith "Character "^x^" doesn't match terrain types"

type worldmap_t = {
  size : pos_t;
  data : terrain_t array;
  pos_start : pos_t list;
  pos_finish : pos_t list;
}

let terrain_at m (x,y) =
  let t = m.terrain in
  if x < 0 || y < 0 || Array.length t <= y then None
  else 
    let a = t.(y) in
    if Array.length a <= x then None
    else a.(x)
      
let string_of_mapdata m = 
  matrix_fold (fun acc (pos,x) -> 
      let newline = if fst pos = 0 && snd pos <> 0 then "\n" else "" in
      let c = string_of_terrain x in
      acc^newline^c)
    ""
    m

let mapdata_of_lines lines =
  let ls = list_map (fun line ->
    List.rev @: fst @: 
      iterate_until
        (fun (acc,str) -> 
          let c = string_take 1 str in
          let t = terrain_of_string c in
          t::acc, string_drop 1 str)
        (fun (_,str) -> str = "")
        line
  )
  lines in
  matrix_of_lists ls

let mapdata_of_string str = mapdata_of_lines @: string_lines str

(* obtain a full map structure from a string *)
let map_of_string str =
  let lines = string_lines str in
  let reg = Str.regexp "\([0-9]+\),\([0-9]+\)" in
  if not Str.string_match reg @: list_head lines 
  then failwith "Bad format" else
  let rows_s, cols_s = Str.matched_group 1 str, Str.matched_group 2 str in
  let rows, cols = ios rows_s, ios cols_s in
  let data = mapdata_of_lines @: list_tail lines in
  let start,finish = matrix_fold (fun (s,f) (pos,d) -> 
      match d with 
      | Start  -> pos::s,f
      | Finish -> s, pos::f
      | _      -> s, f)
    data in
  if list_null start then failwith "Must have a start position" else
  if list_null finish then failwith "Must have a finish position" else
  {size=cols,rows; data=data; pos_start=start; pos_finish=finish}

(* choose randomly from the possible start states *)
let get_random_start_state world =
  let ps = world.pos_start in
  (MyRandom.random_select_one ps, (0,0))

let map_str_write str_lines (x,y) c =
  let before = list_take y str_lines in
  let remain = list_drop y str_lines in
  let line = list_head remain in
  let line' = string_take x line^c^string_drop (x+1) line in
  before @ line' :: list_tail remain
  
(* retrieves a string representing a full simulation step *)
let render_simulation_step world step =
  let str = string_of_mapdata world.data in
  let lines = string_lines str in
  let min_rows = 5 in
  let cols, rows = fst world.size, snd world.size in
  let lines' = 
    if rows < min_rows then 
      (* make a blank line *)
      let line = iterate (fun a -> a^" ") "" cols in
      iterate (fun acc -> acc@line) lines (min_rows - rows)
    else lines in
  let get_line = List.nth lines' in (* curry *)
  let pos_line = 
    let l = get_line 1 in
    l^"         Position: "^string_of_pos @: fst step.state in
  let vel_line =
    let l = get_line 2 in
    l^"         Velocity: "^string_of_pos @: snd step.state in
  let acc_line = maybe (fun () -> List.nth 3 lines') (fun action ->
    let l = get_line 3 in
    l^"         Accel:    "^string_of_pos @: action.accel)
    step.action in
  let score_line = 
    let l = get_line 4 in
    l^"         Score:    "^string_of_int @: step.before_score in
  let lines = pos_line::vel_line::acc_line::score_line::(list_drop 4 lines') in
  let lines' = map_str_write lines (fst step.state) "@" in
  (string_unlines lines')^"\n"

let in_start_finish lpos pos = List.mem pos lpos

