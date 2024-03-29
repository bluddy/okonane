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
  | x   -> failwith @: "Character "^x^" doesn't match terrain types"

type worldmap_t = {
  size : pos_t;
  data : terrain_t array array;
  pos_start : pos_t list;
  pos_finish : pos_t list;
}

let terrain_at m (x,y) =
  let t = m.data in
  if x < 0 || y < 0 || Array.length t <= y then Wall
  else 
    let a = t.(y) in
    if Array.length a <= x then Wall
    else a.(x)
      
let string_of_mapdata m = 
  matrix_fold (fun acc (pos,x) -> 
      let newline = if fst pos = 0 && snd pos <> 0 then "\n" else "" in
      let c = string_of_terrain x in
      acc^newline^c)
    "" m

let mapdata_of_lines lines =
  let ls = list_map (fun line ->
      iterate_until
        (fun (acc,str) -> 
          if str = "" then Left (List.rev acc)
          else
            let c = string_take 1 str in
            let t = terrain_of_string c in
            Right(t::acc, string_drop 1 str))
        ([], line)
  )
  lines in
  matrix_of_lists ls

let mapdata_of_string str = mapdata_of_lines @: string_lines str

(* obtain a full map structure from a string *)
let map_of_string str =
  let lines = string_lines str in
  let reg = Str.regexp "\\([0-9]+\\),\\([0-9]+\\)" in
  if not @: Str.string_match reg (list_head lines) 0
  then failwith "Bad format" else
  let rows_s, cols_s = Str.matched_group 1 str, Str.matched_group 2 str in
  let rows, cols = ios rows_s, ios cols_s in
  let data = mapdata_of_lines @: list_tail lines in
  let start,finish = matrix_fold (fun (s,f) (pos,d) -> 
      match d with 
      | Start  -> pos::s,f
      | Finish -> s, pos::f
      | _      -> s, f)
    ([],[]) data in
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
  
let in_start_finish lpos pos = List.mem pos lpos
let in_finish world pos = in_start_finish world.pos_finish pos
let in_start world pos = in_start_finish world.pos_start pos

(* get a list of all positions in the world *)
let all_positions world =
  matrix_fold (fun acc ((i,j),x) -> match x with 
    | Ground | Rough | Start | Finish -> (i,j)::acc
    | _ -> acc) 
  [] world.data
  



