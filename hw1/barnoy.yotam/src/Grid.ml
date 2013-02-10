(* Grid module *)
open Util

type square_t = 
          | Obstacle
          | Start
          | Goal
          | Open
          | Slow
          | Path

type loc_t = (int * int)  (* x, y *)

type grid_data_t = (square_t list) list

type grid_t = { size : (int * int);
                start : loc_t;
                goal : loc_t;
                data : grid_data_t;
               }

let square_of_char = function
    | '#'       -> Obstacle
    | 's' | 'S' -> Start
    | 'g' | 'G' -> Goal
    | '.'       -> Open
    | ','       -> Slow
    | 'o'       -> Path
    | x         -> invalid_arg @: "don't know how to convert char "^(String.make 1 x)

let string_of_square = function
    | Obstacle -> "#"
    | Start    -> "s"
    | Goal     -> "g"
    | Open     -> "."
    | Slow     -> ","
    | Path     -> "o"

let cost_of_square = function
    | Obstacle -> None
    | Start -> Some 1
    | Goal -> Some 1
    | Open -> Some 1
    | Slow -> Some 2
    | Path -> invalid_arg "Can't compute cost of path square"

let gridline_of_string ?(expected_len=None) str = 
    let len = String.length str in
    let convert l = 
        let rng = create_range 0 len in
        List.map (fun i -> square_of_char @: str.[i]) rng
    in
    match expected_len with
      | None                 -> convert len
      | Some l when l <= len -> convert l
      | _                    -> invalid_arg "Bad file: wrong string length"

let string_of_gridline gridl =
    let s = List.fold_left (fun acc square -> acc^string_of_square square) "" gridl
    in s^"\n"

let string_of_grid g =
    List.fold_left (fun acc line -> acc^string_of_gridline line) "" g.data

let find_square g sq =
    let (width, height) = g.size in
    let w_r = create_range 0 width in
    let h_r = create_range 0 height in
    let map_line j line = List.map2 (fun i x -> (x, (i, j))) w_r line in
    let num_data = List.flatten @: List.map2 map_line h_r g.data in
    List.assoc sq num_data

let change_nth li nth a = 
    let newl = List.fold_left 
        (fun (acc, count) elem -> if count = nth then (a::acc, count+1)
                                  else elem::acc, count+1)
        ([], 0) li
    in List.rev @: fst @: newl
    
let set_square g sq (x,y) =
    let listy = List.nth g.data y in
    let listy' = change_nth listy x sq in
    let data = change_nth g.data y listy' in
    {g with data=data}

let set_squares g sq locs = 
    List.fold_left (fun acc_g loc -> set_square acc_g sq loc) g locs

let grid_of_file file : grid_t =
    let lines = read_file_lines file in
    match lines with
     | line1 :: lines' -> let wh = Str.split (Str.regexp " ") line1 in
        begin match wh with
         | w::h::[] -> let width = int_of_string w in
                       let height = int_of_string h in
                       let lines'' = list_take height lines' in
             if height = List.length lines'' then
                 let data = List.map (gridline_of_string ~expected_len:(Some width)) lines''
                 in let grid = 
                     {size = (width, height); data = data; 
                       start = (0,0); goal = (0,0)} in
                 let start = find_square grid Start in
                 let goal = find_square grid Goal in
                 { grid with start = start; goal = goal }
             else invalid_arg "Bad file format: missing lines"
         | _ -> invalid_arg "Bad file format: missing width/height"
        end
     | _ -> invalid_arg "Bad file format"
    
exception LookupError of string

let lookup grid (x, y) = 
    let g = grid.data in
    try
        let gridline = List.nth g y in
        List.nth gridline x
    with Not_found -> raise @: LookupError ("positiong "^string_of_int x^"
        "^string_of_int y^" is not in the grid")

type dir_t = Up | Down | Left | Right

let clamp_move grid x y dir = 
    let (w, h) = grid.size in
    match x, y, dir with
     | 0, _, Left                 -> None
     | _, 0, Up                   -> None
     | x, _, Right when x = w - 1 -> None
     | _, y, Down when y = h - 1  -> None
     | x, y, Up                   -> Some (x, y-1)
     | x, y, Down                 -> Some (x, y+1)
     | x, y, Left                 -> Some (x-1, y)
     | x, y, Right                -> Some (x+1, y)

let expand grid (x,y) = 
    match lookup grid (x, y) with
    | Obstacle -> raise @: LookupError "Cannot expand on an obstacle"
    | _        -> let dirs = [Up; Down; Left; Right] in
      let moves = List.map (clamp_move grid x y) dirs in
      let clamped_moves = flatten_option moves in
      let squares = List.map (lookup grid) clamped_moves in
      let costs = List.map cost_of_square squares in
      List.flatten @: List.map2 
          (fun m c -> match c with 
                        Some x -> [(m, x)] 
                      | None -> [])
          clamped_moves costs
    
