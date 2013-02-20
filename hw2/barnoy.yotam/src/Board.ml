(* Board module *)

open Util

type square_t = Empty | White | Black

type dir_t = Up | Down | Right | Left

type move_t = Remove of int * int
            | Move of (int * int) * (dir_t * int) 

type board_t = { size:int; 
                grid:square_t array array; }

let make_default size = 
  let check_size = function 4 | 6 | 8 -> true | _ -> false in
  if not @: check_size size then None
  else 
    let r = create_range 1 (size*size/4) in
    let half_list a b = 
      let bwlist = List.flatten @: list_map (fun _ -> [a; b]) r in
      list_bunch size bwlist in
    let board_list = 
      list_intersperse (half_list Black White) (half_list White Black) in
    let larray = List.map (fun l -> Array.of_list l) board_list in
    let garray = Array.of_list larray in
    Some {size=size; grid=garray}

(* ----- String representation ----- *)

let abc = ["a";"b";"c";"d";"e";"f";"g";"h"]

let string_of_square = function
  | Black -> "b"
  | White -> "w"
  | Empty -> " "

let string_of_dir = function
  | Up -> "up" | Down -> "down" | Left -> "left" | Right -> "right"

let string_of_pos (x,y) = "("^string_of_int x^", "^string_of_int y^")"

let abc_of_pos (x,y) = List.nth abc x^string_of_int (y + 1)

let string_of_move = function
  | Remove (a,b) -> "removes "^abc_of_pos (a,b)
  | Move ((x,y),(dir,i)) -> "moves "^
    abc_of_pos (x,y)^" "^string_of_dir dir^" "^string_of_int i

let string_of_board b = 
  let string_of_line l = 
    "|"^(String.concat "|" @: array_map string_of_square l)^"|" in
  String.concat "\n" @: array_map string_of_line b.grid

(* string of the board, overlaid with specific positions and their strings *)
(* the list is of the form: [(pos, string)] *) 
let string_of_board_and_coords b =
  let top_coord_line = "   "^String.concat " " @: list_take b.size abc in
  let string_of_line (i,l) = 
    string_of_int i^
      " |"^(String.concat "|" @: array_map string_of_square l)^"|"
  in
  (* convert grid to list-of-arrays form *)
  let grid_list = array_map id_fn b.grid in 
  let num_grid = insert_index_fst 1 grid_list in
  top_coord_line^"\n"^
    String.concat "\n" @: list_map string_of_line num_grid

(* convert a position in "a1" form to a tuple *)
let pos_of_str str =
  let abc_i = insert_index_snd 0 abc in
  let len = String.length str in
  if len <> 2 then None
  else 
    let s1 = String.sub str 0 1 and s2 = String.sub str 1 1 in
    let interpret a b = 
      let a_i = List.assoc a abc_i in
      let b_i = (int_of_string b) - 1 in
      Some (a_i, b_i)
    in try 
      interpret s1 s2
    with Not_found | Failure _ ->
      begin 
        try interpret s2 s1
        with Not_found | Failure _ -> None
      end

let move_of_2_pos ((x1,y1) as pos) (x2,y2) =
  match x2-x1, y2-y1 with
   | 0, y when y mod 2 <> 0 -> None (* must be even *)
   | 0, y when y > 0        -> Some(Move(pos, (Down, y)))
   | 0, y when y < 0        -> Some(Move(pos, (Up, -y)))
   | x, 0 when x > 0        -> Some(Move(pos, (Right, x)))
   | x, 0 when x < 0        -> Some(Move(pos, (Left, -x)))
   | _ -> None

let apply_dir (x,y) = function
  | Up, i   -> x, y-i
  | Down,i  -> x, y+i
  | Right,i -> x+i, y
  | Left,i  -> x-i, y

let dest_of_move = function
  | Remove (x,y)   -> x,y
  | Move (pos,dir) -> apply_dir pos dir

(* generic fold over the grid. Takes a function given acc ((i,j),x) *)
let fold f acc b =
  snd @: 
    Array.fold_left
      (fun (j, accj) l -> 
        let res = Array.fold_left
          (fun (i, acci) x -> i+1, f acci ((i,j),x))
          (0, accj) l
        in j+1, snd res)
      (0, acc) b.grid

(* get a list of squares of a specific type *)
let get_squares b sq = 
  let get_sq acc = function 
    | (pos, x) when x=sq -> pos::acc
    | _                  -> acc
  in
  List.rev @: fold get_sq [] b

let pos_in_board b (x,y) = match b.size with
  | s when x >= 0 && y >= 0 && x < s && y < s -> true
  | _ -> false

(* limit positions to those on the board *)
let clamp_to_board b l = List.filter (pos_in_board b) l

let lookup b (x,y) = 
  try (b.grid.(y)).(x) with Invalid_argument a -> 
   invalid_arg @: a^"("^string_of_int x^","^string_of_int y^")"

let set b sq (x,y) = (b.grid.(y)).(x) <- sq

let eq_square b sq pos = let value = lookup b pos in
  if value = sq then true else false

(* filter only the positions that match the square *)
let filter_sq b sq l = List.filter (eq_square b sq) l

let rev_dir = function Up -> Down | Down -> Up | Left -> Right | Right -> Left

let color_of_turn t = match t mod 2 with 0 -> White | _ -> Black

let other_color = function Black -> White | White -> Black | _ -> invalid_arg ""

let expand b turn = 
  let color = color_of_turn turn in
  let otherc = other_color color in
  match turn with
  | 1 -> 
    let s = b.size in let halfs = s/2 in
    let crossboard = [0; halfs - 1; halfs; s - 1] in
    List.map (fun x -> Remove (x, x)) crossboard
  | 2 ->
    let fourdirs (x,y) = [x-1,y; x+1,y; x,y-1; x,y+1] in
    (* 1.get gaps 2.move in all directions 3.clamp 4.compare color *)
    let gaps = get_squares b Empty in
    let moves = clamp_to_board b @: List.flatten @: list_map fourdirs gaps in
    let whites = filter_sq b White moves in
    List.map (fun (a,b) -> Remove (a,b)) whites

  | t ->
      let dirs = [Up,0; Down,0; Left,0; Right,0] in
      let pieces = get_squares b color in
      let adjs = List.flatten @:
        list_map (fun x -> list_zip [x;x;x;x] dirs) pieces in

      (* we loop, extending our jumps until we can't extend any *)
      let rec loop acc l = 
        (* check if we can extend our matches with longer jumps *)
        let extend (x, (dir,i)) = x, (dir, i+2) in
        let extended = List.map extend l in
        let clamped = List.filter 
          (fun (x,d) -> pos_in_board b @: apply_dir x d) extended in
        (* check the conditions for jumping *)
        let matches = List.filter 
          (fun (x,(dir,i)) -> 
            let x1 = apply_dir x (dir, i-1) in
            let x2 = apply_dir x (dir, i) in
            eq_square b otherc x1 && eq_square b Empty x2) clamped in
        begin match matches with
         | [] -> acc
         | _  -> loop (matches@acc) matches (* another round of tests *)
        end in

        let matches = loop [] adjs in
        List.map (fun ((x,y),(d,i)) -> Move ((x,y),(d,i))) matches

let valid_move b turn move = 
  let moves = expand b turn in
  List.exists ((=) move) moves

(* unified logic for rewinding/playing a move on the board *)
let play_rewind rewind b turn move = 
  let toplay = color_of_turn turn in
  match move with
   | Remove (x,y) | Move ((x,y), _) when not (pos_in_board b (x,y)) -> 
       invalid_arg "Position out of bounds"
   | Remove (x,y) -> let pos = x,y in
     begin if rewind then
       match lookup b pos with
       | Empty -> set b toplay pos; ()
       | _ -> invalid_arg "Cannot rewind remove on non-Empty"
     else (* play *)
       match lookup b pos with
       | Empty -> invalid_arg "Cannot play move on Empty"
       | x when x <> toplay -> invalid_arg "Wrong color being removed"
       | _ -> set b Empty pos; ()
     end
   | Move (_, (_,i)) when i < 2 || i mod 2 != 0 -> 
       invalid_arg "Move amount must be even integer >= 2"
   | Move (p, (d, num)) ->
      let do_move pos dir =
        begin match lookup b pos with
         | Empty -> invalid_arg @:
             "Cannot"^(if rewind then "rewind" else "play")^ " move on empty"
         | color when color <> toplay -> invalid_arg "Color mismatch"
         | color ->
             let r = create_range 2 ~step:2 (num/2) in
             let otherc = other_color color in
             set b Empty pos;
             let modify i = 
               let pos1 = apply_dir pos (dir, i-1) in
               let pos2 = apply_dir pos (dir, i) in
               let set_pos () = 
                 set b Empty pos1; 
                 if i = num then set b color pos2; () in
               begin match rewind, lookup b pos1, lookup b pos2 with
                | false, c, Empty when c = otherc -> set_pos ()
                | true, Empty, Empty -> set_pos ()
                | _ -> failwith "Bad board configuration" end 
             in List.iter modify r
        end in 
      if rewind then 
        let pos = apply_dir p (d, num)  in
        let dir = rev_dir d in
        do_move pos dir
      else (* play *)
        do_move p d


(* play a move on a board, modifying the board *)
let play = play_rewind false
(* rewind a move on a board, restoring the board to the way it was *)
let rewind = play_rewind true
