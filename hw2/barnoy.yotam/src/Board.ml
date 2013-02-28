(* Board module *)

open Util

type square_t = Empty | White | Black

type dir_t = DirUp | DirDown | DirRight | DirLeft

type move_t = Remove of int * int
            | Move of (int * int) * (dir_t * int) 

type board_t = { size:int; 
                grid:square_t array array; }

let make_default size = 
  let r = create_range 1 (size*size/4) in
  let half_list a b = 
    let bwlist = List.flatten @: list_map (fun _ -> [a; b]) r in
    list_bunch size bwlist in
  let board_list = 
    list_intersperse (half_list Black White) (half_list White Black) in
  let larray = List.map (fun l -> Array.of_list l) board_list in
  let garray = Array.of_list larray in
  {size=size; grid=garray}

(* ----- String representation ----- *)

let abc = ["a";"b";"c";"d";"e";"f";"g";"h"]

let string_of_square = function
  | Black -> "b"
  | White -> "w"
  | Empty -> " "

let string_of_dir = function
  | DirUp -> "up" | DirDown -> "down" | DirLeft -> "left" | DirRight -> "right"

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
  let num_grid = insert_idx_fst 1 grid_list in
  top_coord_line^"\n"^
    String.concat "\n" @: list_map string_of_line num_grid

(* convert a position in "a1" form to a tuple *)
let pos_of_str str =
  let abc_i = insert_idx_snd 0 abc in
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
   | 0, y when y > 0        -> Some(Move(pos, (DirDown, y)))
   | 0, y when y < 0        -> Some(Move(pos, (DirUp, -y)))
   | x, 0 when x > 0        -> Some(Move(pos, (DirRight, x)))
   | x, 0 when x < 0        -> Some(Move(pos, (DirLeft, -x)))
   | _ -> None

let apply_dir (x,y) = function
  | DirUp, i   -> x, y-i
  | DirDown,i  -> x, y+i
  | DirRight,i -> x+i, y
  | DirLeft,i  -> x-i, y

let src_of_move = function
  | Remove (x,y)   -> x,y
  | Move (pos,dir) -> pos

let dest_of_move = function
  | Remove (x,y)   -> x,y
  | Move (pos,dir) -> apply_dir pos dir

(*let range_of_move = function*)
  (*| Move ((x,y) as p, (d,l)) ->*)
    (*let r = create_range 1 l in*)
    (*p::list_map (fun i -> apply_dir p (d,i)) r*)
  (*| _ -> failwith "not a Move"*)

(*[> check if two ranges overlap <]*)
(*let overlap_range r1 r2 = *)
  (*List.exists (fun p -> List.exists ((=)p) r2) r1*)

let moves_overlap m n = 
  let (x1, y1), (x2, y2), (u1, v1), (u2, v2) = 
    src_of_move m, dest_of_move m, src_of_move n, dest_of_move n in
  let max_min a b = if a >= b then (a,b) else (b,a) in
  let mmax_x, mmin_x = max_min x1 x2 in
  let nmax_x, nmin_x = max_min u1 u2 in
  let mmax_y, mmin_y = max_min y1 y2 in
  let nmax_y, nmin_y = max_min v1 v2 in
  if (mmin_y >= nmin_y && mmax_y <= nmax_y) || 
     (nmin_y  >= mmin_y && nmax_y <= mmax_y) then
      if (mmin_x >= nmin_x && mmax_x <= nmax_x) || 
         (nmin_x >= mmin_x && nmax_x <= mmax_x) then true
      else false
  else false

(* find conflicts between a list of moves *)
let find_conflicts = function
  | [] | [_] -> []
  | moves    ->
    snd @: List.fold_left 
      (fun (remain, acc) y ->
        let l = List.fold_left 
          (fun acc2 x ->
            if moves_overlap x y then (x, y)::acc2
            else acc2)
          acc 
          remain in
          match remain with
           | []    -> ([], l)
           | x::xs -> (xs, l)
      )
      (List.tl moves,[]) 
      moves

(* get sets of moves that don't conflict *)
let ortho_move_sets (moves : move_t list) : move_t list list =
  let conflicts = find_conflicts moves in
  List.fold_left (fun acc (x,y) ->
    List.fold_left (fun acc2 l ->
      if List.exists ((=) x) l && List.exists ((=) y) l then
        (list_remove x l)::(list_remove y l)::acc2
      else l::acc2)
      []
      acc)
  [moves]
  conflicts

(* Does this move take place along the border? It seems that border moves are
 * more important than non-border moves *)
let is_border_move b move =
  let last = b.size - 1 in
  let src = src_of_move move and dst = dest_of_move move in
  let check_border = function
     | (0,_) | (_,0)         -> true
     | (i,_) when i = b.size -> true
     | (_,i) when i = b.size -> true
     | _                     -> false
  in check_border src || check_border dst

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

let get_squares_empty b sq =
  let get_sq ((m,accs,n,acce) as x) = function 
    | (pos, x) when x=sq -> (m+1, pos::accs, n, acce)
    | (pos, Empty)       -> (m, accs, n+1, pos::acce)
    | _                  -> x
  in
  let a, l1, b, l2 = fold get_sq (0,[],0,[]) b in
  a, List.rev l1, b, List.rev l2


let get_num_squares b sq = 
  fold (fun acc (_,s) -> if s = sq then acc+1 else acc) 0 b

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

let rev_dir = function DirUp -> DirDown | DirDown -> DirUp 
              | DirLeft -> DirRight | DirRight -> DirLeft

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
      let dirs = [DirUp,0; DirDown,0; DirLeft,0; DirRight,0] in
      let (n_p, pieces, n_e, empty) = get_squares_empty b color in
      let adjs =  (* if there are fewer empties, use those *)
        if n_p <= n_e then
          List.flatten @:
            list_map (fun x -> list_zip [x;x;x;x] dirs) pieces
        else 
          List.filter (fun (x, _) -> pos_in_board b x && eq_square b color x) @:
            list_map (fun (x, (d,i)) -> apply_dir x (d,2), (rev_dir d, 0)) @:
              List.flatten @:
                list_map (fun x -> list_zip [x;x;x;x] dirs) empty in

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
               let set_pos c = 
                 set b c pos1; 
                 if i = num then set b color pos2; () in
               begin match rewind, lookup b pos1, lookup b pos2 with
                | false, c, Empty when c = otherc -> set_pos Empty 
                | true, Empty, Empty -> set_pos otherc
                | _ -> failwith @: "Bad board configuration:\n"^
                      string_of_board_and_coords b^" rewind: "^
                      string_of_bool rewind^" pos1:"^string_of_pos pos1^
                      " pos2:"^string_of_pos pos2  end 
             in List.iter modify r
        end in 
      (* if we're rewinding, turn the move around *)
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

(* utility function to make sure we rewind a test turn *)
let with_turn b turn move f =
  play b turn move;
  let res = f () in
  rewind b turn move;
  res


