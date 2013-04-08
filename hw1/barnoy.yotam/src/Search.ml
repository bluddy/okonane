open Util
open Grid

module Q = Queue
module S = Stack
module Hash = Hashtbl

(* This is the type of node we store in our queue *)
type 'a qnode_t = Delete of loc_t | Node of loc_t * 'a * loc_t list

(* This is our return value from the search function *)
type 'a return_t = SomePath of 'a | NoPath of int | MaxDepth

(* translate these functions to be exceptionless *)
let hfind hash x = try Some(Hash.find hash x) with Not_found -> None
let qpop q = try Some(Q.pop q) with Q.Empty -> None
let spop s = try Some(S.pop s) with S.Empty -> None

let count = ref 0 

let search is_stack f_create f_push f_pop max_depth grid = 
    let hash = Hash.create 100 in (* hashtable for efficiency *)
    let expanded = ref 0 in
    let q = f_create () in
    let rec loop loc cost path past_depth = 
        print_endline @: string_of_loc loc;
        if loc = (97,9997) then exit 1;
        let rec goto_next past = match f_pop q with
            | None when past -> MaxDepth
            | None -> NoPath !expanded
            | Some (Node(next, cost, path)) -> loop next cost path past
            | Some (Delete x) -> print_endline @: "delete "^string_of_loc x; Hash.remove hash x;
            count := !count + 1; if !count >= 110 then exit 1 else goto_next past
        in
        let new_path = loc::path in
        match (hfind hash loc, max_depth) with
         | None, _ -> goto_next past_depth (* already dealt with this node *)
         (*| Some cost', _ when cost' <> cost -> goto_next past_depth*)
         | Some _, Some depth when List.length new_path > depth -> goto_next true
         | Some _, _ when loc = grid.goal -> SomePath (List.rev new_path, cost, !expanded)
         | Some _, _ ->
            let options = expanded := !expanded + 1; 
                    print_endline @: "expand "^string_of_loc loc;
                    expand grid loc in
            let update_q_hash (x, c) = 
                let new_cost = c + cost in
                match hfind hash x with 
                 | Some old_cost when old_cost <= new_cost -> print_endline @:
                     "old"^string_of_loc loc; 
                 | Some _ | None -> 
                         print_endline @: "adding "^string_of_loc x;
                         Hash.replace hash x new_cost; 
                         f_push (Node (x, new_cost, new_path)) q
            in
            (* only delete us once our children are done *)
            let if_is_stack f g = if is_stack then g @: f () else f @: g () in
            if_is_stack (fun () -> f_push (Delete loc) q)
                        (fun () -> List.iter update_q_hash options)
            ; goto_next past_depth
    in
    Hash.add hash grid.start 0;
    loop grid.start 0 [] false

let bfs = search false Q.create Q.push qpop None
let dfs = search true S.create S.push spop None
let iddfs grid = 
    let search' = search true S.create S.push spop in
    let rec loop i = 
        match search' (Some i) grid with
         | MaxDepth -> loop @: i+1
         | x -> x
    in loop 1

let bidir grid = 
    let hash1 = Hash.create 100 in let hash2 = Hash.create 100 in 
    let expanded = ref 0 in
    let q1 = Q.create () in let q2 = Q.create () in
    let choose a b = function 1 -> a | 2 -> b | _ -> failwith "bad value in choose" 
    in let getq = choose q1 q2 in
    let geth = choose hash1 hash2 in
    let getgoal = choose grid.goal grid.start in
    let alt = choose 2 1 in
    
    let rec loop i loc cost path = 
        (* the current hash & q *)
        let hash = geth i in let q = getq i in let goal = getgoal i in
        (* the other hash & q *)
        let otheri = alt i in let otherq = getq otheri in let otherh = geth otheri in
        (* switch off between queues and hash tables when looping *)
        let rec goto_next () = 
            match qpop otherq with
            | None -> NoPath !expanded
            | Some (Node(next, cost, path)) -> loop otheri next cost path
            | Some (Delete x) -> Hash.remove otherh x; goto_next ()
        in
        let new_path = loc::path in
        let concat_path localp otherp = choose 
            ((List.rev localp) @ otherp) ((List.rev otherp) @ localp) i
        in
        match hfind hash loc with
         | None -> goto_next ()    (* already dealt with this node *)
         | Some cost' when cost' <> cost -> goto_next ()
         | Some _ when loc = goal        -> SomePath (List.rev new_path, cost, !expanded)
         | Some _ -> 
             (* check for us in the other hash table *)
             match hfind otherh loc with
             | Some other_cost -> 
                 let getmatches acc = begin function
                   | (Node (l, c, _)) as x when l = loc && c = other_cost -> x::acc
                   | _ -> acc 
                 end in
                 let matches = Q.fold getmatches [] otherq in
                 begin match matches with 
                  | (Node (_, _, otherp))::_ -> 
                          SomePath(concat_path new_path otherp, other_cost + cost, !expanded)
                  | _  -> failwith "couldn't find member in queue!"
                 end
                      
             | None ->
                let options = expanded := !expanded + 1; expand grid loc in
                let update_q_hash (x, c) = 
                    let new_cost = c + cost in
                    begin match hfind hash x with 
                     | Some old_cost when old_cost <= new_cost -> ()
                     | Some _ | None -> 
                             Hash.replace hash x new_cost; 
                             Q.push (Node (x, new_cost, new_path)) q
                    end
                in
                (* only delete us once our children are done *)
                List.iter update_q_hash options;
                Q.push (Delete loc) q;
                goto_next ()
    in
    Hash.add hash1 grid.start 0;
    Hash.add hash2 grid.goal 0;
    Q.push (Node (grid.goal, 0, [])) q2;
    loop 1 grid.start 0 []

(* create an ordering to be able to use the Heap *)
module PriorityOrder (*: BatInterfaces.OrderedType *)= 
struct
    type t = (float * int) qnode_t (* h, cost *)
    let compare n1 n2 = match (n1, n2) with
      | Node (_, (h1, _), _), Node (_, (h2, _), _) when h1 > h2 -> 1
      | Node (_, (h1, _), _), Node (_, (h2, _), _) when h1 = h2 -> 0
      | Node (_, (h1, _), _), Node (_, (h2, _), _) when h1 < h2 -> -1
      | _ -> failwith "bad values in priority queue!"
end

(* make the heap use our ordering *)
module PQ = BatHeap.Make(PriorityOrder)

(* make the priority queue find_min exceptionless *)
let pq_min q = try Some(PQ.find_min q) with Invalid_argument _ -> None

let l2norm (x,y) (dest_x, dest_y) = 
    let dx = dest_x - x in let dy = dest_y - y in
    sqrt @: float_of_int @: dx * dx + dy * dy

let l1norm (x, y) (dest_x, dest_y) =
    let dx = dest_x - x in let dy = dest_y - y in
    float_of_int @: abs @: dx + dy

let linorm (x, y) (dest_x, dest_y) = 
    let dx = dest_x - x in let dy = dest_y - y in
    float_of_int @: max dx dy

let h_function f loc cost grid = (float_of_int cost) +. f loc grid.goal
let h_l1norm = h_function l1norm
let h_l2norm = h_function l2norm
let h_linorm = h_function linorm

let astar_general h_func grid debug = 
    let hash = Hashtbl.create 100 in (* to avoid adding duplicates *)
    let expanded = ref 0 in
    let rec loop loc cost path queue = 
        if debug then print_endline @: string_of_loc loc; (* debug *)
        let goto_next q = 
            begin match pq_min q with
            | Some(Node(next, (hval, next_cost), next_path)) -> 
                if debug then print_endline @: string_of_float hval;
                let q' = PQ.del_min q in 
                Hash.remove hash next;
                loop next next_cost next_path q'
            | None -> NoPath !expanded (* empty q *)
            | _ -> failwith "invalid value in hstar goto_next!"
            end
        in
        let new_path = loc::path in
        if loc = grid.goal then SomePath (List.rev new_path, cost, !expanded)
        else if List.exists ((=) loc) path then goto_next queue
        else 
            let options = expanded := !expanded + 1; expand grid loc in
            let update_q accq (x, c) = 
                let new_cost = c + cost in
                let new_hval = h_func x new_cost grid in
                match hfind hash x with 
                 | Some old_hval when old_hval <= new_hval -> accq
                 | Some _ | None -> 
                     Hash.replace hash x new_hval; 
                     PQ.insert accq @: Node (x, (new_hval, new_cost), new_path)
            in
            let q' = List.fold_left update_q queue options in
            goto_next q'
    in
    let q = PQ.empty in
    loop grid.start 0 [] q

let astar = astar_general h_l1norm

