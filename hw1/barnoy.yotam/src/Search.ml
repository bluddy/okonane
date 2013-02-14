open Util
open Grid

(* This is the type of node we store in our queue *)
type qnode_t = Delete of loc_t | Node of loc_t * int * loc_t list

(* This is our return value from the search function *)
type 'a return_t = SomePath of 'a | NoPath | MaxDepth

(* translate these functions to be exceptionless *)
let hfind hash x = try Some(Hashtbl.find hash x) with Not_found -> None
let qpop q = try Some(Queue.pop q) with Queue.Empty -> None
let spop s = try Some(Stack.pop s) with Stack.Empty -> None

let search is_stack f_create f_push f_pop max_depth grid = 
    let hash = Hashtbl.create 100 in (* hashtable for efficiency *)
    let q = f_create () in
    let rec loop loc cost path past_depth = 
        let rec goto_next past = match f_pop q with
            | None when past -> MaxDepth
            | None -> NoPath
            | Some (Node(next, cost, path)) -> loop next cost path past
            | Some (Delete x) -> Hashtbl.remove hash x; goto_next past
        in
        let new_path = loc::path in
        match (hfind hash loc, max_depth) with
         | None, _ -> goto_next past_depth (* already dealt with this node *)
         | Some cost', _ when cost' <> cost -> goto_next past_depth
         | Some _, Some depth when List.length new_path > depth -> goto_next true
         | Some _, _ when loc = grid.goal -> SomePath (List.rev new_path, cost)
         | Some _, _ ->
            let options = expand grid loc in
            let update_q_hash (x, c) = 
                let new_cost = c + cost in
                match hfind hash x with 
                 | Some old_cost when old_cost <= new_cost -> ()
                 | Some _ | None -> 
                         Hashtbl.replace hash x new_cost; 
                         f_push (Node (x, new_cost, new_path)) q
            in
            (* only delete us once our children are done *)
            let if_is_stack f g = if is_stack then g @: f () else f @: g () in
            if_is_stack (fun () -> f_push (Delete loc) q)
                        (fun () -> List.iter update_q_hash options)
            ; goto_next past_depth
    in
    Hashtbl.add hash grid.start 0;
    loop grid.start 0 [] false

let bfs = search false Queue.create Queue.push qpop None
let dfs = search true Stack.create Stack.push spop None
let iddfs grid = 
    let search' = search true Stack.create Stack.push spop in
    let rec loop i = 
        match search' (Some i) grid with
         | MaxDepth -> loop @: i+1
         | x -> x
    in loop 1

let bidir grid = 
    let hash1 = Hashtbl.create 100 in let hash2 = Hashtbl.create 100 in 
    let q1 = Queue.create () in let q2 = Queue.create () in
    let choose a b = function 1 -> a | 2 -> b in
    let getq = choose q1 q2 in
    let geth = choose hash1 hash2 in
    let getgoal = choose grid.goal grid.start in
    let alt = choose 2 1 in
    
    let rec loop i loc cost path = 
        (* the current hash & q *)
        let hash = geth i in let q = getq i in let goal = getgoal i in
        (* the other hash & q *)
        let otheri = alt i in let otherq = getq otheri in otherh = geth otheri in
        (* switch off between queues and hash tables when looping *)
        let rec goto_next () = 
            match qpop otherq with
            | None -> NoPath
            | Some (Node(next, cost, path)) -> loop otheri next cost path past
            | Some (Delete x) -> Hashtbl.remove otherh x; goto_next ()
        in
        let new_path = loc::path in
        let concat_path localp localc otherp otherc = match i with 
            | 1 -> SomePath((List.rev localp) @ otherp, localc + otherc)
            | 2 -> SomePath((List.rev otherp) @ localp, localc + otherc)
        in
        match hfind hash loc with
         | None -> goto_next ()    (* already dealt with this node *)
         | Some cost' when cost' <> cost -> goto_next ()
         | Some _ when loc = goal        -> SomePath (List.rev new_path, cost)
         | Some _ -> 
             (* check for us in the other hash table *)
             match hfind otherh loc with
             | Some other_cost -> 
                 let getmatches acc = function
                   | (Node (l, c, p)) as x when l = loc && c = other_cost -> x::acc
                   | _ -> acc in
                 let matches = Queue.fold getmatches [] otherq in
                 match matches with 
                  | (Node (_, _, otherp))::_ -> 
                          concat_path path cost otherp other_cost 
                  | _  -> failwith "couldn't find member in queue!"
                      
             | None ->
                let options = expand grid loc in
                let update_q_hash (x, c) = 
                    let new_cost = c + cost in
                    match hfind hash x with 
                     | Some old_cost when old_cost <= new_cost -> ()
                     | Some _ | None -> 
                             Hashtbl.replace hash x new_cost; 
                             Queue.push (Node (x, new_cost, new_path)) q
                in
                (* only delete us once our children are done *)
                List.iter update_q_hash options;
                f_push (Delete loc) q;
                goto_next ()
    in
    Hashtbl.add hash1 grid.start 0;
    Hashtbl.add hash2 grid.goal 0;
    Queue.push (Node (grid.goal, 0, [])) q2;
    loop grid.start 0 []

