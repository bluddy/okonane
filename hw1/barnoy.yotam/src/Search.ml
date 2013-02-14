open Util
open Grid

exception No_path

type qnode_t = Delete of loc_t | Node of loc_t * int * loc_t list

(* translate hash find to be exceptionless *)
let hfind hash x = try Some(Hashtbl.find hash x) with Not_found -> None
let qpop q = try Some(Queue.pop q) with Queue.Empty -> None
let spop s = try Some(Stack.pop s) with Stack.Empty -> None

let search is_stack f_create f_push f_pop grid = 
    let hash = Hashtbl.create 100 in (* hashtable for efficiency *)
    let q = f_create () in
    let rec loop loc cost path = 
        let rec goto_next () = match f_pop q with
            | None -> None
            | Some (Node(next, cost, path)) -> loop next cost path
            | Some (Delete x) -> Hashtbl.remove hash x; goto_next ()
        in
        match hfind hash loc with
         | None -> goto_next () (* already dealt with this node *)
         | Some cost' when cost' <> cost -> goto_next ()
         | Some _ ->
            let new_path = loc::path in
            if loc = grid.goal then Some (List.rev new_path, cost)
            else
                let options = expand grid loc in
                let update_q_hash (x, c) = 
                    let new_cost = c + cost in
                    match hfind hash x with 
                     | None -> Hashtbl.add hash x new_cost; 
                               f_push (Node (x, new_cost, new_path)) q
                     | Some old_cost when old_cost <= new_cost -> ()
                     | Some old_cost -> 
                             Hashtbl.replace hash x new_cost; 
                             f_push (Node (x, new_cost, new_path)) q
                in
                (* only delete us once our children are done *)
                let if_is_stack f g = if is_stack then g @: f () else f @: g () in
                if_is_stack (fun () -> f_push (Delete loc) q)
                            (fun () -> List.iter update_q_hash options)
                ; goto_next ()
    in
    Hashtbl.add hash grid.start 0;
    loop grid.start 0 []

let bfs = search false Queue.create Queue.push qpop
let dfs = search true Stack.create Stack.push spop

(* Iterative deepening depth first search *)
(*let iddfs = *)
    (*let rec iterate_until_some fn d = *)
        (*match fn d with*)
         (*| None -> iterate_until_some fn (d+1)*)
         (*| Some x -> x*)
    (*in let iddfs_inner grid max_depth = *)
        (*let q = Stack.create () in*)
        (*let rec search loc path cost = *)
            (*let goto_next () = *)
                (*if Stack.is_empty q then None*)
                (*else *)
                    (*let (next, next_path, next_cost) = Stack.pop q in*)
                    (*search next next_path next_cost *)
            (*in let path' = loc::path in*)
            (*if loc = grid.goal then Some (path', cost)*)
            (*else*)
                (*if List.exists ((=) loc) path then goto_next () [> loop detection <]*)
                (*else if List.length path = max_depth then goto_next ()*)
                (*else*)
                    (*let options = expand grid loc in*)
                    (*List.iter (fun (x, c) -> Stack.push (x, path', cost + c) q) options;*)
                    (*goto_next ()*)
        (*in match search grid.start [] 0 with*)
          (*| None -> None*)
          (*| Some (path, cost) -> (List.rev path, cost)*)
    (*in iterate_until_some (iddfs_inner grid) 1*)

(*[> Bi-directional search <]*)
(*let bidir_bfs grid = *)
    (*let q = Queue.create () in*)
    (*let rec search loc path cost = *)
        (*let goto_next () = *)
            (*if Queue.is_empty q then None*)
            (*else *)
                (*let (next, next_path, next_cost) = Queue.pop q in*)
                (*search next next_path next_cost *)
        (*in let path' = loc::path in*)
        (*if loc = grid.goal then Some (path', cost)*)
        (*else*)
            (*if List.exists ((=) loc) path [> loop detection <]*)
            (*then goto_next ()*)
            (*else*)
                (*let options = expand grid loc in*)
                (*List.iter (fun (x, c) -> Queue.push (x, path', cost + c) q) options;*)
                (*goto_next ()*)
    (*in match search grid.start [] 0 with*)
      (*| None -> None*)
      (*| Some (path, cost) -> (List.rev path, cost)*)
