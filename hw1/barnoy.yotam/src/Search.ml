open Util
open Grid

exception No_path

(* translate hash find to be exceptionless *)
let hfind hash x = try Some(Hashtbl.find hash x) with Not_found -> None

let search f_push f_pop grid = 
    let hash = Hashtbl.create 100 in (* hashtable for efficiency *)
    let rec loop loc queue = 
        let goto_next q : (loc_t list * int) option= match f_pop q with
            | None -> None
            | Some (next, q') -> loop next q'
        in
        match hfind hash loc with
         | None -> goto_next queue (* already dealt with this node *)
         | Some (cost, path_ref) -> 
            let path = !path_ref in
            Hashtbl.remove hash loc;
            let new_path = loc::path in
            if loc = grid.goal then Some (List.rev new_path, cost)
            else
                let options = expand grid loc in
                let update_q_hash q (x, c) = 
                    let new_cost = c + cost in
                    match hfind hash x with 
                     | None -> Hashtbl.add hash x (new_cost, ref new_path); f_push x q
                     | Some(old_cost, _) when old_cost <= new_cost -> q
                     | Some(old_cost, _) -> 
                             Hashtbl.replace hash x (new_cost, ref new_path); 
                             f_push x q
                in
                let queue' = List.fold_left update_q_hash queue options
                in goto_next queue'
    in
    let q_init = BatDeque.empty in
    Hashtbl.add hash grid.start (0, ref []);
    loop grid.start q_init

let bfs = search (flip BatDeque.snoc) BatDeque.front
let dfs = search BatDeque.cons BatDeque.front 

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
