open Util
open Grid

exception No_path

let search f_create f_push f_pop f_isempty grid = 
    let rec loop loc path cost queue = 
        let goto_next q = match f_pop q with
            | None -> None
            | Some ((next, n_path, n_cost), q') -> loop next n_path n_cost q'
        in let path' = loc::path in
        if loc = grid.goal then Some (List.rev path', cost)
        else
            (* loop detection *)
            let q', skip = BatDeque.fold_right 
                (fun l (accq, skip) -> match l with
                    | (loc', _, cost') as x when loc' = loc && cost' <= cost ->
                            BatDeque.cons x accq, true
                    | loc', _, cost' when loc' = loc && cost' < cost  -> accq, false
                    | x -> (BatDeque.cons x accq, skip)
                )
                queue (BatDeque.empty, false) in 
            if skip then goto_next q'
            else
                let options = expand grid loc in
                let q'' = List.fold_left 
                    (fun accq (x, c) -> f_push (x, path', cost + c) accq) q' options
                in goto_next q''
    in
    let q_init = f_create in
    loop grid.start [] 0 q_init

let bfs = search BatDeque.empty (flip BatDeque.snoc) BatDeque.front BatDeque.is_empty
let dfs = search BatDeque.empty BatDeque.cons BatDeque.front BatDeque.is_empty

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
