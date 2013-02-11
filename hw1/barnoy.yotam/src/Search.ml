open Util
open Grid

exception No_path

let multisearch f_create f_push f_pop f_isempty grid = 
    let q = f_create () in
    let rec search loc path cost = 
        let goto_next () = 
            if f_isempty q then None
            else 
                let (next, next_path, next_cost) = f_pop q in
                search next next_path next_cost 
        in let path' = loc::path in
        if loc = grid.goal then Some (List.rev path', cost)
        else
            if List.exists ((=) loc) path then goto_next () (* loop detection *)
            else
                let options = expand grid loc in
                List.iter (fun (x, c) -> f_push (x, path', cost + c) q) options;
                goto_next ()
    in search grid.start [] 0

let bfs = multisearch Queue.create Queue.push Queue.pop Queue.is_empty
let dfs = multisearch Stack.create Stack.push Stack.pop Stack.is_empty

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
