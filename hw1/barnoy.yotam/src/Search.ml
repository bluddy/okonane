open Util
open Grid

exception No_path

let bfs grid = 
    let q = Queue.create () in
    let rec search loc path cost = 
        let goto_next () = 
            let (next, next_path, next_cost) = 
                try Queue.pop q with Queue.Empty -> raise No_path
            in search next next_path next_cost 
        in let path' = loc::path in
        if loc = grid.goal then (path', cost)
        else
            if List.exists ((=) loc) path (* loop detection *)
            then goto_next ()
            else
                let options = expand grid loc in
                List.iter (fun (x, c) -> Queue.push (x, path', cost + c) q) options;
                goto_next ()
    in let (path, cost) = search grid.start [] 0 in
    (List.rev path, cost)
    



