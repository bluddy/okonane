open Util
open Grid

let bfs grid = 
    let q = Queue.create () in
    let rec search loc path = 
        let goto_next () = 
            let (next, next_path) = Queue.pop q in
            search next next_path in
        let path' = loc::path in
        if loc = grid.goal then path'
        else
            if List.exists ((=) loc) path (* loop detection *)
            then goto_next ()
            else
                let options = expand grid loc in
                List.iter (fun (x, _) -> Queue.push (x, path') q) options;
                goto_next ()
    in List.rev @: search grid.start []



