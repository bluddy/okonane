open Util
open Search
open Grid
open Arg

let error s = prerr_endline s; exit 1

type search_t = BFS | DFS | IDDFS | BIDIR | ASTAR

let search_method = ref BFS
let target_file = ref ""
let show_graph = ref false
let debug_search = ref false

let param_specs = Arg.align 
    [
        "-bfs", Arg.Unit (fun () -> search_method := BFS), " Use BFS search";
        "-dfs", Arg.Unit (fun () -> search_method := DFS), " Use DFS search";
        "-iddfs", Arg.Unit (fun () -> search_method := IDDFS), " Use incremental DFS";
        "-bidir", Arg.Unit (fun () -> search_method := BIDIR), " Use bi-directional search";
        "-astar", Arg.Unit (fun () -> search_method := ASTAR), " Use A* search";
        "-g", Arg.Set  show_graph, " Show result graph";
        "-d", Arg.Set  debug_search, " Show debug information";
    ]

let usage_msg = "search search_method file"

let parse_cmd_line () = Arg.parse param_specs (fun str -> target_file := str) usage_msg

let do_search file search = 
    let g = grid_of_file file in
    let result = match search with
      | BFS -> bfs g
      | DFS -> dfs g
      | IDDFS -> iddfs g
      | BIDIR -> bidir g
      | ASTAR -> astar g !debug_search
    in let path, cost, expanded = match result with
      | SomePath (path, cost, expanded) -> (path, cost, expanded)
      | NoPath expanded -> ([], -1, expanded)
      | _ -> failwith "Received bad value"
    in let dirs_str = string_of_dirs @: dirs_of_locs path in
    let g_final = set_squares g Path path in
    let g_str = string_of_grid g_final in
    dirs_str, cost, expanded, g_str

let main () =
    parse_cmd_line ();
    if !target_file = "" then (Arg.usage param_specs usage_msg;
        error "\nNo input file specified");
    let dirs_str, cost, expanded, g_str = do_search !target_file !search_method in
    print_endline dirs_str;
    if !show_graph then print_string @: "\n"^g_str^"\n";
    print_endline @: "cost: "^ (match cost with -1 -> "infinite" | _ ->
        string_of_int cost);
    print_endline @: "expanded: "^ string_of_int expanded

let _ = main ()

