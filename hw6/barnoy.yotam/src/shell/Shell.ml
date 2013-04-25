open Util
open WorldMap

let prompt = "RLASH:> "

module StringMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end)

type shell_t = {
    output_width : int;
    command_map : command_t StringMap.t;
    (* registrations seen by this shell *)
    reg_set : (command_t * string list) list; 
    terminate : bool;
    env: environment_t;
    worldmap : worldmap_t option;
    agent : agent_t option;
    simulation : simulation_step list;
}
    
let new_shell width =
  { output_width = width;
    command_map = StringMap.empty;
    reg_set = [];
    terminate = false;
    env = default_env;
    world = None;
    agent = None;
    simulation = [];
  }
    
(* expects a list of (command, namelist) *)
let register shell commands =
  let m = s.command_map in
  (* flatten the commands to make them easier to add *)
  let cs = List.flatten @: List.rev_map 
     (fun (c, ls) -> 
        List.rev_map (fun x -> c,x) ls
     ) commands in
  let new_map = List.fold_left (fun acc (c,x) -> Map.add x c m) in
  let new_reg = s.reg_set commands in
  {shell with command_map = new_map; reg_set = new_reg}

(* interpret the provided string *)
let rec interpret shell command_str =
  let m = shell.command_map in
  let parts = string_words command_str in
  match parts with
  | []   -> shell
  | c::args -> 
    match map_find c m with
    | None ->  (* shortcut to set *)
              if String.contains command_str '=' &&
                 not String.contains command_str ' ' then
                interpret @: "set "^command_str
              else
                Printf.printf "Unrecognized command: %s\n" x;
                shell
    | Some command -> 
              try
                Command.execute command shell args
              with CommandFailure e -> print_string e; shell

(* execute shell. first execute init_str *)
let execute shell init_str =
  let init_sh = list_fold_until
    (fun sh str -> interpret sh str)
    (fun sh _   -> sh.terminate)
    shell init_str in
  iterate_until
    (fun sh ->
      print_string prompt;
      let s = read_line () in
      if s = "" then sh
      else interpret sh s)
    (fun sh -> sh.terminate)
    init_sh



    

   


                
    








    

