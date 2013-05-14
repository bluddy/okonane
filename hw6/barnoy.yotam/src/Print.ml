open Util
module P = Printf
open Str

let files = Sys.readdir "./output/" 

let reg1 = regexp ".*\\(.\\)-track.*"
let reg2 = regexp ".*track\\(.\\)_"
let files_maps =  array_map (fun f ->
    let prefix = 
      if string_match reg1 f 0 then
        matched_group 1 f 
      else failwith "error" in
    let suffix =
      if string_match reg2 f 0 then
        matched_group 1 f
      else "" in
    f, prefix^"-track"^suffix)
  files

let results = List.iter (fun (f,track) ->
  (*let str = P.sprintf *)
  (*"echo %s >> test_data.txt; ./run.sh 'map ./data/%s.txt\ *)
  (*load ./output/%s sim 10 metrics 0 q' \*)
    (*>> test_data.txt" f track f in*)
  let str = P.sprintf 
  "echo %s ; ./run.sh 'map ./data/%s.txt\ 
  load ./output/%s sim 100 metrics 0 q' \
    " f track f in
  ignore(Sys.command str))
  files_maps
