open Util
module P = Printf

let cp = cartesian_product
let files = ["L-track"; "O-track"; "O-track2"; "R-track"]

(* Value iteration *)
let gamma_vals = [0.8; 0.6; 0.4; 0.2]

let (vi_vals : (float * string) list) = cp gamma_vals files

let make_vi_str (gamma, file) = P.sprintf "./run.sh 'map ./data/%s.txt \
  set gamma=%.1f make vi i oo save vi_%s_g%.1f.agent quit'" file gamma file gamma

let alpha_vals = [1.0; 0.8; 0.6; 0.4]
let hard_crash_vals = [false] (* true *)
let annealing = [0; 60; 100]

let q_vals = cp annealing @: cp hard_crash_vals @: cp alpha_vals vi_vals 

let make_q_str (anneal, (hcrash, (alpha, (gamma, file)))) = P.sprintf 
  "./run.sh 'map ./data/%s.txt set gamma=%.1f set alpha=%.1f set hard_crash=%b \
  set annealing=%d make q i 500000 save q_%s_g%.1f_a%.1f_c%b_an%d.agent quit'" 
  file gamma alpha hcrash anneal file gamma alpha hcrash anneal

let dists = [2, 2; 4, 4; 4, 8; 8, 4; 5, 10]

let (q_basis_vals : ((int * int) * (int * (bool * (float * (float * string)))))
list) = cp dists q_vals

let make_q_basis_str 
  ((dist, width), (anneal, (hcrash, (alpha, (gamma, file))))) = P.sprintf 
  "./run.sh 'map ./data/%s.txt set gamma=%.1f set alpha=%.1f set hard_crash=%b \
  set annealing=%d set use_basis=true set basis_max_dist=%d set basis_width=%d \
  make q i 500000 save qb_%s_g%.1f_a%.1f_c%b_an%d_d%d_w%d.agent quit'" 
  file gamma alpha hcrash anneal dist width 
  file gamma alpha hcrash anneal dist width

type agent_t = VI | Q | QBasis

let get_strs = function
  | VI -> list_map make_vi_str vi_vals
  | Q  -> list_map make_q_str q_vals
  | QBasis -> list_map make_q_basis_str q_basis_vals

let get_all_strs () = List.flatten @: list_map get_strs [Q; QBasis]

let main () = 
  let strs = get_all_strs () in
  List.iter (fun s -> print_endline s; ignore(Sys.command s)) strs

let _ = if !Sys.interactive then () else main ()


