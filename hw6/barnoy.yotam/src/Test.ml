open Util
module P = Printf

let cp = cartesian_product
let files = ["L-track"; "O-track"; "O-track2"; "R-track"]

(* Value iteration *)
let gamma_vals = [0.8; 0.6; 0.4]
let hard_crash_vals = [true] (* true *)

let vi_vals = cp gamma_vals files
let vi_vals2 = cp hard_crash_vals @: cp gamma_vals files

let make_vi_str (hard, (gamma, file)) = 
  P.sprintf "./run.sh 'map ./data/%s.txt \
  set gamma=%.1f set hard_crash=%b make vi i oo save vi_%s_g%.1f_c%b.agent quit'"
  file gamma hard file gamma hard

let alpha_vals = [0.8; 0.6; 0.4]
let hard_crash_vals = [false] (* true *)
let annealing = [0; 60; 100]

let q_vals = cp annealing @: cp hard_crash_vals @: cp alpha_vals vi_vals 

let make_q_str (anneal, (hcrash, (alpha, (gamma, file)))) = P.sprintf 
  "./run.sh 'map ./data/%s.txt set gamma=%.1f set alpha=%.1f set hard_crash=%b \
  set annealing=%d make q i 500000 save q_%s_g%.1f_a%.1f_c%b_an%d.agent quit'" 
  file gamma alpha hcrash anneal file gamma alpha hcrash anneal

let dists = [2, 2; 4, 4; 4, 8; 8, 4; 5, 10]

let alpha_vals_b = [0.01; 0.005]

let q_basis_vals = 
  cp dists @: cp annealing @: cp hard_crash_vals @: cp alpha_vals_b vi_vals

let q_basis_vals = 
  cp dists @: cp [60] @: cp [false] @: cp alpha_vals_b @: cp [0.6] files

let make_q_basis_str 
  ((dist, width), (anneal, (hcrash, (alpha, (gamma, file))))) = P.sprintf 
  "./run.sh 'map ./data/%s.txt set gamma=%.1f set alpha=%.2f set hard_crash=%b \
  set annealing=%d set use_basis=true set basis_max_dist=%d set basis_width=%d \
  make q i 500000 save qb_%s_g%.1f_a%.2f_c%b_an%d_d%d_w%d.agent quit'" 
  file gamma alpha hcrash anneal dist width 
  file gamma alpha hcrash anneal dist width

type agent_t = VI | Q | QBasis

let get_strs = function
  | VI -> list_map make_vi_str vi_vals2
  | Q  -> list_map make_q_str q_vals
  | QBasis -> list_map make_q_basis_str q_basis_vals

let get_all_strs () = List.flatten @: list_map get_strs [VI;(* Q; QBasis*)]

let main () = 
  let strs = get_all_strs () in
  List.iter (fun s -> print_endline s; ignore(Sys.command s)) strs

let _ = if !Sys.interactive then () else main ()


