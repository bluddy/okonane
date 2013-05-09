(* Function approximation with radial basis functions, as well as a simpler map
 * implementation for Q-Agent learning *)
open Util
open State
open Actions
open StateAction
open Pos
module P = Printf

type center_t = state_action_t

(*let num_basis (x,y) =*)
  (*let action_size = List.length legal_actions in*)
  (*let vel_size = max_velocity - min_velocity + 1 in*)
  (*let map_size = x * y in*)

type weight_t = float

type basis_t =
  {
    max_distance : float;
    width : float;
    inv_width_square : float; (* for speed *)
    (* list of weights, centers *)
    params : (weight_t * center_t) list;
  }

let string_of_basis b =
  let s1 = P.sprintf "max_dist: %f, width: %f, inv_w: %f\n" 
    b.max_distance b.width b.inv_width_square in
  let ps = List.fold_left (fun acc (w, c) ->
    acc^"\nbasis w:"^sof w^", c:"^string_of_st_act c) "" b.params
  in
  s1^ps

let new_basis max_dist width =
  {
    max_distance = max_dist;
    width = width;
    inv_width_square = 1. /. (width *. width);
    params = []
  }


let num_params basis = List.length basis.params

(* get the closest distance to a center *)
let get_closest_dist basis x =
  if list_null basis.params then max_float else
  let d = snd @: list_min (fun (_, e) ->
      sam_distance x e) 
    basis.params in
  sqrt d

let add_center_if_needed basis x =
  if get_closest_dist basis x > basis.max_distance then
    let new_params = (0.,x)::basis.params in
    {basis with params = new_params}
  else basis

(* evaluate the main part of the gaussian exponential *)
let eval_gauss gauss_center inv_width_square state = 
  let dist = sam_distance state gauss_center in
  let res = -.dist *. inv_width_square in
  exp res

(* evaluate the value at a certain state *)
let eval_state_val basis (state:center_t) =
  List.fold_left (fun acc (weight, center) ->
      if weight = 0. then acc
      else 
        let g = eval_gauss center basis.inv_width_square state in
        let res = weight *. g in
        acc +. res
      )
    0.
    basis.params

(* update the parameters of the basis functions *)
let update_params basis learn_rate old_val learn_val (state:center_t) =
  let mult = learn_rate *. (learn_val -. old_val) in
  let params = List.rev_map (fun (weight, center) ->
      let delta = mult *. eval_gauss center basis.inv_width_square state in
      (weight +. delta, center)
    )
    basis.params in
  {basis with params = params}

(* a unifying type for a regular value map and basis functions *)
type funmap_t = ValueMap of float SAM.t
              | BasisFuncs of basis_t

(* initialize a new functional mapping, either by basis functions or by simple
 * map *)
let new_funmap use_basis max_dist width = match use_basis with
  | false -> ValueMap(SAM.empty)
  | true  -> BasisFuncs(new_basis max_dist width)

(* lookup a state,action either in the map or in the basis functions *)
let lookup_val st_act = function 
  | ValueMap map     -> sam_lookup_float st_act map
  | BasisFuncs basis -> eval_state_val basis st_act

let lookup_val_m st_act = function
  | ValueMap map     -> sam_lookup_m st_act map
  | BasisFuncs basis -> Some(eval_state_val basis st_act)

(* update the map or basis function to get updated parameters *)
let update_val st_act alpha old_val learn_val = function
  | ValueMap map     -> 
      let exp_val = (1. -. alpha) *. old_val +. alpha *. learn_val in
      let map' = SAM.add st_act exp_val map in
      ValueMap map'
  | BasisFuncs basis ->
      (* first check if we need to add a basis function *)
      let basis'  = add_center_if_needed basis st_act in
      (* now update the basis functions as needed *)
      let basis'' = update_params basis' alpha old_val learn_val st_act in
      BasisFuncs basis''

let dump_funmap visit_map val_map = 
    (* order by state *)
    let st_map = 
       SAM.fold (fun (st,act) visits acc ->
           match find StateMap.find st acc with
           | None         -> StateMap.add st [act,(visits, 0.)] acc
           | Some actions -> StateMap.add st ((act,(visits, 0.))::actions) acc)
         visit_map
         StateMap.empty in
    let st_map =
      StateMap.mapi (fun st l ->
        List.rev_map (fun (act,(v, d)) ->
          act, (v, lookup_val (st,act) val_map)) l
      ) st_map in
    let st_str = StateMap.fold 
      (fun st l acc ->
        acc^string_of_state st^" "^
        (List.fold_left 
          (fun acc (act,(v, d)) -> 
            P.sprintf "%s%s:%d:%.2f" acc (string_of_action act) v d)
          "" l)
        ^"\n")
      st_map 
      ""
    in
    let extra_str = 
      match val_map with
      | ValueMap m -> ""
      | BasisFuncs b -> string_of_basis b
    in
    st_str^extra_str^"\n"

