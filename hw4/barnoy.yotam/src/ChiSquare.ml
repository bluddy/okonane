open Util

(* this file was translated from the C version at 
 * http://rosettacode.org/wiki/Verify_distribution_uniformity/Chi-squared_test#Python
 * on 4/9/13 *)

(* note: there's some sort of bug that prevents looking up with a dof of 1. We
 * hardcode the answers for that *)
(* another note: this code is really bad. Once you hit >280 dof you get nans.
 * We'll use a hack to make it work in those ranges, but only for 0.05 *)

(*external gamma_q : float -> float -> float = "GammaIncomplete_Q"*)
 
(*/* Numerical integration method */*)

let pi = 3.14159265358979323846

let simpson3_8 f a b n : float =
  let h = (b -. a) /. float_of_int n in
  let h1 = h /. 3. in
  let sum = f a +. f b in

  let rec loop acc i = 
    (*print_endline @: string_of_int i ^ "\n";*)
    match i with
    | j when j <= 0 -> acc
    | j -> 
      let l1 = match j mod 3 with 0 -> 2. | _ -> 3. in
      let v = l1 *. f (a +. h1 *. (float_of_int j)) in
      loop (acc +. v) (j-1)
  in 
  let sum' = loop sum (3 * n - 1) in
  let res = h *. sum' /. 8. in
  (*print_endline @: "simpson: "^string_of_float sum;*)
  res

let a_num = 12

(* base coefficients *)
let coefs = 
  let a = float_of_int a_num in
  let init = [sqrt (2. *. pi)]
  in
  let rec loop k1_factrl c_l = function
    | k when k >= a_num -> c_l
    | k -> 
        let k' = float_of_int k in
        let p = (a -. k') ** (k' -. 0.5) in
        let e = exp (a -. k') in
        let v = e *. p /. k1_factrl in
        loop ((-.k') *. k1_factrl) (v::c_l) (k+1)
  in
  List.rev @: loop 1. init 1

let gamma_spouge z : float =
    let a = float_of_int a_num in

    let accum = fst @:
      List.fold_left (fun (acc,k) x ->
          (acc +. (x /. (z +. k)), k +. 1.)
        )
        (list_head coefs, 1.)
        (list_tail coefs)
    in
    let m = (exp (-.(z +. a))) *. ((z +. a) ** (z +. 0.5)) in
    let accum' = accum *. m in
    accum' /. z
 
let f0 aa1 t = (t ** aa1) *. exp (-.t)
 
let gamma_incomplete_q a x =
  let h = 1.5e-2 in (* approximate integration step size *)

  (* this cuts off the tail of the integration to speed things up *)
  let aa1 = a -. 1. in
  let f0' = f0 aa1 in
  let rec loop y =
    if (f0' y) *. (x -. y) > 2.0e-20 && y < x then loop (y +. 0.3)
    else y
  in 
  let y = loop aa1 in
  let y' = if y > x then x else y in

  (*print_endline @: string_of_float y';*)
  let v = simpson3_8 f0' 0. y' (iof @: y' /. h) in
  let v' = v /. gamma_spouge a in
  1. -. v'

let df1_vals = [0.001, 0.975; 0.004, 0.95; 0.016, 0.9; 2.706, 0.1; 
  3.841, 0.05; 5.024, 0.025; 6.635, 0.01; 7.879, 0.005] 

let high_df_vals = 
  [280., 320.0277; 300., 341.3951; 400., 447.6324; 500., 553.1268; 
  600., 658.0935; 700., 762.6606; 800., 866.9113]

(* use our lists to look up and interpolate (or exterpolate) values *)
let search_interpolate l input =
  let interpolate (x1,y1) (x2,y2) (z:float) =
    let d = (y2 -. y1) /. (x2 -. x1) in
    let delta = d *. (z -. x1) in
    y1 +. delta in
  let l_rev = List.rev l in
  let smaller = list_find (fun (v, _) -> input > v) l_rev in
  let bigger = list_find (fun (v, _) -> input < v) l in
  begin match smaller, bigger with
  | None, None -> invalid_arg "error"
  (* biggest number *)
  | Some x, None -> interpolate (List.nth l_rev 1) x input
  (* smallest number *)
  | None, Some y -> interpolate y (List.nth l 1) input
  (* in the middle *)
  | Some x, Some y -> interpolate x y input
  end

(* the actual probability function *)
let chi2prob dof distance = match dof with
  | 1 -> let v = search_interpolate df1_vals distance in
         if v < 0. then 0. else v
  | x when x > 280 -> (* the code can't handle this range. hack!!! *)
      let point5 = search_interpolate high_df_vals (foi x) in
      if distance >= point5 then 0.
      else 1.
  | _ -> gamma_incomplete_q (0.5 *. (float_of_int dof)) (0.5 *. distance)

