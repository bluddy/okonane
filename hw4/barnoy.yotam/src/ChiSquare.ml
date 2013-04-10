open Util

(* this file was translated from the C version at 
 * http://rosettacode.org/wiki/Verify_distribution_uniformity/Chi-squared_test#Python
 * on 4/9/13 *)

(* note: there's some sort of bug that prevents looking up with a dof of 1. We
 * hardcode the answers for that *)

(*external gamma_q : float -> float -> float = "GammaIncomplete_Q"*)
 
(*/* Numerical integration method */*)

let pi = 3.14159265358979323846

let simpson3_8 f a b n : float =
  let h = (b -. a) /. float_of_int n in
  let h1 = h /. 3. in
  let sum = f a +. f b in

  let rec loop acc i = 
    print_endline @: string_of_int i ^ "\n";
    match i with
    | j when j <= 0 -> acc
    | j -> 
      let l1 = match j mod 3 with 0 -> 2. | _ -> 3. in
      let v = l1 *. f (a +. h1 *. (float_of_int j)) in
      loop (acc +. v) (j-1)
  in 
  let sum' = loop sum (3 * n - 1) in
  let res = h *. sum' /. 8. in
  print_endline @: "simpson: "^string_of_float sum;
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
    if (f0' y) *. (x -. y) > 2.0e-8 && y < x then loop (y +. 0.4)
    else y
  in 
  let y = loop aa1 in
  let y' = if y > x then x else y in

  print_endline @: string_of_float y';
  let v = simpson3_8 f0' 0. y' (int_of_float @: y' /. h) in
  let v' = v /. gamma_spouge a in
  1. -. v'

let df1_vals = [0.001, 0.975; 0.004, 0.95; 0.016, 0.9; 2.706, 0.1; 
  3.841, 0.05; 5.024, 0.025; 6.635, 0.01; 7.879, 0.005] 

(* the actual probability function *)
let chi2prob dof distance = match dof with
  | 1 -> let df1_rev = List.rev df1_vals in
    let smaller = list_find (fun (v, _) -> distance > v) df1_rev in
    let bigger = list_find (fun (v, _) -> distance < v) df1_vals in
    begin match smaller, bigger with
    | None, None -> invalid_arg "error"
    | Some x, None | None, Some x -> snd x
    | Some (x1, x2), Some (y1, y2) -> 
        let delta = (distance -. x1) /. (y1 -. x1) in
        let delta' = delta *. (y2 -. x2) in
        x2 +. delta'
    end
  | _ -> gamma_incomplete_q (0.5 *. (float_of_int dof)) (0.5 *. distance)

