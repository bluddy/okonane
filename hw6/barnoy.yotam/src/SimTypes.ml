(* types for simulator, to avoid circular inclusion *)

open WorldMap
open TransFunc

type sim_t = {
  world : worldmap_t;
  trans_fn : trans_fn_t;
  verbose : bool;
  output_delay : int;      (* delay in ms *)
  last_display : float;      (* last time message was displayed *)
}
