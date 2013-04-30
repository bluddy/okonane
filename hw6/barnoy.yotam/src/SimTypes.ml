(* types for simulator, to avoid circular inclusion *)

open WorldMap

type simulator_t = {
  world : worldmap_t;
  verbose : bool;
  output_delay : float;      (* delay in ms *)
  last_display : float;      (* last time message was displayed *)
}
