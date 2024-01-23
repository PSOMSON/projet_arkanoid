(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

module Init = struct
  let dt = 1000. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

let draw_state (qtree, raquette, balle) =
    G


(* extrait le score courant d'un etat : *)
let score state : int = failwith "A DEFINIR"

let draw flux_state =
  let rec loop flux_state last_score =
    match Flux.(uncons flux_state) with
    | None -> last_score
    | Some (state, flux_state') ->
      Graphics.clear_graph ();
      (* DESSIN ETAT *)
      draw_state state;
      (* FIN DESSIN ETAT *)
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_state' (last_score + score state)
    | _ -> assert false
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_state 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

let () = game_hello ();
        let first_state = game_initialize Box.infx Box.infy Box.supx Box.supy 30 10000 in
        let states = Flux.(cons first_state vide)
