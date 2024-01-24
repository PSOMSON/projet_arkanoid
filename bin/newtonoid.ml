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

let score_total = 10000

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

let draw_state (_, raquette, balle, bric_list) =


(* extrait le score courant d'un etat : *)
let score (_, _, _, bric_list) : int =
    let rec remaining_score l =
        match l with
        | [] -> 0
        | h::t -> if Briques.Briques2d.getetat h = Briques.Cassable then (Briques.Briques2d.getscore h) + (remaining_score t) else remaining_score t
    in score_total - (remaining_score bric_list)


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
        let nb_bloc_x = 10 in
        let nb_bloc_y = 5 in
        let first_state = game_initialize Box.infx Box.infy Box.supx Box.supy nb_bloc_x nb_bloc_y score_total in
        let states = Flux.(cons first_state vide) in ()
