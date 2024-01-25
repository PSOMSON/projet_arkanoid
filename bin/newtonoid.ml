(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

module Init = struct
  let dt = 1. /. 60. (* 60 Hz *)
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

let draw_state (etat: state) =
    Graphics.set_color Graphics.green;
    Graphics.fill_rect 0 0 800 600;

    let _, raquette, balle, bric_list = etat in
    let (pos_balle,_), r = balle in
    let pos_raquette, _ = Raquette.get_floats_pos raquette in
    let (w,h) = Raquette.get_floats_dim raquette in

    Graphics.set_color Graphics.black;

    Graphics.fill_circle (int_of_float (fst pos_balle)) (int_of_float (snd pos_balle)) r;
    Graphics.draw_circle (int_of_float (fst pos_balle)) (int_of_float (snd pos_balle)) r;
    Graphics.set_color Graphics.blue;
    Graphics.fill_rect (int_of_float (fst pos_raquette -. w/.2.)) (int_of_float (snd pos_raquette -. h/.2.)) (int_of_float w) (int_of_float h);
    Graphics.set_color Graphics.black;
    Graphics.draw_rect (int_of_float (fst pos_raquette -. w/.2.)) (int_of_float (snd pos_raquette -. h/.2.)) (int_of_float w) (int_of_float h);

    let rec draw_brics l =
        match l with
        | [] -> ()
        | b::q -> let pos = Briques.Briques2d.getposition b in
        let (x,y) = match Briques.Briques2d.getpos pos with
        | x::[y] -> (x,y)
        | _ -> failwith "Erreur de position de brique" in
        let dim = Briques.Briques2d.getdimension b in
        let (w,h) = match Briques.Briques2d.getdim dim with
        | w::[h] -> (w,h)
        | _ -> failwith "Erreur de dimension de brique" in
        let x1, x2 = int_of_float (x -. w/.2.), int_of_float (x +. w/.2.) in
        let y1, y2 = int_of_float (y -. h/.2.), int_of_float (y +. h/.2.) in
        Graphics.set_color Graphics.red;
        Graphics.fill_rect x1 y1 (x2-x1) (y2-y1);
        Graphics.set_color Graphics.black;
        Graphics.draw_rect x1 y1 (x2-x1) (y2-y1);
        draw_brics q
    in
    draw_brics bric_list


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
        (*draw states*)
        draw (run_game Box.infx Box.supx Init.dt first_state)
