open Quadtree
open Motion
open Briques
open Raquette
open Iterator

module EnvMotion :  Env =
    struct
        type box = float * float * float
        let bords = 10. , 790. , 590.
        let dt = 1000./.60.
        let contact = fun _ _ -> false
        let rebond = fun p _ -> p
    end

module MotionArkanoid =
    Motion(EnvMotion)




let game_hello () = print_endline "Hello, Newtonoiders!"

(* Temporary before we know what to do with it *)
type position = vector2 * vector2
type t_balle = position * int
type t_raquette = Raquette2d.raquette
type state = Briques2d.brique qtree * t_raquette * t_balle * Briques2d.brique list

(* Beaucoup de mots pour faire une grille en m*n avec une brique entre chaque brique
 horizontalement et verticalement*)
let populate_brics infx infy supx supy nb_briques_x nb_briques_y score_total=
    let infx', supx' = 2.*.infx, supx -. infx in
    let infy', supy' = 2.*.supy/.3. +. infy, supy -. infy in
    let h = Float.trunc ((supy' -. infy') /. (2.*.float_of_int nb_briques_y -. 1.)) in
    let w = Float.trunc ((supx' -. infx') /. (2.*.float_of_int nb_briques_x -. 1.)) in
    let score = score_total / (nb_briques_x * nb_briques_y) in
    let briquenulle = Briques2d.createbrique 0 Invisible (Briques2d.createpos (0.::0.::[])) (Briques2d.createdim (0.::0.::[])) in
    let qtree = createAndInitialize (supx -. infx) (supy -. infy) (w,h) briquenulle in
    let rec aux qtree x y l =
        if y >= 2*nb_briques_y then qtree, l
        else if x >= 2*nb_briques_x then aux qtree 0 (y+2) l
        else let (xb, yb) = infx' +. (float_of_int x +. 0.5)*.w, infy' +. (float_of_int y +. 0.5)*.h in
        let pb = Briques2d.createpos [xb; yb] in
        let d = Briques2d.createdim [w;h] in
        let b = Briques2d.createbrique score Briques.Cassable pb d in
        aux (insertOnInitializedTree qtree {position = (xb, yb); value = b}) (x+2) y (b::l)
    in aux qtree 0 0 []

    (* Encore incomplet, utilise la fonction ci-dessus, et prend des valeurs initiales temporaires*)
let game_initialize infx infy supx supy nb_briques_x nb_briques_y score_total : state =
    print_string "Initializing game \n";
    let qtree, bric_list = populate_brics infx infy supx supy nb_briques_x nb_briques_y  score_total in
    let raquette = Raquette.create_raquette_autom supx infx supy infy in
    let position_init = (supx +. infx) /. 2., (supy +. infy) /. 2. in
    let vitesse_init = (0., 0.) in
    let balle = ((position_init, vitesse_init),  10) in (* On démarre à la moitié de l'écran, à gérer plus tard*)
    print_string "Game initialized ! \n";
    (qtree, raquette, balle, bric_list)

let game_step infx supx dt (etat:state) : state =
    let (qtree, raquette, balle, bric_list) = etat
    in
    let (posb, vitb), r = balle in
    let r' = float_of_int r in
    let (posb', vitb') = MotionArkanoid.run r' (posb, vitb) in
    let raq = raquette in
    let ((xr, yr), _) = Raquette.get_floats_pos raq in
    let (w,h) = Raquette.get_floats_dim raq in
    let posr' = Raquette.get_pos_raq supx infx w yr in
    let (vxr', vyr') = MotionArkanoid.derivate dt (xr, yr) posr' in
    let raquette' : t_raquette = Raquette2d.(create_raquette (create_pos ([fst posr'; snd posr'], [vxr'; vyr'])) (create_dim [w; h])) in
    let brique = isOccupied qtree posb' in
    let qtree' : Briques2d.brique qtree = match brique with
                | None -> qtree
                | Some {position=_; value=b} -> Briques.remove_quadtree qtree b
    in
    let bric_list' : Briques2d.brique list= match brique with
                | None -> bric_list
                | Some {position=_; value=b} -> List.filter (fun x -> x <> b) bric_list
    in
    let trucb'' : position = match brique with
                | None -> MotionArkanoid.collision r' (posb', vitb') (posr', (vxr', vyr')) (w, h)

                | Some {position=p; value=b} -> match Briques2d.getdim (Briques2d.getdimension b) with
                                                | [w;h] -> MotionArkanoid.collision r' (posb', vitb') (p,(0.,0.)) (w,h)
                                                | _ -> failwith "Erreur dimension brique"

    in
    let balle'' : t_balle = trucb'', r in
    let final : state = (qtree', raquette', balle'', bric_list')
    in final

let game_flux infx supx dt etat_initial =
    let fonction = game_step infx supx dt in
    let rec acc =
        Tick (lazy (Some (etat_initial, Flux.map (fun x ->fonction x) acc)))
    in
    acc
