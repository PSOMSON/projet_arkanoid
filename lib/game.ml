open Quadtree
open Motion
open Briques

let game_hello () = print_endline "Hello, Newtonoiders!"

(* Temporary before we know what to do with it *)
type position = vector2 * vector2
type t_balle = position * float
type t_raquette = unit
type state = Briques2d.brique qtree * t_raquette * t_balle


let game_initialize infx infy supx supy nb_briques_sup score_total : state =
    let (hmin, hmax) = (infy, supy/.3.) in (* La hauteur de la zone des briques *)
    let (lmin, lmax) = (infx, supx) in (* La largeur de la zone des briques *)
    let qtree = Briques.genbriques nb_briques_sup infx supx infy supy hmin hmax lmin lmax score_total Briques.Cassable in
    let raquette = () (* TODO *) in
    let position_init = (supx -. infx) /. 2., (supy -. infy) /. 2. in
    let vitesse_init = (0., 0.) in
    let balle = ((position_init, vitesse_init), 0.) in (* On démarre à la moitié de l'écran, à gérer plus tard*)
    (qtree, raquette, balle)

let game_step state =

