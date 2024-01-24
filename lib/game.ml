open Quadtree
open Motion
open Briques
open Raquette

module EnvMotion :  Env =
    struct
        type world
        let dt = 1000./.60.
        let contact = fun _ _ -> false
        let rebond = fun p _ -> p
    end

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
    let h = Float.trunc ((supy' -. infy') /. (2.*.float_of_int nb_briques_y)) in
    let w = Float.trunc ((supx' -. infx') /. (2.*.float_of_int nb_briques_x)) in
    let score = score_total / (nb_briques_x * nb_briques_y) in
    let qtree = create (supx -. infx) (supy -. infy) (w,h) in
    let rec aux qtree x y l =
        if y = 2*nb_briques_y then qtree, l
        else if x = 2*nb_briques_x then aux qtree 0 (y+2) l
        else let (xb, yb) = infx' +. (float_of_int x +. 0.5)*.w, infy' +. (float_of_int y +. 0.5)*.h in
        let pb = Briques2d.createpos [xb; yb] in
        let d = Briques2d.createdim [w;h] in
        let b = Briques2d.createbrique score Briques.Cassable pb d in
        aux (insert qtree {position = (xb, yb); value = b}) (x+1) y (b::l)
    in aux qtree 0 0 []

    (* Encore incomplet, utilise la fonction ci-dessus, et prend des valeurs initiales temporaires*)
let game_initialize infx infy supx supy nb_briques_x nb_briques_y score_total : state =
    print_string "Initializing game";
    let qtree, bric_list = populate_brics infx infy supx supy nb_briques_x nb_briques_y  score_total in
    let raquette = Raquette.create_raquette_autom supx infx supy infy in
    let position_init = (supx -. infx) /. 2., (supy -. infy) /. 2. in
    let vitesse_init = (0., 0.) in
    let balle = ((position_init, vitesse_init),  10) in (* On démarre à la moitié de l'écran, à gérer plus tard*)
    print_string "Game initialized !";
    (qtree, raquette, balle, bric_list)

let game_step state =
    state
