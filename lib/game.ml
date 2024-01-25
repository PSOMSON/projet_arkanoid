open Quadtree
open Motion
open Briques
open Raquette
open Iterator

module EnvMotion :  Env =
    struct
        type box = float * float * float
        let bords = 10. , 790. , 590.
        let dt = 1./.60.
        let contact = fun _ _ -> false
        let rebond = fun p _ -> p
    end

module MotionArkanoid =
    Motion(EnvMotion)




let game_hello () = print_endline "Hello, Newtonoiders!"

(* Temporary before we know what to do with it *)
type position = vector2 * vector2
type t_balle = position * int
type t_raquette = Raquette2d.raquette * (float * bool) flux
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
    let qtree, bric_list = populate_brics infx infy supx supy nb_briques_x nb_briques_y  score_total in
    let raquette: t_raquette = Raquette.create_raquette_autom supx infx supy infy in
    let position_init = (supx +. infx) /. 2., (supy +. infy) /. 2. in
    let vitesse_init = (0., 0.) in
    let balle = ((position_init, vitesse_init),  10) in (* On démarre à la moitié de l'écran, à gérer plus tard*)
    (qtree, raquette, balle, bric_list)

let game_step infx supx dt (etat:state) : state =
    let (qtree, raquette, balle, bric_list) = etat
    in
    let (posb, vitb), r = balle in
    let r' = float_of_int r in
    let (posb', vitb') = MotionArkanoid.run r' (posb, vitb) in
    let (raq, flux) = raquette in
    let ((xr, yr), _) = Raquette.get_floats_pos raq in
    let (w,h) = Raquette.get_floats_dim raq in
    let posr', flux' = Raquette.get_pos_raq (xr, yr) supx infx w flux in
    let (vxr', vyr') = MotionArkanoid.derivate dt (xr, yr) posr' in
            print_string ("Raquette : vitesse = " ^ string_of_float vxr' ^ " " ^ string_of_float vyr' ^ "\n");

    let raquette' : t_raquette = Raquette2d.(create_raquette (create_pos ([fst posr'; yr], [vxr'; vyr'])) (create_dim [w; h])), flux' in
    let balle' = ((posb', vitb'), r) in
    let final : state = (qtree, raquette', balle', bric_list)
    in final


let en_collision_brique  : Briques2d.brique qtree -> t_balle -> bool =
    fun qtree ((posb, _), _) ->

    let brique = isOccupied qtree posb in
    let brique_collision = match brique with
                | None -> false
                | Some {position=_; value=b} -> Briques2d.est_cassable b
    in
    brique_collision

let en_collision_raquette : t_raquette -> t_balle -> bool =
    fun (raquette, _) ((posb, _), r) ->
    let ((xr, yr), _) = Raquette.get_floats_pos raquette in
    let (w, h) = Raquette.get_floats_dim raquette in
    let (xb, yb) = posb in
    let (botx, topx, topy, boty) = (xr -. w /. 2., xr +. w /. 2., yr +. h /. 2., yr -. h /. 2.) in
    let (dtl, dtr) = (sqrt ((xb -. botx) ** 2. +. (yb -. topy) ** 2.), sqrt ((xb -. topx) ** 2. +. (yb -. topy) ** 2.)) in
    if Float.abs (dtl -. float_of_int r) <= 0.1 || Float.abs (dtr -. float_of_int r) <= 0.1
        then true
        else
            let (dxg, dxd, dy) = xb -. botx, xb -. topx, yb -. topy in
            (Float.abs dxg <= 0.1 && dy <= 0.) || (Float.abs dxd <= 0.1 && dy <= 0.1) || (dxg >= 0. && dxd <= 0. && Float.abs (dy -. float_of_int r) <= boty)

let rebond infx supx dt (etat :state): state =
    let (qtree, raquette, balle, bric_list) = etat
    in
    let (posb, vitb), r = balle in
    let r' = float_of_int r in
    let (posb', vitb') = MotionArkanoid.run r' (posb, vitb) in
    let (raq, flux) = raquette in
    let ((xr, yr), _) = Raquette.get_floats_pos raq in
    let (w,h) = Raquette.get_floats_dim raq in
    let posr', flux' = Raquette.get_pos_raq (xr, yr) supx infx w flux in
    let (vxr', vyr') = MotionArkanoid.derivate dt (xr, yr) posr' in
    let raquette' : t_raquette = Raquette2d.(create_raquette (create_pos ([fst posr'; snd posr'], [vxr'; vyr'])) (create_dim [w; h])), flux' in
    let brique = isOccupied qtree posb' in
    let qtree' : Briques2d.brique qtree = match brique with
                | None -> qtree
                | Some {position=_; value=b} -> if Briques2d.est_cassable b then Briques.remove_quadtree qtree b else qtree
    in
    let bric_list' : Briques2d.brique list= match brique with
                | None -> bric_list
                | Some {position=_; value=b} -> List.filter (fun x -> x <> b) bric_list
    in
    let trucb'' : position = match brique with
                | None -> MotionArkanoid.collision r' (posb', vitb') (posr', (vxr', vyr')) (w, h)
                | Some {position=p; value=b} -> if Briques2d.est_cassable b then
                                                    match Briques2d.getdim (Briques2d.getdimension b) with
                                                    | [wb;hb] -> MotionArkanoid.collision r' (posb', vitb') (p,(0.,0.)) (wb,hb)
                                                    | _ -> failwith "Erreur dimension brique"
                else MotionArkanoid.collision r' (posb', vitb') (posr', (vxr', vyr')) (w, h)

    in
    let balle'' : t_balle = trucb'', r in
    let final : state = (qtree', raquette', balle'', bric_list')
    in final

let game_end balle =
    let (((_,y), _), _) = balle in
    y <= 0.




let game_flux infx supx dt etat_initial : state flux =
    let fonction = game_step infx supx dt in
    let rec acc =
        Tick (lazy (Some (etat_initial, Flux.map (fun x -> fonction x) acc)))
    in
    acc

let run_game infx supx dt (etat_initial :state) : state flux =
    let fonction = rebond infx supx dt in
    let rec run : state -> state flux =
        fun etat ->
    let (qtree, raquette, balle, bric_list) = etat in
            let flux = game_flux infx supx dt etat in
            let f = Flux.unless flux
            (fun (q, r, b, _) -> en_collision_brique q b || en_collision_raquette r b)
            (fun x -> run (fonction x))
            in Flux.unless f
            (fun (_, _, b, _) -> game_end b)
            (fun _ -> Tick (lazy(None)))
    in
    run etat_initial
