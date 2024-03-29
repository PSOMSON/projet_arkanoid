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


let acc = 0.02


let game_hello () = print_endline "Hello, Newtonoiders!"

(* Temporary before we know what to do with it *)
type position = vector2 * vector2
type t_balle = position * int
type t_raquette = Raquette2d.raquette * (float * bool) flux
type state = Briques2d.brique qtree * t_raquette * t_balle

let approx (xa,ya) (xb,yb) (w,h) : bool =
    let (dx, dy) = xb -. xa, yb -. ya in
    Float.(abs dx < w && abs dy < h)

let rec find (x,y) (l : Briques2d.brique feuille list) (w,h) =
    match l with
    | [] -> None
    | {position = p; value = b}::q ->
               if approx p (x,y) (w,h) then Some p else find (x,y) q (w,h)

(* Beaucoup de mots pour faire une grille en m*n avec une brique entre chaque brique
 horizontalement et verticalement*)
let populate_brics r infx infy supx supy nb_briques_x nb_briques_y score_total=
    let infx', supx' = 5.*.infx, supx -. 5.*.infx in
    let infy', supy' = 2.*.supy/.4. +. infy, 3./.4. *.supy in
    let hr = Float.trunc ((supy' -. infy') /. float_of_int nb_briques_y) in
    let wr = Float.trunc ((supx' -. infx') /. float_of_int nb_briques_x) in
    let (w,h) = wr -. 2. *. r, hr -. 2. *. r in
    print_string "w = "; print_float w; print_string " h = "; print_float h; print_newline();
    if w <= 1. || h <= 1. then failwith "Too many bricks";
    let score = score_total / (nb_briques_x * nb_briques_y) in
    let briquenulle = Briques2d.createbrique 0 Invisible (Briques2d.createpos (0.::0.::[])) (Briques2d.createdim (0.::0.::[])) in
    let qtree = createAndInitialize (supx -. infx) (supy -. infy) (wr,hr) briquenulle in
    let lbriques = parcour qtree in
    let rec aux qtree x y =
        if y >= nb_briques_y
            then qtree
        else if x >= nb_briques_x
            then aux qtree 0 (y+1)
        else
            let (xb, yb) = infx' +. (float_of_int x +. 0.5)*.wr, infy' +. (float_of_int y +. 0.5)*.hr in
            let pb = Briques2d.createpos [xb; yb] in
            let d = Briques2d.createdim [w;h] in
            let b = Briques2d.createbrique score Briques.Cassable pb d in
            match find (xb,yb) lbriques (wr,hr) with
            | None -> failwith "Brique non trouvée"
            | Some (xb,yb) ->
                print_string ("pos : x = " ^ string_of_float (xb+.w) ^ " y = " ^ string_of_float (yb+.h) ^ "\n");
                    aux (Quadtree.insertOnInitializedTree qtree {position = (xb, yb) ; value = b}) (x+1) y
    in aux qtree 0 0

    (* Encore incomplet, utilise la fonction ci-dessus, et prend des valeurs initiales temporaires*)
let game_initialize infx infy supx supy nb_briques_x nb_briques_y score_total : state =
    let r = 4 in
    let qtree = populate_brics (float_of_int r) infx infy supx supy nb_briques_x nb_briques_y  score_total in
    let raquette: t_raquette = Raquette.create_raquette_autom supx infx supy infy in
    let position_init = (supx +. infx) /. 2., (supy +. infy) /. 2. in
    let vitesse_init = (0., -100.) in
    let balle = ((position_init, vitesse_init),  r) in (* On démarre à la moitié de l'écran, à gérer plus tard*)
    (qtree, raquette, balle)

let game_step infx supx dt (etat:state) : state =
    let (qtree, raquette, balle) = etat
    in
    let (posb, vitb), r = balle in
    let r' = float_of_int r in
    let (posb', (vx, vy)) = MotionArkanoid.run r' (posb, vitb) in
    let (raq, flux) = raquette in
    let ((xr, yr), _) = Raquette.get_floats_pos raq in
    let (w,h) = Raquette.get_floats_dim raq in
    let posr', flux' = Raquette.get_pos_raq (xr, yr) supx infx w flux in
    let (vxr', vyr') = MotionArkanoid.derivate dt (xr, yr) posr' in
    let raquette' : t_raquette = Raquette2d.(create_raquette (create_pos ([fst posr'; yr], [vxr'; vyr'])) (create_dim [w; h])), flux' in
    let norm_vitb = sqrt (vx ** 2. +. vy ** 2.) in
    let vitb'' = vx +. vx /. norm_vitb *. acc, vy +. vy /. norm_vitb *. acc in
    let balle' = ((posb', vitb''), r) in
    let final : state = (qtree, raquette', balle')
    in final


let en_collision_rectangle : (float * float) * (float * float) -> t_balle -> bool =
    fun ((xr, yr), (w,h)) (((xb, yb), _), r) ->
    let (botx, topx, topy, boty) = (xr -. w /. 2., xr +. w /. 2., yr +. h /. 2., yr -. h /. 2.) in
    let (dtl, dtr, dbl, dbr) = (sqrt ((xb -. botx) ** 2. +. (yb -. topy) ** 2.), sqrt ((xb -. topx) ** 2. +. (yb -. topy) ** 2.), sqrt ((xb -. botx) ** 2. +. (yb -. boty) ** 2.), sqrt ((xb -. topx) ** 2. +. (yb -. boty) ** 2.)) in
    if (dtl <= float_of_int r -. MotionArkanoid.eps  ||
        dtr <= float_of_int r -. MotionArkanoid.eps ||
        dbl <= float_of_int r -. MotionArkanoid.eps ||
        dbr <= float_of_int r -. MotionArkanoid.eps)
        then true
    else
        let (dxl, dxr, dyt, dyb) = xb -. botx, xb -. topx, yb -. topy, yb -. boty in
        let r' = float_of_int r in
        let (dxlr, dxrr, dytr, dybr) = dxl -. r', dxr +. r', dyt +. r', dyb -. r' in
        (dxl >= MotionArkanoid.eps && dxr <= -. MotionArkanoid.eps && dytr <= -. MotionArkanoid.eps && dybr >= MotionArkanoid.eps)
        || (dxlr >= MotionArkanoid.eps && dxrr <= -. MotionArkanoid.eps && dyt <= -. MotionArkanoid.eps && dyb >= MotionArkanoid.eps)

let en_collision_brique  : Briques2d.brique qtree -> t_balle -> bool =
    fun qtree balle ->
    let ((posb, vitb), r) = balle in
    let (posb', vitb') = MotionArkanoid.run (float_of_int r) (posb, vitb) in
    let brique = isOccupied qtree posb in
    match brique with
                | None -> false
                | Some {position=_; value=b} -> if Briques2d.est_cassable b then
                                                    let posbr = match Briques2d.getpos (Briques2d.getposition b) with
                                                                    | [x;y] -> (x,y)
                                                                    | _ -> failwith "Erreur de position"
                                                    in
                                                    let dim = match Briques2d.getdim (Briques2d.getdimension b) with
                                                                    | [w;h] -> (w,h)
                                                                    | _ -> failwith "Erreur : dimension de brique invalide"
                                                    in
                                                    en_collision_rectangle (posbr, dim) ((posb', vitb'), r)
                                                else false

let en_collision_raquette : float -> float -> t_raquette -> t_balle -> bool =
    fun supx infx (raquette, flux) ((posb, vitb), r) ->
    let ((xr, yr), _) = Raquette.get_floats_pos raquette in
    let (w, h) = Raquette.get_floats_dim raquette in
    let posr', _ = Raquette.get_pos_raq (xr, yr) supx infx w flux in
    let (posb', vitb') = MotionArkanoid.run (float_of_int r) (posb, vitb) in
    en_collision_rectangle (posr', (w,h)) ((posb', vitb'), r)

let rebond infx supx dt (etat :state): state =
    let (qtree, raquette, balle) = etat
    in
    let (posb, vitb), r = balle in
    let r' = float_of_int r in
    let (posb', vitb') = MotionArkanoid.run r' (posb, vitb) in
    let (raq, flux) = raquette in
    let ((xr, yr), (vxr', vyr')) = Raquette.get_floats_pos raq in
    let (w,h) = Raquette.get_floats_dim raq in
    let posr', flux' = Raquette.get_pos_raq (xr, yr) supx infx w flux in
    let raquette' : t_raquette = Raquette2d.(create_raquette (create_pos ([fst posr'; snd posr'], [vxr'; vyr'])) (create_dim [w; h])), flux' in
    let brique = isOccupied qtree posb' in
    let qtree' : Briques2d.brique qtree = match brique with
                | None -> qtree
                | Some {position=_; value=b} -> if Briques2d.est_cassable b then Briques.remove_quadtree qtree b else qtree
    in
    let trucb'' : position = match brique with
                | None -> MotionArkanoid.collision dt r' (posb', vitb') (posr', (vxr', vyr')) (w, h) true
                | Some {position=_; value=b} -> if not (Briques2d.est_invisible b) then
                                                    let dim = match Briques2d.getdim (Briques2d.getdimension b) with
                                                    | [wb;hb] -> (wb, hb)
                                                    | _ -> failwith "Erreur dimension brique"
                                                    in let posbr = match Briques2d.getpos (Briques2d.getposition b) with
                                                                    | [xb;yb] -> (xb, yb)
                                                                    | _ -> failwith "Erreur position brique"
                                                    in MotionArkanoid.collision dt r' (posb', vitb') (posbr, (0., 0.)) dim false
                                                else MotionArkanoid.collision dt r' (posb', vitb') (posr', (vxr', vyr')) (w, h) true

    in
    let (pos'', (vx, vy)) = trucb'' in
    let norm_vitb = sqrt (vx ** 2. +. vy ** 2.) in
    let vitb'' = vx +. vx /. norm_vitb *. acc, vy +. vy /. norm_vitb *. acc in
    let balle'' : t_balle = (pos'', vitb''), r in
    let final : state = (qtree', raquette', balle'')
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
            let flux = game_flux infx supx dt etat in
            let f = Flux.unless flux
            (fun (q, r, b) -> en_collision_brique q b || en_collision_raquette supx infx r b)
            (fun x -> run (fonction x))
            in Flux.unless f
            (fun (_, _, b) -> game_end b)
            (fun _ -> Tick (lazy(None)))
    in
    run etat_initial
