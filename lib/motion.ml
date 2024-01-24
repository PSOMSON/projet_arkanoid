open Iterator
open Quadtree
open Briques
let g = -9.81


type vector2 = float*float (*seras implémenté plus en détail plus tard *)


type position = vector2 * vector2
type taille = float*float
type balle = position * float
type raquette = float (*position sur la ligne, aka la seule valeur qui change*)
type state = Briques2d.brique qtree * raquette * balle

(* Descripteur env. *)
(* On le voudra probablement mis dans un autre fichier, pour avoir acces aux briques pas exemple? *)
module type Env =
sig
  type world
  val dt : float
  val contact : position -> world -> bool
  val rebond : position -> world -> position
end

let integre dt flux =
  (* valeur initiale de l'intégrateur                         *)
  let init = ( 0., 0.) in
  (* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i} *)
  let iter (acc1, acc2) (flux1, flux2) =
    (acc1 +. dt *. flux1, acc2 +. dt *. flux2) in
  (* définition récursive du flux acc                         *)
  let rec acc =
    Tick (lazy (Some (init, Flux.map2 iter acc flux)))
  in acc;;

(* TODO Mise a jour d'etat!! *)
module Motion (E : Env) =
struct
  let rec run : position -> state -> position Flux.t 
    = fun ((px, py), (vx, vy)) ->
      let acceleration = Flux.constant (0.,g)
      in let speed = Flux.map (fun (x,y) -> (x +. vx, y +. vy)) (integre E.dt acceleration)
      in let position = Flux.map (fun (x,y) -> (x +. px, y +. py)) (integre E.dt speed)
      in Flux.unless (Flux.map2 (fun p v -> (p, v)) position speed) 
        (fun b -> (E.contact b world)) 
        (fun (p, v) -> (run (E.rebond (p, v) world) state))
end

module EnvRaquette (E : Env) : Env = struct
  type world = raquette (* decrit la raquette *)
  let dt = E.dt
  let contact p w = false
  let rebond p w = p
end
  
