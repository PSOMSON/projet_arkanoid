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

let integre dt acc flux =
  (* fonction auxiliaire de calcul de acc_{i} + dt * flux_{i} *)
  let iter (acc1, acc2) (flux1, flux2) =
    (acc1 +. dt *. flux1, acc2 +. dt *. flux2)
  (* définition récursive du flux acc                         *)
  in iter acc flux;;

(*TODO Mise a jour d'etat!! *)
module Motion (E : Env) =
struct
  let rec run : position -> position 
    = fun ((px, py), (vx, vy)) ->
      let acceleration =  (0.,g)
      in let speed = (integre E.dt (vx, vy) acceleration)
      in let position = (integre E.dt (px, py) speed)
      in (position, speed)
end

module EnvRaquette (E : Env) : Env = struct
  type world = raquette (* decrit la raquette *)
  let dt = E.dt
  let contact p w = false
  let rebond p w = p
end
 
