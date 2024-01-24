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
  type box = float * float* float
  val bords : box
  val dt : float
  val contact : position -> box -> bool
  val rebond : position -> box -> position
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
  let contact_x r x dx = let (bx, tx, _) = E.bords
  in ((x-.r) < bx && dx < 0.) || ((x+.r) > tx && dx > 0.)  
  let contact_y r y dy = let (_, _, ty) = E.bords
  in ((y+.r) > ty && dy > 0.)

  let rebond_x r x dx = if contact_x r x dx then
    -. dx
  else dx

  let rebond_y r y dy = if contact_y r y dy then
    -. dy
  else dy

    let rec run : position -> float -> position
    = fun ((px, py), (vx, vy)) r ->
      let acceleration =  (0.,g)
      in let (dvx, dvy) = (integre E.dt (vx, vy) acceleration)
      in let (dpx, dpy) = (integre E.dt (px, py) (dvx, dvy))
      in ((dpx, dpy), (rebond_x r dpx dvx, rebond_y r dpy dvy))
end
