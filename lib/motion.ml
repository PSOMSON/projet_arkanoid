open Iterator
open Quadtree
open Briques
let g = -9.81


type vector2 = float*float (*seras implémenté plus en détail plus tard *)

let croissance = 0.003

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

  let run : float -> position -> position
    = fun r ((px, py), (vx, vy)) ->
      let acceleration =  (0.,g)
      in let (dvx, dvy) = (integre E.dt (vx, vy) acceleration)
      in let (dpx, dpy) = (integre E.dt (px, py) (dvx, dvy))
      in ((dpx, dpy), (rebond_x r dpx dvx, rebond_y r dpy dvy))


    let sign x = if x > 0. then 1. else -1.

    (* Try reading that lol *)
    (* ça renvoie un angle bizarre selon l'angle de la balle, voire rapport *)
    let collision : float -> position -> position -> float*float -> position =
        fun r ((px, py), (vx, vy)) ((cx, cy), (cvx, cvy))(w,h) ->
            let (bx, by, tx, ty) = (cx -. w /. 2., cy -. h /. 2., cx +. w /. 2., cy +. h /. 2.)
            in let (dx, dy) = ((px -. bx), (py -. tx))
            in let (rl, rr) = dx /. r *. (sign vx), dx /. r *. (sign vx)
            in let ((a,b),(c,d)) = if px +. r >= bx && py -. r <= ty && px -. r <= tx then
                if rl -. 1. <= -0.1 && rl +. 1. >= 0.1 then
                    let v' = Float.(cos (asin rl) *. (vx +. 0.3 *. cvx), -. sin (asin rl) *. (vy +. 0.3 *. cvy))
                    in ((px, py), v')
                else if rr -. 1. <= -0.1 && rr +. 1. >= 0.1 then
                    let v' = Float.(cos (asin rr) *. (vx +. 0.3 *. cvx) , -. sin (asin rr) *. (vy +. 0.3 *. cvy))
                    in ((px, py), v')
                else if Float.abs (rl -. 1.) < 0.1 then ((px, py), (-. vx +. cvx, vy +. cvy))
                else if Float.abs (rr -. 1.) < 0.1 then ((px, py), (-. vx +. cvx, vy +. cvy))
                else ((px, py), (vx +. 0.3*. cvx, -. vy +. 0.3*. cvy))
            else ((px, py), (vx, vy))
            in ((a,b),(c*.croissance,d*.croissance))


let derivate dt acc flux =
    let iter (acc1, acc2) (flux1, flux2) =
        ((flux1 -. acc1) /. dt, (flux2 -. acc2) /. dt)
    in
    iter acc flux;;
end
