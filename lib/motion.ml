open Iterator
open Quadtree
open Briques
let g = -90.81


type vector2 = float*float (*seras implémenté plus en détail plus tard *)

let croissance = 1.01

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


let integre dt (acc1, acc2) (flux1, flux2) =
    (acc1 +. (dt *. flux1), acc2 +. (dt *. flux2));;

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
            let (bx, _, tx, ty) = (cx -. w /. 2., cy -. h /. 2., cx +. w /. 2., cy +. h /. 2.)
            in let (dlx, drx) = ((px -. bx), (px -. tx))
            in let (rl, rr) = dlx /. r *. (sign vx), drx /. r *. (sign vx)
            in if dlx >= -. r && py -. r <= ty && drx <= -. r then
                let ((a,b),(c,d)) =
                    if rl -. 1. <= 0.01 && rl +. 1. >= -0.01 then
                        let v' = Float.(sin (asin rl) *. (vx +. 0.3 *. cvx), -. cos (asin rl) *. (vy +. 0.3 *. cvy))
                        in print_string ("Vitesse left : " ^ string_of_float (fst v') ^ " " ^ string_of_float (snd v') ^ "\n");
                        ((px, py), v')
                    else if rr -. 1. <= -0.01 && rr +. 1. >= 0.01 then
                        let v' = Float.(sin (asin rr) *. (vx +. 0.3 *. cvx) , -. cos (asin rr) *. (vy +. 0.3 *. cvy))
                        in print_string ("Vitesse right : " ^ string_of_float (fst v') ^ " " ^ string_of_float (snd v') ^ "\n");
                        ((px, py), v')
                    else if Float.abs (rl -. 1.) < 0.01 then (print_string "left\n";
                        ((px, py), (-. vx *. (sign vx) +. cvx, vy +. cvy)))
                    else if Float.abs (rr -. 1.) < 0.01 then (print_string "right\n";
                        ((px, py), (vx *. (sign vx) +. cvx, vy +. cvy)))
                    else (print_string "middle\n";
                        print_string ("cvx : " ^ string_of_float cvx ^ "\n");
                        ((px, py), (vx +. 3. *. cvx, -. vy)))
                in ((a,b),(c*.croissance,d*.croissance))
            else (print_string "no collision\n"; ((px, py), (vx, vy)))



let derivate dt acc flux =
    let iter (acc1, acc2) (flux1, flux2) =
        ((flux1 -. acc1) /. dt, (flux2 -. acc2) /. dt)
    in
    iter acc flux;;
end
