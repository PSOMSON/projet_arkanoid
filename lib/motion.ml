open Iterator
open Quadtree
open Briques
let g = -90.81


type vector2 = float*float (*seras implémenté plus en détail plus tard *)

let croissance = 1.1

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

    let eject_circle (px, py) (cx, cy) r =
        let (dirx, diry) = (px -. cx, py -. cy) in
        let (normx, normy) = (dirx /. (sqrt (dirx ** 2. +. diry ** 2.)), diry /. (sqrt (dirx ** 2. +. diry ** 2.))) in
        (normx *. r +. cx, normy *. r +. cy)


    (* Try reading that lol *)
    (* ça renvoie un angle bizarre selon l'angle de la balle, voire rapport *)
    let collision : float -> float -> position -> position -> float*float -> position =
        fun dt r ((px, py), (vx, vy)) ((cx, cy), (cvx, cvy)) (w,h) ->
            let (bxr, byr, txr, tyr) = (cx -. r -. w /. 2., cy -. r -. h /. 2. , cx +. r +. w /. 2. , cy +. r +. h /. 2.) in
            let (bx, by, tx, ty) = (cx -. w /. 2., cy -. h /. 2. , cx +. w /. 2. , cy +. h /. 2.) in
            let (disttl, disttr, distbl, distbr) = (sqrt ((px -. bxr) ** 2. +. (py -. tyr) ** 2.), sqrt ((px -. txr) ** 2. +. (py -. tyr) ** 2.), sqrt ((px -. bxr) ** 2. +. (py -. byr) ** 2.), sqrt ((px -. txr) ** 2. +. (py -. byr) ** 2.)) in
            let (dxl, dxr, dyt, dyb) = (px -. bx, px -. tx, py -. ty, py -. by) in
            let (dxlext, dxrext, dytext, dybext) = (px -. bxr, px -. txr, py -. tyr, py -. byr) in
            let (left, up) = (px -. cx < 0., py -. cy > 0.) in
            let (a,b) =
                if (dxl >= 0. && dxr <= 0. && dytext <= 0. && dybext >= 0.) then
                    if up then (px,tyr) else (px, byr)
                else if (dxlext >= 0. && dxrext <= 0. && dyt <= 0. && dyb >= 0.) then
                    if left then (bxr, py) else (txr, py)
                else if disttl <= r then
                    eject_circle (px, py) (bxr, tyr) r
                else if disttr <= r then
                    eject_circle (px, py) (txr, tyr) r
                else if distbl <= r then
                    eject_circle (px, py) (bxr, byr) r
                else if distbr <= r then
                    eject_circle (px, py) (txr, byr) r
                else
                    (px, py)
            in
            let norm = sqrt (vx ** 2. +. vy ** 2.) *. sqrt (cvx ** 2. +. cvy ** 2.) in
            let (dxdt, dydt) = (a -. px) /. dt, (b -. py) /. dt in
            let (vx', vy') = (dxdt *. norm, dydt *. norm) in
            let prod_scal = vx' *. cvx +. vy' *. cvy in
            ((a,b), (vx' /. norm *. prod_scal, vy' /. norm *. prod_scal))

let derivate dt acc flux =
    let iter (acc1, acc2) (flux1, flux2) =
        ((flux1 -. acc1) /. dt, (flux2 -. acc2) /. dt)
    in
    iter acc flux;;
end
