open Iterator
open Quadtree
open Briques
let g = -90.81


type vector2 = float*float (*seras implémenté plus en détail plus tard *)

let frottement = 0.6
let eps = 0.1

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
    let eps = eps
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
        (-. normx *. (r +. eps)  +. cx, -. normy *. (r +. eps) +. cy)

    let sign x = if x > 0. then 1. else -1.

    let get_frottement (vx1, vy1) (vx2, vy2) =
        let (dir_minx, dir_miny) = (vx1, -. vy1) in
        let (norm_minx, norm_miny) = (dir_minx /. (sqrt (dir_minx ** 2. +. dir_miny ** 2.)), dir_miny /. (sqrt (dir_minx ** 2. +. dir_miny ** 2.))) in
        let (norm_vx2, norm_vy2) = (vx2 /. (sqrt (vx2 ** 2. +. vy2 ** 2.)), vy2 /. (sqrt (vx2 ** 2. +. vy2 ** 2.))) in
        let sens = sign (vx1 *. vy1) in
        let theta_min = if norm_minx = 0. then Float.pi /. 2.
                        else atan (norm_miny /. norm_minx) in
        let theta_max = theta_min -. (Float.pi *. sens) in
        let theta_mod = atan (norm_vy2 /. norm_vx2) in
        let theta = if theta_mod -. sens *. theta_min > 0. then theta_mod -. sens *. Float.pi else theta_mod in
        let ratio = (theta -. theta_min) /. (theta_max -. theta_min) in
        (ratio) *. (1. -. frottement)



    (* Try reading that lol *)
    (* ça renvoie un angle bizarre selon l'angle de la balle, voire rapport *)
    let collision : float -> float -> position -> position -> float*float -> position =
        fun dt r ((px, py), (vx, vy)) ((cx, cy), (cvx, cvy)) (w,h) ->
            let (bxr, byr, txr, tyr) = (cx -. r -. w /. 2., cy -. r -. h /. 2. , cx +. r +. w /. 2. , cy +. r +. h /. 2.) in
            let (bx, by, tx, ty) = (cx -. w /. 2., cy -. h /. 2. , cx +. w /. 2. , cy +. h /. 2.) in
            let (disttl, disttr, distbl, distbr) = (sqrt ((px -. bx) ** 2. +. (py -. ty) ** 2.), sqrt ((px -. tx) ** 2. +. (py -. ty) ** 2.), sqrt ((px -. bx) ** 2. +. (py -. by) ** 2.), sqrt ((px -. tx) ** 2. +. (py -. by) ** 2.)) in
            let (dxl, dxr, dyt, dyb) = (px -. bx, px -. tx, py -. ty, py -. by) in
            let (dxlext, dxrext, dytext, dybext) = (px -. bxr, px -. txr , py -. tyr, py -. byr) in
            let (left, up) = (px -. cx < 0., py -. cy > 0.) in
            let (a,b) =
                if (dxl >= eps && dxr <= -. eps && dytext <= -. eps && dybext >= eps) then
                    (print_string "middle\n";
                    let dy = if up then tyr +. eps -. py else byr -. eps -. py in
                    let dx = if vx = 0. then 0. else  -. dy *. vx /. vy in
                    (px +. dx , py +. dy))
                else if (dxlext >= eps && dxrext <= -. eps && dyt <= -. eps && dyb >= eps) then
                    (print_string "side\n";
                    if left then (bxr, py) else (txr, py))
                else if disttl <= r -. eps then
                    (print_string "tl\n";
                    eject_circle (px, py) (bxr, tyr) r)
                else if disttr <= r -. eps then
                    (print_string "tr\n";
                    eject_circle (px, py) (txr, tyr) r)
                else if distbl <= r -. eps then
                    (print_string "bl\n";
                    eject_circle (px, py) (bxr, byr) r)
                else if distbr <= r -. eps then
                    (print_string "br\n";
                    eject_circle (px, py) (txr, byr) r)
                else
                    (px, py)
            in
            let norm = sqrt (vx ** 2. +. vy ** 2.) in
            let (dxdt, dydt) = (a -. px) /. dt, (b -. py) /. dt in
            let (normx, normy) = (dxdt /. sqrt((dxdt ** 2. +. dydt ** 2.)), dydt /. sqrt((dxdt ** 2. +. dydt ** 2.))) in
            let (vx', vy') = (normx *. norm, normy *. norm) in
            let coef = get_frottement (vx, vy) (vx', vy') in
            ((px,py), (vx' +. coef *. cvx, vy' +. coef *. cvy))

let derivate dt acc flux =
    let iter (acc1, acc2) (flux1, flux2) =
        ((flux1 -. acc1) /. dt, (flux2 -. acc2) /. dt)
    in
    iter acc flux;;
end
