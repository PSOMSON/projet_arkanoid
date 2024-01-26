open Quadtree
open Briques
let g = -9.81


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
        frottement +. (ratio) *. (1. -. frottement)



    let eject_circle (px, py) (cx, cy) (vx, vy) r dt =
        let (dirx, diry) = (px -. cx, py -. cy) in
        let (normx, normy) = (dirx /. (sqrt (dirx ** 2. +. diry ** 2.)), diry /. (sqrt (dirx ** 2. +. diry ** 2.))) in
        let (x', y') = (-. normx *. (r +. eps)  +. cx, -. normy *. (r +. eps) +. cy) in
        let (dxdt, dydt) = (x' -. px) /. dt, (y' -. py) /. dt in
        let (normx, normy) = (dxdt /. sqrt((dxdt ** 2. +. dydt ** 2.)), dydt /. sqrt((dxdt ** 2. +. dydt ** 2.))) in
        let vin = (vx, vy) in
        let norm = sqrt (vx ** 2. +. vy ** 2.) in
        let vout = (normx *. norm, normy *. norm)
        in ((x', y'), get_frottement vin vout, (x' -. px, y' -. py))


    (* Try reading that lol *)
    (* ça renvoie un angle bizarre selon l'angle de la balle, voire rapport *)
    let collision : float -> float -> position -> position -> float*float -> bool -> position =
        fun dt r ((px, py), (vx, vy)) ((cx, cy), (cvx, cvy)) (w,h) no_bottom ->
            let (bxr, byr, txr, tyr) = (cx -. r -. w /. 2., cy -. r -. h /. 2. , cx +. r +. w /. 2. , cy +. r +. h /. 2.) in
            let (bx, by, tx, ty) = (cx -. w /. 2., cy -. h /. 2. , cx +. w /. 2. , cy +. h /. 2.) in
            let (disttl, disttr, distbl, distbr) = (sqrt ((px -. bx) ** 2. +. (py -. ty) ** 2.), sqrt ((px -. tx) ** 2. +. (py -. ty) ** 2.), sqrt ((px -. bx) ** 2. +. (py -. by) ** 2.), sqrt ((px -. tx) ** 2. +. (py -. by) ** 2.)) in
            let (dxl, dxr, dyt, dyb) = (px -. bx, px -. tx, py -. ty, py -. by) in
            let (dxlext, dxrext, dytext, dybext) = (px -. bxr, px -. txr , py -. tyr, py -. byr) in
            let (left, up) = (px -. cx < 0., py -. cy > 0.) in
            let norm = sqrt (vx ** 2. +. vy ** 2.) in
            let (a,b), coef_frott, (dx1, dy1)=
                if (dxl >= eps && dxr <= -. eps && dytext <= -. eps && dybext >= eps) then
                    (print_string "middle\n";
                    let dy' = if sign vy = 1. then
                                        -. dybext
                                    else
                                        -. dytext
                    in
                    let dx' = if vy = 0. then 0. else dy' /. vy *. vx in
                    let dy = if up || no_bottom then tyr +. eps -. py else byr -. eps -. py in
                    let dx = if vy = 0. then 0. else  -. dy *. vx /. vy in
                    print_string ("dx = " ^ string_of_float dx ^ ", dy = " ^ string_of_float dy ^ "\n");
                    print_string ("vx = " ^ string_of_float vx ^ ", vy = " ^ string_of_float vy ^ "\n");
                    (px +. dx , py +. dy), frottement, (dx', dy'))
                else if (dxlext >= eps && dxrext <= -. eps && dyt <= -. eps && dyb >= eps) then
                    (print_string "side\n";
                    let dx' = if sign vx = 1. then
                                        -. dxlext
                                    else
                                        -. dxrext
                    in
                    let dy' = if vx = 0. then 0. else dx' /. vx *. vy in
                    let dx = if not left then txr +. eps -. px else bxr -. eps -. px in
                    let dy = if vx = 0. then 0. else  -. dx *. vy /. vx in
                    (px +. dx , py +. dy), 1., (dx', dy'))
                else if disttl <= r -. eps then
                    (print_string "tl\n";
                    eject_circle (px, py) (bxr, tyr) (vx,vy) r dt)
                else if disttr <= r -. eps then
                    (print_string "tr\n";
                    eject_circle (px, py) (txr, tyr) (vx, vy) r dt)
                else if distbl <= r -. eps then
                    (print_string "bl\n";
                    eject_circle (px, py) (bxr, byr) (vx,vy) r dt)
                else if distbr <= r -. eps then
                    (print_string "br\n";
                    eject_circle (px, py) (txr, byr) (vx, vy) r dt)
                else
                    (px, py), 0., (0., 0.)
            in
            let (dxdt, dydt) = (a -. px) /. dt, (b -. py) /. dt in
            let (normx, normy) = (dxdt /. sqrt((dxdt ** 2. +. dydt ** 2.)), dydt /. sqrt((dxdt ** 2. +. dydt ** 2.))) in
            let (vx', vy') = (normx *. norm, normy *. norm) in
            let norme1 = sqrt(dx1 ** 2. +. dy1 ** 2.) in
            let dx2, dy2  = normx *. norme1, normy *. norme1 in
            ((px +. dx1 +. dx2, py +. dy1 +. dy2), (vx' +. coef_frott *. cvx, vy' +. coef_frott *. cvy))

let derivate dt acc flux =
    let iter (acc1, acc2) (flux1, flux2) =
        ((flux1 -. acc1) /. dt, (flux2 -. acc2) /. dt)
    in
    iter acc flux;;
end
