open Iterator
open Input


module type Raquette =
sig
  (*Type position de la raquette*)
  type p 
  (*Type dimension de la raquette*)
  type d 
  (*Type raquette*)
  type raquette = p*d 
  (*Renvoie la position de la raquette*)
  val getposition : raquette -> p 
  (*Renvoie la dimension de la raquette*)
  val getdimension : raquette -> d
  (*Modifie la position de la raquette*)
  val setposition : raquette -> p -> raquette
  (*Modifie la dimension de la raquette*)
  val setdimension : raquette -> d -> raquette
  (*Crée une raquette à partir d'une position et d'une dimension*)
  val create_raquette : p -> d -> raquette
  (*Crée une dimension à partir d'une liste de float*)
  val create_dim : float list -> d
  (*Crée une position à partir d'une liste de float*)
  val create_pos : (float list * float list) -> p
  (*Renvoie la position et la vitesse de la raquette*)
  val getpos : raquette -> (float list *float list)
  (*Renvoie la dimension de la raquette*)
  val getdim : raquette -> float list
end

module Raquette2d : Raquette =
struct
  type p = (float*float)*(float*float)
  type d = (float*float)
  type raquette = p*d
  let getposition (p,_) = p
  let getdimension (_,d) = d
  let setposition (_,d) p = (p,d)
  let setdimension (p,_) d = (p,d)
  let create_raquette p d = (p,d)
  let create_dim l = match l with
    |[a;b] -> (a,b)
    |_ -> failwith "dimension non valide en 2D"
  let create_pos (pos,vit) = match pos,vit with
    |[a;b], [c;d] -> (a,b), (c,d)
    |_ -> failwith "position non valide en 2D"
  let getpos ((p,v),_) = [fst p; snd p], [fst v; snd v]
  let getdim (_,d) = [fst d; snd d]
end

(* Fonction qui permet de récupérer la position et la vitesse de la raquette sous la forme d'un couple de position et vitesse, chacun couple de float
  * Paramètres : la raquette dont on veut récupérer la position et la vitesse
  * Préconditions : aucune
  * Postconditions : aucune
  * Retourne : un couple de float list, le premier étant la position et le second la vitesse
   *)
let get_floats_pos : Raquette2d.raquette -> (float*float)*(float*float) = fun r ->
  let (pos, vit) = Raquette2d.getpos r in
  let y = List.hd (List.tl (pos)) in
  let x = List.hd (pos) in
  let vy = List.hd (List.tl (vit)) in
  let vx = List.hd (vit) in
  (x,y), (vx, vy)

(* Fonction qui permet de récupérer la dimension de la raquette sous la forme d'un couple de float
  * Paramètres : la raquette dont on veut récupérer la dimension
  * Préconditions : aucune
  * Postconditions : aucune
  * Retourne : un couple de float, le premier étant la largeur et le second la hauteur
   *)
let get_floats_dim : Raquette2d.raquette -> float*float = fun r ->
  let x = List.hd (Raquette2d.getdim r) in
  let y = List.hd (List.tl (Raquette2d.getdim r)) in
  (x,y)

(* Fonction qui permet de créer une raquette a permis d'une liste de l'ensemble de ses paramètres 
  * Paramètres : une liste de float, le premier étant la position à droite de la raquette, le deuxième la position à gauche de la raquette, le troisième la position en haut de la raquette, le quatrième la position en bas de la raquette
  * Préconditions : aucune
  * Postconditions : aucune
  * Retourne : une raquette spécifiée par les paramètres en entrée
  *)
let create_raquette_autom : float -> float -> float -> float -> Raquette2d.raquette * (float*bool) flux =
  fun box_xmax box_xmin box_ymax box_ymin ->
    let dimx = 80. in
    let dimy = 20. in
    let posx =  (box_xmax +. box_xmin) /. 2. in
    let posy = 5. +. dimy/.2. in
    let pos = Raquette2d.create_pos ([posx;posy],[0.;0.]) in
    let dim = Raquette2d.create_dim [dimx;dimy] in
    (Raquette2d.create_raquette pos dim), (Input.mouse)

(* test de création d'une raquette *)    
let%test "Création de raquette" =
    let _ = create_raquette_autom 100. 0. 100. 0. in
    true

(* Fonction qui permet de récupérer 
  * Paramètres : le couple de position de la raquette en 2D, la coordonnée gauche de la boîte de jeu, la coordonnée droite de la boîte de jeu, la largeur de la raquette et le flux de la souris
  * Préconditions : aucune
  * Postconditions : aucune
  * Retourne : le couple de position de la raquette en 2D et le flux de la souris
  *)    
let get_pos_raq : float*float -> float -> float -> float -> (float*bool) flux -> (float*float) * (float*bool) flux = fun (x,y) bxmax bxmin lraq flux->
  let return = Flux.uncons flux in
    match return with
    | Some ((x',b), t) -> if b then
                        (if x' > bxmax then (bxmax -. lraq/.2.,y), t
                        else if x' < bxmin then (bxmin +. lraq/.2.,y), t
                        else (x',y), t)
            else (x,y),t
    | None -> failwith "Erreur récupération position de la souris"
