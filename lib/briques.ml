open Quadtree

(*Etat Solide : la brique peut être cassée mais ne l'a pas encore été
   Etat Incassable : la brique ne peut pas être cassée
   Etat Invisible : la brique a été solide et a été cassée/n'est plus un obstacle pour la balle*)

(*note : on stoque la position de chaque brique comme étant la position de 
   leur coin inférieur gauche !*)
type etat = Cassable | Incassable | Invisible

module type Briques =
sig
  type p (*type position*)
  type d (*type dimension*)
  type brique = int*etat*p*d (*type brique : tuple d'un score, d'un état, d'une position et de ses dimensions*)
  type briques = brique list
  val getscore : brique -> int
  val getetat : brique -> etat
  val getposition : brique -> p
  val est_cassable : brique -> bool
  val est_incassable : brique -> bool
  val getdimension : brique -> d
  val setdimension : brique -> d -> brique
  val setposition : brique -> p -> brique
  val setetat : brique -> etat -> brique
  val setscore : brique -> int -> brique
  val createbrique : int -> etat -> p -> d -> brique
  val setbrique : brique -> int -> etat -> p -> d -> brique
  val createdim : float list -> d
  val createpos : float list -> p
  val getdim : d -> float list
  val getpos : p -> float list
end

module Briques2d : Briques =
struct
  type p = (float * float) (*Dimension 2*)
  type d = (float * float) (*Dimension 2*)
  type brique = int*etat*p*d
  type briques = brique list
  let getscore (s,_,_,_) = s
  let getetat (_,e,_,_) = e
  let getposition (_,_,p,_) = p
  let getdimension (_,_,_,d) = d
  let est_cassable b = getetat b = Cassable
  let est_incassable b = getetat b = Incassable
  let setdimension (s,e,p,_) d = (s,e,p,d)
  let setposition (s,e,_,d) p = (s,e,p,d)
  let setetat (s,_,p,d) e = (s,e,p,d)
  let setscore (_,e,p,d) s = (s,e,p,d)
  let createbrique s e p d = (s,e,p,d)
  let setbrique (_,_,_,_) s e p d = (s,e,p,d)
  (*Méthodes spécifiques à la 2D*)
  let createdim l = match l with
    |[a;b] -> (a,b)
    |_ -> failwith "dimension non valide en 2D"
  let createpos l = match l with
    |[a;b] -> (a,b)
    |_ -> failwith "position non valide en 2D"
  let getdim (l,h) = l::h::[]
  let getpos (x,y) = x::y::[]
end


(*Fonctions suivantes implémentées pour la 2d seulement*)

let chevauchement_brique : Briques2d.brique -> float -> float -> float -> float -> bool = fun brique xnew ynew haut larg ->
   let xpos = Briques2d.getpos (Briques2d.getposition brique) in
   let xdim = Briques2d.getdim (Briques2d.getdimension brique) in
   let xbrique = List.hd xpos in
   let ybrique = List.hd (List.tl xpos) in
   let lbrique = List.hd xdim in
   let hbrique = List.hd (List.tl xdim) in
   let xbriqueop = xbrique +. lbrique in
   let ybriqueop = ybrique +. hbrique in
   let xnewop = xnew +. larg in
   let ynewop = ynew +. haut in
   let plusx = xnew > xbriqueop in
   let moinsx = xnewop < xbrique in
   let plusy = ynew > ybriqueop in
   let moinsy = ynewop < ybrique in
   if plusx || moinsx || plusy || moinsy then false
   else true

let create_position bxmin bxmax bymin bymax h l = 
   let x = Random.float (bxmax -. bxmin -. l) +. bxmin in
   let y = Random.float (bymax -. bymin -. h) +. bymin in
   Briques2d.createpos (x::y::[])
   
let find_position bxmin bxmax bymin bymax h l liste = 
   Printf.printf "Je cherche une position\n";
   let x = Random.float (bxmax -. bxmin -. l) +. bxmin in
   let y = Random.float (bymax -. bymin -. h) +. bymin in
   let p = Briques2d.createpos (x::y::[]) in
   let nmax = 2*(List.length liste + 1)+1 in
      Printf.printf "Je vais boucler au maximum de %d fois\n" nmax;
      let rec aux : Briques2d.briques -> Briques2d.p -> int -> Briques2d.p = fun lst pos n ->
         match lst,n with
         | _,0 -> raise Not_found
         |[],_ -> pos
         |b::q,_ -> let x = List.hd (Briques2d.getpos pos) in
            let y = List.hd (List.tl (Briques2d.getpos pos)) in
            if (chevauchement_brique b x y h l) then aux liste (create_position bxmin bxmax bymin bymax h l) n  else aux q pos (n-1)
      in aux liste p nmax

(*On peut reprendre une autre implémentation d'une fenêtre pour indiquer la fenêtre où générer les briques (ie pas toute la fenêtre affichée) 
   plutôt que les quatre paramètres mais on n'a pas repris ce module encore je crois*)


let recreate_floats : Briques2d.p -> float*float = fun p ->
   let x = List.hd (Briques2d.getpos p) in
   let y = List.hd (List.tl (Briques2d.getpos p)) in
   (x,y)

let rec insert_quadtree : Briques2d.briques -> Briques2d.brique qtree -> Briques2d.brique qtree = fun liste ptitree ->
   match liste with
   
   |[] -> failwith "liste vide"
   |b::[] -> let feuille = {position = recreate_floats (Briques2d.getposition b); value = b} in
      Printf.printf "J'insère la dernière brique\n";
      Quadtree.insertOnInitializedTree ptitree feuille

   |b::q -> let feuille = {position = recreate_floats (Briques2d.getposition b); value = b} in
      Printf.printf "J'insère une brique\n";
      let newquadtree = Quadtree.insertOnInitializedTree ptitree feuille in
      insert_quadtree q newquadtree

let rec genrandombrique : int -> Briques2d.briques -> float -> float -> float -> float -> float -> float -> float -> float -> int -> etat -> Briques2d.briques = fun n liste hmax hmin lmax lmin bxmax bxmin bymax bymin rptaillescore etatbrique->
   if n = 0 then [] else
      let hauteur = Random.float (hmax -. hmin) +. hmin in
      let largeur = Random.float (lmax -. lmin) +. lmin in
      let dimension =  Briques2d.createdim (largeur::hauteur::[]) in
      let etat = etatbrique in
      try
         let position = find_position bxmin bxmax bymin bymax hauteur largeur liste in
         let score = (1/rptaillescore)*(int_of_float (hauteur*.largeur)) in
         let brique = Briques2d.createbrique score etat position dimension in
         brique::(genrandombrique (n-1) (brique::liste) hmax hmin lmax lmin bxmax bxmin bymax bymin rptaillescore etatbrique)
      with | Not_found -> Printf.printf "je n'ai pas inséré une brique"; genrandombrique 0 liste hmax hmin lmax lmin bxmax bxmin bymax bymin rptaillescore etatbrique

let briquenulle = Briques2d.createbrique 0 Invisible (Briques2d.createpos (0.::0.::[])) (Briques2d.createdim (0.::0.::[])) 
   
let genbriques n bxmin bxmax bymin bymax hmin hmax lmin lmax rptaillescore etatbrique =
   let quadtree = Quadtree.createAndInitialize (bxmax -. bxmin) (bymax -. bymin) (hmin, lmin) briquenulle in
   let check_param = hmin <= hmax && lmin <= lmax  && n >= 1 && (bxmax -. bxmin) >= lmin && (bymax -. bymin) >= hmin in
   
      if not check_param then failwith "paramètres non valides" else
         let liste = genrandombrique n [] hmax hmin lmax lmin bxmin bxmax bymin bymax rptaillescore etatbrique in
         insert_quadtree liste quadtree


let%test "Terminaison" = 
   let _ = genbriques 100 0. 100. 0. 100. 1. 10. 1. 10. 1 Cassable in
   true

(*envoie celle à enlever et j'enlève du quadtree, mise à Invisible*)

let remove_quadtree : Briques2d.brique qtree -> Briques2d.brique -> Briques2d.brique qtree = fun ptitree brique ->
   let qtree = Quadtree.remove ptitree (recreate_floats (Briques2d.getposition brique)) briquenulle in
   qtree
