open Quadtree

(* Etat cassable : la brique est visible par l'utilisateur et peut être cassée pour augmenter son score
   * Etat incassable : la brique est visible par l'utilisateur mais ne peut pas être cassée
   * Etat invisible : la brique n'est pas visible par l'utilisateur et a été cassée
   *)

type etat = Cassable | Incassable | Invisible

module type Briques =
sig
   (*type position*)
   type p 
   (*type dimension*)
   type d 
   (*type brique : tuple d'un score, d'un état, d'une position et de ses dimensions*)
   type brique = int*etat*p*d 
   (*type briques : liste de briques*)
   type briques = brique list
   (*Renvoie le score associé à la brique*)
   val getscore : brique -> int
   (*Renvoie l'état de la brique*)
   val getetat : brique -> etat
   (*Renvoie la position de la brique*)
   val getposition : brique -> p
   (*Renvoie si la brique est cassable*)
   val est_cassable : brique -> bool
   (*Renvoie si la brique est incassable*)
   val est_incassable : brique -> bool
   (*Renvoie si la brique est invisible*)
   val est_invisible : brique -> bool
   (*Renvoie les dimensions de la brique*)
   val getdimension : brique -> d
   (*Modifie les dimensions de la brique*)
   val setdimension : brique -> d -> brique
   (*Modifie la position de la brique*)
   val setposition : brique -> p -> brique
   (*Modifie l'état de la brique*)
   val setetat : brique -> etat -> brique
   (*Modifie le score de la brique*)
   val setscore : brique -> int -> brique
   (*Crée une brique*)
   val createbrique : int -> etat -> p -> d -> brique
   (*Modifie une brique*)
   val setbrique : brique -> int -> etat -> p -> d -> brique
   (*Crée une dimension*)
   val createdim : float list -> d
   (*Crée une position*)
   val createpos : float list -> p
   (*Renvoie les dimensions de la brique*)
   val getdim : d -> float list
   (*Renvoie la position de la brique*)
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
  let est_invisible b = getetat b = Invisible
  let setdimension (s,e,p,_) d = (s,e,p,d)
  let setposition (s,e,_,d) p = (s,e,p,d)
  let setetat (s,_,p,d) e = (s,e,p,d)
  let setscore (_,e,p,d) s = (s,e,p,d)
  let createbrique s e p d = (s,e,p,d)
  let setbrique (_,_,_,_) s e p d = (s,e,p,d)
  (*Méthodes spécifiques à la 2D ci-dessous*)
  let createdim l = match l with
    |[a;b] -> (a,b)
    |_ -> failwith "dimension non valide en 2D"
  let createpos l = match l with
    |[a;b] -> (a,b)
    |_ -> failwith "position non valide en 2D"
  let getdim (l,h) = l::h::[]
  let getpos (x,y) = x::y::[]
end


(*************************Fonctions suivantes implémentées pour la 2d seulement**********************************************)

(*Fonction qui renvoie si la brique que l'on souhaite créée chevauche une préexistante
* Paramètres : brique avec laquelle on souhaite comparer la nouvelle brique, position de la nouvelle brique en x, en y, dimensions de la nouvelle brique, hauteur, largeur
* Renvoie un booléen : true si les briques se chevauchent, false sinon
* Préconditions : la brique en entrée n'est pas nulle
* Postconditions : aucune
*)
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

(*Fonction qui renvoie une position aléatoire pour une brique
* Paramètres : bornes en x et en y, hauteur et largeur de la brique
* Renvoie une position
* Préconditions : les bornes sont valides, la hauteur et la largeur sont valides
* Postconditions : aucune
*)
let create_position bxmin bxmax bymin bymax h l =
   let x = Random.float (bxmax -. bxmin -. l) +. bxmin in
   let y = Random.float (bymax -. bymin -. h) +. bymin in
   Briques2d.createpos (x::y::[])

(*Fonction qui renvoie une position aléatoire pour une brique qui ne chevauche pas une brique préexistante
* Paramètres : bornes en x et en y, hauteur et largeur de la brique, liste de briques préexistantes
* Renvoie une position
* Préconditions : les bornes sont valides, la hauteur et la largeur sont valides, la liste de briques est non vide
* Postconditions : aucune
*)
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

(* Fonction qui convertit un objet de type position en couple de float
* Paramètres : position en x, en y
* Renvoie un couple de float
* Préconditions : aucune
* Postconditions : aucune
*)
let recreate_floats : Briques2d.p -> float*float = fun p ->
   let x = List.hd (Briques2d.getpos p) in
   let y = List.hd (List.tl (Briques2d.getpos p)) in
   (x,y)

(* Fonction qui insert une liste de briques dans un quadtree
* Paramètres : liste de briques, quadtree
* Renvoie un quadtree
* Préconditions : aucune
* Postconditions : aucune
*)
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

(* Fonction qui génère une liste de briques aléatoires
* Paramètres : nombre de briques à générer, liste de briques préexistantes, hauteur et largeur maximales et minimales, bornes en x et en y, rapport taille/score, état des briques
* Renvoie une liste de briques
* Préconditions : les bornes sont valides, la hauteur et la largeur sont valides, la liste de briques est non vide
* Postconditions : aucune
*)
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

(* Brique dite nulle pour le quadtree*)
let briquenulle = Briques2d.createbrique 0 Invisible (Briques2d.createpos (0.::0.::[])) (Briques2d.createdim (0.::0.::[]))

(* Fonction qui génère un quadtree de briques aléatoires
* Paramètres : nombre de briques à générer, hauteur et largeur maximales et minimales, bornes en x et en y, rapport taille/score, état des briques
* Renvoie un quadtree
* Préconditions : les bornes sont valides, la hauteur et la largeur sont valides
* Postconditions : aucune
*)
let genbriques n bxmin bxmax bymin bymax hmin hmax lmin lmax rptaillescore etatbrique =
   let quadtree = Quadtree.createAndInitialize (bxmax -. bxmin) (bymax -. bymin) (hmin, lmin) briquenulle in
   let check_param = hmin <= hmax && lmin <= lmax  && n >= 1 && (bxmax -. bxmin) >= lmin && (bymax -. bymin) >= hmin in

      if not check_param then failwith "paramètres non valides" else
         let liste = genrandombrique n [] hmax hmin lmax lmin bxmin bxmax bymin bymax rptaillescore etatbrique in
         insert_quadtree liste quadtree

(*test d'insertion d'une liste de briques dans un quadtree*)
let%test "Terminaison" =
   let _ = genbriques 100 0. 100. 0. 100. 1. 10. 1. 10. 1 Cassable in
   true

(*Fonction qui retire une brique donnée dans un quadtree
* Paramètres : quadtree, brique à retirer
* Renvoie un quadtree
* Préconditions : aucune
* Postconditions : aucune
*)
let remove_quadtree : Briques2d.brique qtree -> Briques2d.brique -> Briques2d.brique qtree = fun ptitree brique ->
   let qtree = Quadtree.remove ptitree (recreate_floats (Briques2d.getposition brique)) briquenulle in
   qtree
