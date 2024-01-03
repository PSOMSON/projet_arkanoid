

(*Etat Solide : la brique peut être cassée mais ne l'a pas encore été
   Etat Incassable : la brique ne peut pas être cassée
   Etat Invisible : la brique a été solide et a été cassée/n'est plus un obstacle pour la balle*)

(*note : on stoque la position de chaque brique comme étant la position de 
   leur coin inférieur gauche !*)
type etat = Cassable | Incassable

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
  (*
  let sethauteur (s,e,p,(l,_)) h = (s,e,p,(l,h))
  let setlargeur (s,e,p,(_,h)) l = (s,e,p,(l,h))
  let gethauteur (_,_,_,(_,h)) = h
  let getlargeur (_,_,_,(l,_)) = l
   *)
  let getdim (l,h) = l::h::[]
  let getpos (x,y) = x::y::[]
end


(*Fonctions suivantes implémentées pour la 2d seulement*)

let chevauchement_brique : Briques2d.brique -> float -> float -> float -> float -> Briques2d.p = fun brique xnew ynew haut larg ->
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
   let x = if xnewop < xbrique then xbrique -. xnewop 
         else if xnew > xbriqueop then xbriqueop -. xnew  
         else if xnewop = xbrique then -0.1 
         else if xnew = xbriqueop then 0.1 
         else 0.0 in
   let y = if ynewop < ybrique then ybrique -. ynewop 
         else if ynew > ybriqueop then ybriqueop -. ynew 
         else if ynewop = ybrique then -0.1 
         else if ynew = ybriqueop then 0.1 
         else 0.0 in
   Briques2d.createpos (x::y::[])

let maj_position brique res = 
   let x = List.hd brique in
   let y = List.hd (List.tl brique) in
   let xres = List.hd res in
   let yres = List.hd (List.tl res) in
   Briques2d.createpos (x +. xres :: y +. yres :: []) 
   
let find_position bxmin bxmax bymin bymax h l liste = 
   let x = Random.float (bxmax -. bxmin -. l) +. bxmin in
   let y = Random.float (bymax -. bymin -. h) +. bymin in
   let p = Briques2d.createpos (x::y::[]) in
   let rec aux : Briques2d.briques -> Briques2d.p -> Briques2d.p = fun lst pos ->
      match lst with
      |[] -> pos
      |b::q -> let res = Briques2d.getpos (chevauchement_brique b x y h l) in
         if res = 0.0::0.0::[] then aux q (Briques2d.createpos (res)) else aux liste (maj_position (Briques2d.getpos(pos)) res)
   in aux liste p

(*On peut reprendre une autre implémentation d'une fenêtre pour indiquer la fenêtre où générer les briques (ie pas toute la fenêtre affichée) 
   plutôt que les quatre paramètres mais on n'a pas repris ce module encore je crois*)

let genbriques n bxmin bxmax bymin bymax hmin hmax lmin lmax rptaillescore etatbrique = 
   let rec aux : int -> Briques2d.briques -> Briques2d.briques = fun n liste ->
      if n = 0 then [] else
         let hauteur = Random.float (hmax -. hmin) +. hmin in
         let largeur = Random.float (lmax -. lmin) +. lmin in
         let dimension =  Briques2d.createdim (largeur::hauteur::[]) in
         let etat = etatbrique in
         let position = find_position bxmin bxmax bymin bymax hauteur largeur liste in
         let score = rptaillescore*(int_of_float (hauteur*.largeur)) in
         let brique = Briques2d.createbrique score etat position dimension in
         brique::(aux (n-1) (brique::liste))
   in aux n []
