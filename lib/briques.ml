

(*Etat Solide : la brique peut être cassée mais ne l'a pas encore été
   Etat Incassable : la brique ne peut pas être cassée
   Etat Invisible : la brique a été solide et a été cassée/n'est plus un obstacle pour la balle*)
type etat = Solide | Incassable | Invisible

module type Briques =
sig
  type p (*type position*)
  type d (*type dimension*)
  type brique = int*etat*p*d (*type brique : tuple d'un score, d'un état, d'une position et de ses dimensions*)
  type briques = brique list
  val getscore : brique -> int
  val getetat : brique -> etat
  val getposition : brique -> p
  val est_solide : brique -> bool
  val est_incassable : brique -> bool
  val est_invisible : brique -> bool
  val getdimension : brique -> d
  val setdimension : brique -> d -> brique
  val setposition : brique -> p -> brique
  val setetat : brique -> etat -> brique
  val setscore : brique -> int -> brique
  val createbrique : int -> etat -> p -> d -> brique
  val setbrique : brique -> int -> etat -> p -> d -> brique
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
  let est_solide b = getetat b = Solide
  let est_incassable b = getetat b = Incassable
  let est_invisible b = getetat b = Invisible
  let setdimension (s,e,p,_) d = (s,e,p,d)
  let setposition (s,e,_,d) p = (s,e,p,d)
  let setetat (s,_,p,d) e = (s,e,p,d)
  let setscore (_,e,p,d) s = (s,e,p,d)
  let createbrique s e p d = (s,e,p,d)
  let setbrique (_,_,_,_) s e p d = (s,e,p,d)
  (*Méthodes spécifiques à la 2D*)
  let sethauteur (s,e,p,(l,_)) h = (s,e,p,(l,h))
  let setlargeur (s,e,p,(_,h)) l = (s,e,p,(l,h))
  let gethauteur (_,_,_,(_,h)) = h
  let getlargeur (_,_,_,(l,_)) = l
end

(*On peut reprendre une autre implémentation d'une fenêtre pour indiquer la fenêtre où générer les briques (ie pas toute la fenêtre affichée) 
   plutôt que les quatre paramètres mais on n'a pas repris ce module encore je crois*)
let genbriques n bxmin bxmax bymin bymax = failwith "not implemented"