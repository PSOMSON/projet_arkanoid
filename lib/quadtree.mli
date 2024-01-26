(*un module quadtree : 
   c'est un arbre quatre-aire qui permet d'optimiser la détection
   de collision.
   décompose la surface de jeux en 4 partie égales puis réccurssivement ces 4 parties etc...
   jusqu'à arriver à des zones élémentaires de taille T : int*int pixels*)

   type vector2 = float*float (*seras implémenté plus en détail plus tard *)
   type 'a feuille = {position : vector2; value : 'a}
   
       (*un arbre est soit vide, soit une feuille, soit un noeud*)
   
       
   type 'a tree = 
           | Empty
           | Leaf of 'a feuille
           | Node of 'a qtree * 'a qtree * 'a qtree * 'a qtree
   and 'a qtree = {tree : 'a tree; size : float*float; resol : float*float}
   
(*fonction empty : 
  in : 'a qtree 
  out : true si l'arbre est vide, false sinon. *)
   val empty : 'a qtree -> bool
   
(*fonction createAndInitialize :
  in : longueure (float) largeur (float) résolution (float*float) valeur par défaut 'a
  out : un arbre 'a qtree initialisé avec des valeurs par défaut 'a*)
   val createAndInitialize : float -> float -> float*float -> 'a -> 'a qtree
   

(*fonction insertOnInitializedTree :
  in : un arbre 'a qtree, une feuille 'a feuille
  out : un arbre 'a qtree avec la feuille 'a feuille insérée*)
(* [!] peu importe la position de la feuille, elle sera échelonné selon la résolution de sorte à ce
   qu'elle soit au centre de la case de la quadtree*)
   
   val insertOnInitializedTree : 'a qtree -> 'a feuille -> 'a qtree
   (*fonction : remove
      in : 'a qtree (arbre), (float*float) (position), valeur par défaut
      out : nouvel arbre*)
   val remove : 'a qtree -> vector2 -> 'a -> 'a qtree

(*fonction : isOccupied
  in : 'a qtree (arbre), (float*float) (position)
  out : 'a feuille option (None si la case est vide, Some feuille si elle est occupée)*)
   val isOccupied : 'a qtree -> vector2 -> 'a feuille option
   
(*fonction : colide
  in : 'a qtree (arbre), (float*float) (position), (float*float) (vitesse), rayon de la balle (float)$
  out : nouvel arbre, nouvelle vitesse, nouvelle feuille option (None si la case est vide)$
  *)
  val colide : 'a qtree -> float*float -> float*float -> float -> 'a qtree * (float*float)* 'a feuille option 
  
(*procédure : print :
in : arbre
permet d'afficher l'arbre*)
  val print : 'a qtree -> unit
   
   
  val parcour : 'a qtree -> ('a -> bool) -> 'aList
(*note : initialiser l'arbre avec toutes les valeurs avec des valeurs par défauts peu sembler peu 
   optimisé par rapport à rajouter au fur et à mesure les branches à chaque insertion
   en réalité, même si il y avait 1000x1000 briques, l'arbre ne serais que de profondeur log4(1000000) ~= 10 donc 
   aucun problème.*)