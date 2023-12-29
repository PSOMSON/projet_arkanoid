(*un module quadtree : 
   c'est un arbre quatre-aire qui permet d'optimiser la détection
   de collision.
   décompose la surface de jeux en 4 partie égales puis réccurssivement ces 4 parties etc...
   jusqu'à arriver à des zones élémentaires de taille T : int*int pixels*)

type vector2 = int*int (*seras implémenté plus en détail plus tard *)

    (*un arbre est soit vide, soit une feuille, soit un noeud*)
type 'a tree = 
    | Empty
    | Leaf of 'a
    | Node of 'a tree * 'a tree * 'a tree * 'a tree

type 'a qtree = {tree : 'a tree; size : int*int}

  (*l'arbre est-il vide ?*)
val empty : 'a qtree -> bool

  (*création de l'arbre à partir de la taille de la fenêtre*)
val create : int -> int -> 'a qtree
val insert : 'a qtree -> 'a -> 'a qtree
val remove : 'a qtree -> 'a -> 'a qtree
val isOccupied : 'a qtree -> vector2 -> bool

  (*détection de collision entre un objet de vitesse et de position donnée
     ça renvoie le nouveau vecteur vitesse de l'objet
     si l'objet ne collisionne pas, le vecteur vitesse est inchangé*)
val colide : 'a qtree -> vector2 -> vector2 -> vector2


