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
   
     (*l'arbre est-il vide ?*)
   val empty : 'a qtree -> bool
   
   val createAndInitialize : float -> float -> float*float -> 'a -> 'a qtree
   
     (*création de l'arbre à partir de la taille de la fenêtre*)
   val create : float -> float -> float*float -> 'a qtree
   (*permet d'insérer un objet 'a à une position donnée par vector2*)
   (*bon, on a éssayé de faire que l'arbre s'agrandis à chaque ajout, mais cela pose 
      d'inéxplicable problèmes de réccursions et on a suffisament perdu de temps comme 
      ça, on vas partir sur une initialisation de l'arbre l'ors de la création avec des 
      nulls partout, la "insert" et donc depecated*) 
   val insert : 'a qtree -> 'a feuille-> 'a qtree
   
   val insertOnInitializedTree : 'a qtree -> 'a feuille -> 'a qtree
   
   val remove : 'a qtree -> vector2 -> 'a -> 'a qtree
   val isOccupied : 'a qtree -> vector2 -> 'a feuille option
   
     (*détection de collision entre un objet de vitesse et de position donnée
        ça renvoie le nouveau vecteur vitesse de l'objet
        si l'objet ne collisionne pas, le vecteur vitesse est inchangé*)
     (*de plus, on renvoie le qtree à jour*)
   val colide : 'a qtree -> float*float -> float*float -> float -> 'a qtree * (float*float)* 'a feuille option 
   
   val print : 'a qtree -> unit
   
   
   