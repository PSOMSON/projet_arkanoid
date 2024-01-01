open Briques


type vector2 = int*int (*seras implémenté plus en détail plus tard *)

type 'a feuille = {position : vector2; value : 'a}

(*une qtree est composée de : 
   un arbre, 
   la taille de la qtree
   la résolution de la qtree
      i.e la taille minimum des éléments de la qtree*)

type 'a tree = 
    | Empty
    | Leaf of 'a feuille
    | Node of 'a qtree * 'a qtree * 'a qtree * 'a qtree
and 'a qtree = {tree : 'a tree; size : int*int; resol : int*int}

let empty arbre = 
  arbre.tree == Empty

let create width height res = 
  {tree = Empty; size = (width, height); resol = res}

let create2 width height res = 
  let arbre_inter = create (width/2) (height/2) res in
  {tree = Node(arbre_inter, arbre_inter, arbre_inter, arbre_inter); size = (width, height); resol = res}


  (*permet de savoir si 2 positions seront égales dans la qtree *)
let equals_with_resol posa posb resol = 
  let (xa,ya) = posa in
  let (xb,yb) = posb in
  let (w,h) = resol in
  (xa/w) == (xb/w) && (ya/h) == (yb/h)

  

let rec insert : 'a qtree -> 'a feuille-> 'a qtree =
  fun arbre value ->

    
    let insert_intermediaire : 'a qtree -> 'a feuille -> 'b qtree =
    fun arbre value ->
      match arbre.tree with
        | Node (a,b,c,d) ->
          let (x,y) = value.position in
          let (w,h) = arbre.size in
          if x < w/2 then 
            if y < h/2 then 
              (*cadran en haut à gauche*)
              {tree = Node ((insert a value), b, c, d); size= arbre.size; resol = arbre.resol}
            else 
              (*cadran en bas à gauche*)
              {tree = Node (a, b, (insert c value), d); size = arbre.size; resol = arbre.resol}
            else 
            if y < h/2 then 
              (*cadran en haut à droite*)
              {tree = Node (a, (insert b value), c, d); size = arbre.size; resol = arbre.resol}
          else 
              (*cadran en bas à droite*)
              {tree = Node (a, b, c, (insert d value)); size = arbre.size; resol = arbre.resol}
        | _ -> failwith "insert_intermediaire : arbre incompatible"
  in


  match arbre.tree with 
    | Empty -> {tree=Leaf value;size=arbre.size; resol = arbre.resol}
    | Leaf v -> if (equals_with_resol v.position value.position arbre.resol ) then {tree=Leaf value;size=arbre.size; resol = arbre.resol}  else 
      (*sinon il faut créer un nouveau noeud*)
      let (width, height) = arbre.size in
      let arbre_inter = create2 width height arbre.resol in
      insert_intermediaire (insert_intermediaire arbre_inter {position = v.position; value = v.value}) {position = value.position; value = value.value}

    | Node (_,_,_,_) -> 
      (*maintenant faut choisir dans quel cadran on répartis la 
      value selon sa position :*)
      insert_intermediaire arbre value


(* ce remove est assez rudimentaire, faudrais le rendre plus opti (supprimer les branches mortes..*)
let rec remove arbre pos = 
  match arbre.tree with 
    | Empty -> {tree=Empty;size=arbre.size; resol = arbre.resol}
    | Leaf v -> if (equals_with_resol (v.position) pos arbre.resol) then {tree=Empty;size=arbre.size; resol = arbre.resol} else {tree=Leaf v;size=arbre.size; resol = arbre.resol}
    | Node (a,b,c,d) -> 
      let (x,y) = pos in
      let (w,h) = arbre.size in
      if x < w/2 then 
        if y < h/2 then 
          {tree=Node ((remove a pos), b, c, d);size=arbre.size; resol = arbre.resol}
        else 
          {tree=Node (a, b, (remove c pos), d); size=arbre.size; resol = arbre.resol}
        else 
        if y < h/2 then 
          {tree=Node (a, (remove b pos), c, d); size=arbre.size; resol = arbre.resol}
        else 
          {tree=Node (a, b, c, (remove d pos)); size=arbre.size; resol = arbre.resol}

let rec isOccupied arbre pos = 
  match arbre.tree with 
    | Empty -> None
    | Leaf v -> if (equals_with_resol (v.position) pos arbre.resol) then Some v.value else None
    | Node (a,b,c,d) -> 
      let (x,y) = pos in
      let (w,h) = arbre.size in
      if x < w/2 then 
        if y < h/2 then 
          isOccupied a pos
        else 
          isOccupied c pos
        else 
        if y < h/2 then 
          isOccupied b pos
        else 
          isOccupied d pos

let rec colide arbre pos vit dt = 
  (*on calcule la position future*)
  (* soucis ! on prend pas en compte la taille de la balle là -_-''*)
  let (x,y) = ((fst pos) +. (fst vit)*.dt, (snd pos) +. (snd vit)*.dt) in
  match isOccupied arbre (int_of_float x,int_of_float y) with 
    | None -> (arbre, vit)
    | Some v -> 
      let (x_inter,y_inter) = (float_of_int (fst v.position), float_of_int (snd v.position)) in
      let (x',y') = (x_inter -. x, y_inter -. y) in
      let new_arbre = remove arbre v.position in
      failwith "colide : TODO : gérer la collision !"
        
          
      
      
      (*TODO : gérer l'incrément du score !, et suppression/décrément? de la brique touchée !
         PS : la brique est stoquée dans v.value !*)
      


