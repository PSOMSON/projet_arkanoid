open Briques

type vector2 = int*int (*seras implémenté plus en détail plus tard *)

type 'a feuille = {position : vector2; value : 'a}

type 'a tree = 
    | Empty
    | Leaf of 'a feuille
    | Node of 'a qtree * 'a qtree * 'a qtree * 'a qtree
and 'a qtree = {tree : 'a tree; size : int*int}

let empty arbre = 
  arbre.tree == Empty

let create width height  = 
  {tree = Empty; size = (width, height)}

let create2 width height = 
  let arbre_inter = create (width/2) (height/2) in
  {tree = Node(arbre_inter, arbre_inter, arbre_inter, arbre_inter); size = (width, height)}



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
              {tree = Node ((insert a value), b, c, d); size= arbre.size}
            else 
              (*cadran en bas à gauche*)
              {tree = Node (a, b, (insert c value), d); size = arbre.size}
            else 
            if y < h/2 then 
              (*cadran en haut à droite*)
              {tree = Node (a, (insert b value), c, d); size = arbre.size}
          else 
              (*cadran en bas à droite*)
              {tree = Node (a, b, c, (insert d value)); size = arbre.size}
        | _ -> failwith "insert_intermediaire : arbre incompatible"
  in


  match arbre.tree with 
    | Empty -> {tree=Leaf value;size=arbre.size}
    | Leaf v -> if v = value then {tree=Leaf value;size=arbre.size}  else 
      (*sinon il faut créer un nouveau noeud*)
      let (width, height) = arbre.size in
      let arbre_inter = create2 width height in
      insert_intermediaire (insert_intermediaire arbre_inter {position = v.position; value = v.value}) {position = value.position; value = value.value}

    | Node (_,_,_,_) -> 
      (*maintenant faut choisir dans quel cadran on répartis la 
      value selon sa position :*)
      insert_intermediaire arbre value


(* ce remove est assez rudimentaire, faudrais le rendre plus opti (supprimer les branches mortes..*)
let rec remove arbre pos = 
  match arbre.tree with 
    | Empty -> {tree=Empty;size=arbre.size}
    | Leaf v -> if v.position = pos then {tree=Empty;size=arbre.size} else {tree=Leaf v;size=arbre.size}
    | Node (a,b,c,d) -> 
      let (x,y) = pos in
      let (w,h) = arbre.size in
      if x < w/2 then 
        if y < h/2 then 
          {tree=Node ((remove a pos), b, c, d);size=arbre.size}
        else 
          {tree=Node (a, b, (remove c pos), d); size=arbre.size}
        else 
        if y < h/2 then 
          {tree=Node (a, (remove b pos), c, d); size=arbre.size}
        else 
          {tree=Node (a, b, c, (remove d pos)); size=arbre.size}

let rec isOccupied arbre pos = 
  match arbre.tree with 
    | Empty -> None
    | Leaf v -> if (v.position == pos) then Some v.value else None
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
  let (x,y) = ((fst pos) + (fst vit)*dt, (snd pos) + (snd vit)*dt) in
  match isOccupied arbre (x,y) with 
    | None -> (x,y)
    | Some v -> failwith "TODO"


