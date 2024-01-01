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

let rec remove arbre value = failwith "TODO"

let rec isOccupied arbre pos = failwith "TODO"

let rec colide arbre pos vit = failwith "TODO"

