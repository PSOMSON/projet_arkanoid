type vector2 = int*int (*seras implémenté plus en détail plus tard *)


type 'a tree = 
    | Empty
    | Leaf of 'a
    | Node of 'a tree * 'a tree * 'a tree * 'a tree

type 'a qtree = {tree : 'a tree; size : int*int}

let empty arbre = 
  arbre.tree == Empty

let create width height  = 
  {tree = Empty; size = (width, height)}

let rec insert arbre value = failwith "TODO"

let rec remove arbre value = failwith "TODO"

let rec isOccupied arbre pos = failwith "TODO"

let rec colide arbre pos vit = failwith "TODO"

