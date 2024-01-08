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
    | Leaf v -> if (equals_with_resol (v.position) pos arbre.resol) then Some v else None
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

let colide : 'a qtree -> float*float -> float*float -> int -> 'a qtree * (float*float)* 'a feuille option =
  fun arbre pos vit taille_balle ->

  let (x,y) = (int_of_float (fst pos), int_of_float (snd pos)) in
  let (vx, vy) = vit in 
  

  let colid_inter arbre x y = 
    let pos = (int_of_float x , int_of_float y) in
    match isOccupied arbre pos with 
      | None -> (arbre, false, None)
      | Some v -> let new_arbre = (remove arbre v.position) in (new_arbre, true, Some v)          
        
  in 
    (*soit la balle touche une balle à sa droite, à sa gauche, en haut ou en bas, ou en diagonal, dans un quel
       cas,c'est dans la longueure de la vitesse*)

    let (arbre_inter, did_colide,brique) = colid_inter arbre (float_of_int (x+taille_balle)) (float_of_int y)  in
    if did_colide then (arbre_inter, (-. vx,vy), brique) else
      
    let (arbre_inter, did_colide, brique) = colid_inter arbre (float_of_int (x-taille_balle)) (float_of_int y) in 
    if did_colide then (arbre_inter, (-. vx,vy), brique) else 

    let (arbre_inter, did_colide, brique) = colid_inter arbre (float_of_int x) (float_of_int (y+taille_balle)) in 
    if did_colide then (arbre_inter, (vx, -. vy), brique) else 

    let (arbre_inter, did_colide, brique) = colid_inter arbre (float_of_int x) (float_of_int (y-taille_balle)) in 
    if did_colide then (arbre_inter, (vx, -. vy), brique) else 

    (*on veux calculer la position de l'intersection entre le vecteur vitesse et le bout de la balle*)
    (*d'abord on normalise le vecteur vitesse*)
    let norme = sqrt ((vx *. vx) +. (vy *. vy)) in
    let (vx', vy') = ((vx /. norme)*.(float_of_int taille_balle)  , (vy /. norme)*.(float_of_int taille_balle) ) in
    (*on calcule la position de l'intersection*)
    let (x', y') = ((float_of_int x) +. vx', (float_of_int y) +. vy') in
    (*on regarde si il y a une brique à cette position*)

    let (arbre_inter, did_colide, brique) = colid_inter arbre x' y' in 
    (*dans ce cas là, on a rencontré un coin*)
    if did_colide then (arbre_inter, (-. vx, -. vy), brique) else (arbre, (vx,vy), None)


