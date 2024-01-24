type vector2 = float*float (*seras implémenté plus en détail plus tard *)

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
and 'a qtree = {tree : 'a tree; size : float*float; resol : float*float}

let empty arbre = 
  arbre.tree == Empty

let create width height res = 
  {tree = Empty; size = (width, height); resol = res}

let  createAndInitialize : float -> float -> float*float -> 'a -> 'a qtree
  = fun width height (r1,r2) emptyValue ->

    let rec createAndInitializeInter : float -> float -> float*float -> 'a ->float*float -> 'a qtree
      = fun width height (r1,r2) emptyValue currentPos->
      if (width <= r1) || (height<= r2) then 
        let (nw, nh) = (width /. 2., height /. 2.) in
        let (x,y) = currentPos in 
        let emptyLeave = {position= (x+.nw, y+.nh); value = emptyValue} in 
          {tree = Leaf(emptyLeave); size = (width, height); resol = (r1,r2)}
      else 
        let (nw, nh) = (width /. 2., height /. 2.) in
        let (x,y) = currentPos in 
          {tree = Node((createAndInitializeInter nw nh (r1,r2) emptyValue (x,y)),
                      (createAndInitializeInter nw nh (r1,r2) emptyValue (nw +.x, y)), 
                      (createAndInitializeInter nw nh (r1,r2) emptyValue (x, nh +. y)), 
                      (createAndInitializeInter nw nh (r1,r2) emptyValue (nw +. x, nh +. y)))
          ; size = (width, height)
          ; resol = (r1,r2)}
      in 
      createAndInitializeInter width height (r1,r2) emptyValue (0., 0.)
    

let create2 width height res = 
  let arbre_inter = create (width/.2.) (height/.2.) res in
  {tree = Node(arbre_inter, arbre_inter, arbre_inter, arbre_inter); size = (width, height); resol = res}


  (*permet de savoir si 2 positions seront égales dans la qtree *)
let equals_with_resol posa posb resol = 
  let (xa,ya) = ( int_of_float (fst posa), int_of_float (snd posa) ) in
  let (xb,yb) = ( int_of_float (fst posb), int_of_float (snd posb) ) in
  let (w,h) = (int_of_float (fst resol), int_of_float (snd resol) ) in
  (xa/w) == (xb/w) && (ya/h) == (yb/h)

  
let rec insertOnInitializedTree : 'a qtree -> 'a feuille -> 'a qtree = 
  fun arbre value -> 
  
    let insert_intermediaire : 'a qtree -> 'a feuille -> 'b qtree =
    fun arbre value ->
      match arbre.tree with
        | Node (a,b,c,d) ->
          let (x,y) = (int_of_float (fst value.position), int_of_float (snd value.position)) in
          let (w,h) = (int_of_float (fst arbre.size), int_of_float (snd arbre.size)) in
          if x < w/2 && y < h/2 then 
              (*cadran en haut à gauche*)
              {tree = Node ((insertOnInitializedTree a value), b, c, d); size= arbre.size; resol = arbre.resol}
          else if x < w/2 && y >= h/2 then
              (*cadran en bas à gauche*)
              {tree = Node (a, b, (insertOnInitializedTree c value), d); size = arbre.size; resol = arbre.resol}
          else if x >= w/2 && y < h/2 then     
              (*cadran en haut à droite*)
            {tree = Node (a, (insertOnInitializedTree b value), c, d); size = arbre.size; resol = arbre.resol}
          else if x >= w/2 && y >= h/2 then
              (*cadran en bas à droite*)
            {tree = Node (a, b, c, (insertOnInitializedTree d value)); size = arbre.size; resol = arbre.resol}
          else 
            failwith "insert_intermediaire : arbre incompatible"
        | _ -> failwith "insert_intermediaire : arbre incompatible"
    in

  match arbre.tree with 
    | Node(_,_,_,_) -> (insert_intermediaire arbre value)
    | Leaf(l) -> {tree = Leaf ({position = l.position; value = value.value}); size = arbre.size; resol = arbre.resol}
    | Empty -> failwith "not initialized tree, error"


let rec insert : 'a qtree -> 'a feuille-> 'a qtree =
  fun arbre value ->

    
    let insert_intermediaire : 'a qtree -> 'a feuille -> 'b qtree =
    fun arbre value ->
      match arbre.tree with
        | Node (a,b,c,d) ->
          let (x,y) = (int_of_float (fst value.position), int_of_float (snd value.position)) in
          let (w,h) = (int_of_float (fst arbre.size), int_of_float (snd arbre.size)) in
          if x < w/2 && y < h/2 then 
              (*cadran en haut à gauche*)
              {tree = Node ((insert a value), b, c, d); size= arbre.size; resol = arbre.resol}
          else if x < w/2 && y >= h/2 then
              (*cadran en bas à gauche*)
              {tree = Node (a, b, (insert c value), d); size = arbre.size; resol = arbre.resol}
          else if x >= w/2 && y < h/2 then     
              (*cadran en haut à droite*)
            {tree = Node (a, (insert b value), c, d); size = arbre.size; resol = arbre.resol}
          else if x >= w/2 && y >= h/2 then
              (*cadran en bas à droite*)
            {tree = Node (a, b, c, (insert d value)); size = arbre.size; resol = arbre.resol}
          else 
            failwith "insert_intermediaire : arbre incompatible"
        | _ -> failwith "insert_intermediaire : arbre incompatible"
  in


  match arbre.tree with 
    | Empty -> {tree=Leaf value;size=arbre.size; resol = arbre.resol}
    | Leaf v -> if (equals_with_resol v.position value.position arbre.resol ) then {tree=Leaf value;size=arbre.size; resol = arbre.resol}  else 
      (*sinon il faut créer un nouveau noeud*)
      let (width,height) = (int_of_float (fst arbre.size), int_of_float (snd arbre.size)) in
      let (r1,r2) = (int_of_float (fst arbre.resol), int_of_float (snd arbre.resol)) in
        if width / 2 < r1 || height / 2 < r2 then 
          (*on est arrivé à la résolution minimale, on crée un nouveau noeud*)
          let arbre_inter = create2 (fst arbre.size) (snd arbre.size) arbre.resol in
          insert_intermediaire (insert_intermediaire arbre_inter v) value
        else 
          (*on peut encore descendre dans l'arbre*)
          let arbre_inter = create2 (fst arbre.size) (snd arbre.size) arbre.resol in
          insert_intermediaire (insert_intermediaire arbre_inter {position = v.position; value = v.value}) {position = value.position; value = value.value}

    | Node (_,_,_,_) -> 
      (*maintenant faut choisir dans quel cadran on répartis la 
      value selon sa position :*)
      insert_intermediaire arbre value


(* ce remove est assez rudimentaire, faudrais le rendre plus opti (supprimer les branches mortes..*)
let rec remove arbre pos nulElement = 
  match arbre.tree with 
    | Empty -> {tree=Empty;size=arbre.size; resol = arbre.resol}
    | Leaf v -> if (equals_with_resol (v.position) pos arbre.resol) then {tree=Leaf({position = v.position; value  = nulElement});size=arbre.size; resol = arbre.resol} else {tree=Leaf v;size=arbre.size; resol = arbre.resol}
    | Node (a,b,c,d) -> 
      let (x,y) = (int_of_float (fst pos), int_of_float (snd pos)) in
          let (w,h) = (int_of_float (fst arbre.size), int_of_float (snd arbre.size)) in
      if x < w/2 then 
        if y < h/2 then 
          {tree=Node ((remove a pos nulElement), b, c, d);size=arbre.size; resol = arbre.resol}
        else 
          {tree=Node (a, b, (remove c pos nulElement), d); size=arbre.size; resol = arbre.resol}
      else 
        if y < h/2 then 
          {tree=Node (a, (remove b pos nulElement), c, d); size=arbre.size; resol = arbre.resol}
        else 
          {tree=Node (a, b, c, (remove d pos nulElement)); size=arbre.size; resol = arbre.resol}

let rec isOccupied arbre pos = 
  match arbre.tree with 
    | Empty -> None
    | Leaf v -> if (equals_with_resol (v.position) pos arbre.resol) then Some v else None
    | Node (a,b,c,d) -> 
      let (x,y) = pos in
      let (w,h) = arbre.size in
      if x < w/.2. then 
        if y < h/.2. then 
          isOccupied a pos
        else 
          isOccupied c pos
        else 
        if y < h/.2. then 
          isOccupied b pos
        else 
          isOccupied d pos

let colide : 'a qtree -> float*float -> float*float -> float -> 'a qtree * (float*float)* 'a feuille option =
  fun arbre pos vit taille_balle ->

  let (x,y) = (fst pos, snd pos) in
  let (vx, vy) = vit in 
  

  let colid_inter arbre x y = 
    let pos = (x,y) in
    match isOccupied arbre pos with 
      | None -> (arbre, false, None)
      | Some v -> (arbre, true, Some v)          
        
  in 
    (*soit la balle touche une balle à sa droite, à sa gauche, en haut ou en bas, ou en diagonal, dans un quel
       cas,c'est dans la longueure de la vitesse*)

    let (arbre_inter, did_colide,brique) = colid_inter arbre (x+.taille_balle) y  in
    if did_colide then (arbre_inter, (-. vx,vy), brique) else
      
    let (arbre_inter, did_colide, brique) = colid_inter arbre (x-.taille_balle) y in 
    if did_colide then (arbre_inter, (-. vx,vy), brique) else 

    let (arbre_inter, did_colide, brique) = colid_inter arbre x (y+.taille_balle) in 
    if did_colide then (arbre_inter, (vx, -. vy), brique) else 

    let (arbre_inter, did_colide, brique) = colid_inter arbre x (y-.taille_balle) in 
    if did_colide then (arbre_inter, (vx, -. vy), brique) else 

    (*on veux calculer la position de l'intersection entre le vecteur vitesse et le bout de la balle*)
    (*d'abord on normalise le vecteur vitesse*)
    let norme = sqrt ((vx *. vx) +. (vy *. vy)) in
    let (vx', vy') = ((vx /. norme)*.taille_balle  , (vy /. norme)*.taille_balle ) in
    (*on calcule la position de l'intersection*)
    let (x', y') = (x +. vx', y +. vy') in
    (*on regarde si il y a une brique à cette position*)

    let (arbre_inter, did_colide, brique) = colid_inter arbre x' y' in 
    (*dans ce cas là, on a rencontré un coin*)
    if did_colide then (arbre_inter, (-. vx, -. vy), brique) else (arbre, (vx,vy), None)




let rec gen_tabulation i = 
  if i == 0 then "" else " "^(gen_tabulation (i-1))

let print arbre = 
  let rec print_inter arbre prof= 
    match arbre.tree with 
      | Empty -> print_string "Empty"
      | Leaf v -> print_string "Leaf at" ;print_string (string_of_float (fst v.position));print_string(" "); print_string (string_of_float (snd v.position))
      | Node (a,b,c,d) -> print_string "Node of size : "; print_string (string_of_float (fst arbre.size)); print_string (string_of_float (snd arbre.size)) ; 
        print_string "\n";
        print_string (gen_tabulation prof); print_inter a (prof+1);
        print_string "\n";
        print_string (gen_tabulation prof); print_inter b (prof+1);
        print_string "\n";
        print_string (gen_tabulation prof); print_inter c (prof+1);
        print_string "\n"; 
        print_string (gen_tabulation prof); print_inter d (prof+1);
        print_string "\n";
  in
  print_inter arbre 0





