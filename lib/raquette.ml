
module type Raquette =
sig
  type p (*type position*)
  type d (*type dimension*)
  type raquette = p*d (*type raquette*)
  val getposition : raquette -> p
  val getdimension : raquette -> d
  val setposition : raquette -> p -> raquette
  val setdimension : raquette -> d -> raquette
  val create_raquette : p -> d -> raquette
  val create_dim : float list -> d
  val create_pos : (float list * float list) -> p
  val getpos : raquette -> (float list *float list)
  val getdim : raquette -> float list
end

module Raquette2d : Raquette =
struct
  type p = (float*float)*(float*float)
  type d = (float*float)
  type raquette = p*d
  let getposition (p,_) = p
  let getdimension (_,d) = d
  let setposition (_,d) p = (p,d)
  let setdimension (p,_) d = (p,d)
  let create_raquette p d = (p,d)
  let create_dim l = match l with
    |[a;b] -> (a,b)
    |_ -> failwith "dimension non valide en 2D"
  let create_pos (pos,vit) = match pos,vit with
    |[a;b], [c;d] -> (a,b), (c,d)
    |_ -> failwith "position non valide en 2D"
  let getpos ((p,v),_) = [fst p; snd p], [fst v; snd v]
  let getdim (_,d) = [fst d; snd d]
end

let get_floats_pos : Raquette2d.raquette -> (float*float)*(float*float) = fun r ->
  let (pos, vit) = Raquette2d.getpos r in
  let x = List.hd (List.tl (pos)) in
  let y = List.hd (pos) in
  let vx = List.hd (List.tl (vit)) in
  let vy = List.hd (vit) in
  (x,y), (vx, vy)

let get_floats_dim : Raquette2d.raquette -> float*float = fun r ->
  let x = List.hd (Raquette2d.getdim r) in
  let y = List.hd (List.tl (Raquette2d.getdim r)) in
  (x,y)

let create_raquette_autom : float -> float -> float -> float -> Raquette2d.raquette =
  fun box_xmax box_xmin box_ymax box_ymin ->
    let dimx = 80. in
    let dimy = 20. in
    let posx =  (box_xmax +. box_xmin) /. 2. in
    let posy = 5. +. dimy/.2. in
    let pos = Raquette2d.create_pos ([posx;posy],[0.;0.]) in
    Printf.printf "La raquette aura pour position = (%f,%f)\n" posx posy;
    Printf.printf "La raquette aura pour dimension = (%f,%f)\n" dimx dimy;
    let dim = Raquette2d.create_dim [dimx;dimy] in
    Raquette2d.create_raquette pos dim

let%test "Création de brique" =
    let _ = create_raquette_autom 100. 0. 100. 0. in
    true
