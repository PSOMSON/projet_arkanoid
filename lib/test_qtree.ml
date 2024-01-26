open Quadtree;;

(* let nulElement = "0";;
let arbre = createAndInitialize 8. 8. (1.,1.) nulElement ;;
print arbre;;

(*test d'un ajout*)
let arbre = insertOnInitializedTree arbre (2.,2.) "1";;
print arbre;;

(*test d'un ajout*)
let arbre = insertOnInitializedTree arbre (3.,3.) "2";;
print arbre;;

(*test de suppression dans le cas où on supprime pile au bon endroit *)
let string_to_print =
  match isOccupied arbre (2.,2.) with
    | None -> "None"
    | Some _ -> "(2,2) : occupé";;

print_string string_to_print;;
let arbre_removed = remove arbre (2.,2.) "0";;
print arbre_removed;;

(*test de suppression dans le cas où on supprime au dessus à droite *)
let string_to_print =
  match isOccupied arbre (2.5,2.5) with
    | None -> "None"
    | Some _ -> "(2.5,2.5) : occupé";;

print_string string_to_print;;
let arbre_removed = remove arbre (2.5,2.5) "0";;
print arbre_removed;;

(*test de suppression dans le cas où on supprime au dessous à gauche *)
let string_to_print =
  match isOccupied arbre (1.5,1.5) with
    | None -> "None"
    | Some _ -> "(1.5,1.5) : occupé";;

print_string string_to_print;;
let arbre_removed = remove arbre (1.5,1.5) "0";;
print arbre_removed;;
*)
