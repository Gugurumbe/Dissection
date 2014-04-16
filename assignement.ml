(*On utilise The Mating Algorithm, dont le but est de respecter les priorités, au sens où aucun "rogue couple" n'est formé. Attention : le résultat n'est pas le même pour la matrice de poids et sa transposée !*)

let priorites ligne =
  let liste_poids = Array.to_list ligne in
  let liste_indices = Array.to_list (Array.init (Array.length ligne) (fun i -> i)) in
  let liste_melee = List.combine liste_poids liste_indices in
  snd (List.split (List.sort (fun (e1, _) (e2, _) -> Pervasives.compare e1 e2) liste_melee))
;;

let inscrire src dest table poids prio =
  print_endline ((string_of_int src)^" va s'inscrire chez "^(string_of_int dest)^"...") ;
  match table.(dest) with
  | None -> 
    begin
      table.(dest) <- Some(src) ;
      print_endline ("\t-> J'accepte.\n") ;
      (*Tu peux réessayer au prochain rang.*)
      false (*Il ne s'est rien passé de marquant.*)
    end
  | Some(autre) when poids.(autre).(dest) > poids.(src).(dest) ->
    begin
      print_endline ("\t-> J'accepte, et je vire "^(string_of_int autre)^".") ;
      table.(dest) <- Some(src) ;
      prio.(autre) <- List.tl (prio.(autre)) ;
      (*Ne réessaye pas au prochain rang.*)
      true (*Il s'est passé quelque chose*)
    end
  | Some(autre) ->
    begin
      (*src est un moins bon parti que l'autre.*)
      print_endline ("\t-> Non merci, "^(string_of_int autre)^" me plaît plus.") ;
      prio.(src) <- List.tl (prio.(src)) ;
      true
    end
;;

let assigner poids =
  let n = Array.length poids in
  let prio = Array.map (priorites) poids in
  let favoris = Array.make n None in
  let fini = ref false in
  while not !fini do
    fini := true ;
    print_endline "Nouveau jour.\n" ;
    for i=0 to n-1 do
      if (inscrire i (List.hd (prio.(i))) favoris poids prio) then fini := false
    done ;
    for i=0 to n-1 do
      favoris.(i) <- None
    done ;
  done ;
  Array.map (List.hd) prio
;;

#load "matrices.cmo" ;;

let poids_ex = Array.init 10 (fun _ -> Array.init 10 (fun _ -> Random.int 100)) ;;
Matrices.dessin_matrice stdout poids_ex (string_of_int) ;;
assigner poids_ex ;;
