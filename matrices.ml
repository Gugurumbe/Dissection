(*On s'intéresse à des matrices dans R² auxquelles on associe des isométries vectorielles. On représente les tableaux à deux dimensions de la façon suivante : *)
(* +-+-+-+-+-+-... *)
(* | | | | | | -> ligne 0 *)
(* +-+-+-+-+-+-... *)
(* | | | | | | -> ligne 1 *)
(* +-+-+-+-+-+-... *)
(* | | | | | | ... *)
(* . . . . . . .   *)
(* .|.|.|.|.|.  .  *)
(* .|.|.|.|.|.   . *)
(*  V V V V V      *)
(*  Colonnes 0, 1, 2, 3, 4, ... *)
(*On dispose d'une base orthonormale (i, j) : *)
(*           *)
(*  +--->  j *)
(*  |        *)
(*  V        *)
(*           *)
(*  i        *)
(*On tourne des pièces plates dans l'espace : *)
let isometries_utilisables =
  [|
    [|
      [|1;0|] ; (*Identité*)
      [|0;1|]
    |] ;
    [| 
      [|1; 0|] ; (*Symétrie par rapport à l'axe vertical*)
      [|0;-1|] 
    |] ;
    [|
      [|-1;0|] ; (*Symétrie par rapport à l'axe horizontal*)
      [| 0;1|] 
    |] ;
    [|
      [|-1; 0|] ; (*Demi-tour*)
      [| 0;-1|] 
    |] ;
    [|
      [|0;1|] ; (*Symétrie par rapport à la première bissectrice : *)
      [|1;0|]   (* \ |     *)
    |] ;        (*  \|  j  *) 
    [|          (*---+==>--*) 
      [|0;-1|] ;(*   I\    *)
      [|1; 0|]  (* i V \   *)
    |] ;        (*   |  \  *)
    [|          (* Rotation d'angle +90° sur le dessin *)
      [| 0;1|] ;(* Rotation d'angle -90° *)
      [|-1;0|]
    |] ;
    [|
      [| 0;-1|] ; (*Symétrie par rapport à l'autre bissectrice. *)
      [|-1; 0|]
    |]
  |]
;;

let dessin_matrice flux m f =
  let n = Array.length m in
  let p = if n = 0 then 0 else Array.length m.(0) in
  let chaines = Array.map (Array.map f) m in
  let avec_entete = 
    Array.init (n+1) (fun i -> 
      if i = 0 then 
	Array.init (p+1) (fun j -> if j = 0 then "\\" else (string_of_int (j-1)))
      else
	Array.append [|string_of_int (i-1)|] chaines.(i-1)
    )
  in
  let largeur_max = Array.fold_left
    (
	Array.fold_left 
	  (
	    fun acc_ s -> max acc_ (String.length s)
	  )
    )
    0
    avec_entete
  in
  let agrandir_chaine s =
    let espaces_manquants = largeur_max - (String.length s) in
    let droite = espaces_manquants / 2 in
    let gauche = espaces_manquants - droite in
    (String.make gauche ' ')^s^(String.make droite ' ')
  in
  let normalises = Array.map (Array.map (agrandir_chaine)) avec_entete in
  let dessin_separation () = 
    output_string flux "+" ;
    for i = 0 to p (* + 1 - 1 *) do
      for k=0 to largeur_max - 1 do
	output_string flux "-" ;
      done ;
      output_string flux "+" ;
    done 
  in
  dessin_separation () ;
  print_newline () ;
  let dessin_ligne l =
    output_string flux "|" ;
    Array.iter (fun s -> output_string flux s ; output_string flux "|") l ;
    output_string flux "\n" ;
    dessin_separation () ;
    output_string flux "\n" ;
  in
  Array.iter (dessin_ligne) normalises ;
  flush flux
;;
  
Random.self_init () ;;

let m = Array.init 8 (fun _ -> Array.init 6 (fun _ -> Random.int 100)) ;;

dessin_matrice stdout m (string_of_int) ;;
