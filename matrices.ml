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
type isometrie = int array array ;;
type 'objet matrice = 'objet array array ;;
let isometries_utilisables : isometrie array =
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

let dessin_matrice (flux : out_channel) (m : 'objet matrice) (f : 'objet -> string) : unit =
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
  output_string flux "\n" ;
  flush flux
;;

let apres_transformation (m : 'objet matrice) (iso : isometrie) (f : int -> int -> int -> int -> unit) : unit =
  (*applique f à tous les couples (point initial, point transformé) de l'espace [|0 ; taille de m - 1|]x[|0 ; taille des lignes - 1|]*)
  let n = Array.length m in
  let p = if n = 0 then 0 else (Array.length m.(0)) in
  let u_i = iso.(0).(0) in
  let u_j = iso.(1).(0) in
  let v_i = iso.(0).(1) in
  let v_j = iso.(1).(1) in
  (*(u, v) est une base orthonormale de R², telle que u = u_i * i + u_j * j*)
  let n_ = ((abs u_i) * n) + ((abs v_i) * p) in
  (*Nouveau nombre de ligne, i.e. la composante sur i du plus gros vecteur transformé*)
  let p_ = ((abs u_j) * n) + ((abs v_j) * p) in
  (*On s'interdit les coordonnées négatives : *)
  let marge_i = if u_i < 0 || v_i < 0 then n_ - 1 else 0 in
  let marge_j = if u_j < 0 || v_j < 0 then p_ - 1 else 0 in
  let i_ = ref 0 in
  let j_ = ref 0 in
  for i=0 to n-1 do
    i_ := marge_i + i * u_i ;
    j_ := marge_j + i * u_j ;
    for j=0 to p-1 do
      f i j !i_ !j_ ;
      i_ := !i_ + v_i ;
      j_ := !j_ + v_j ;
    done ;
  done 
;;

let taille_transformee (m : 'objet matrice) (iso : isometrie) : int*int =
  let n = Array.length m in
  let p = if n = 0 then 0 else (Array.length m.(0)) in
  let u_i = iso.(0).(0) in
  let u_j = iso.(1).(0) in
  let v_i = iso.(0).(1) in
  let v_j = iso.(1).(1) in
  let n_ = ((abs u_i) * n) + ((abs v_i) * p) in
  let p_ = ((abs u_j) * n) + ((abs v_j) * p) in
  (n_, p_)
;;

let transformee (m : 'objet matrice) (iso : isometrie) : 'objet matrice =
  let (n_, p_) = taille_transformee m iso in
  if n_ = 0 || p_ = 0 then [||]
  else
    let t = Array.init n_ (fun _ -> Array.make p_ m.(0).(0)) in
    apres_transformation m iso (fun i j i_ j_ -> t.(i_).(j_) <- m.(i).(j)) ;
    t
;;
  
(* Quelques exemples *)
if false then
  begin
    Random.self_init () ;
    let m = Array.init 4 (fun _ -> Array.init 6 (fun _ -> Random.int 100)) in
    dessin_matrice stdout m (string_of_int) ;
    for i=0 to -1 + Array.length isometries_utilisables do
      print_endline "\nTransformons la matrice m avec : " ;
      dessin_matrice stdout isometries_utilisables.(i) (string_of_int) ;
      print_newline () ;
      dessin_matrice stdout (transformee m isometries_utilisables.(i)) (string_of_int)
    done ;
  end ;;
