(*#load "matrices.cmo" ;;*)
(*#load "grappe.cmo" ;;*)
(*#load "assignement.cmo" ;;*)

open Matrices ;;
open Grappe ;;
open Assignement ;;

type corres =
  {
    a : int ;
    b : int ;
    iso : isometrie ;
    iab : int ; (*Déplacement après rotation*)
    jab : int ;
    distance : int
  } ;;

let corres_iso grappe_a grappe_b iso piece_a piece_b =
  let rec floor x =
    if x < 0. then -(floor (-.x)) - 1
    else int_of_float x
  in
  let ceil x = 1 + floor x in
  let distance = ref 0 in
  let t = transformee piece_a.(grappe_a).m iso in
  let (cia, cja) = centre t in
  let (cib, cjb) = piece_b.(grappe_b).centre in
  let iab = [| floor (cib -. cia) ; ceil (cib -. cia) |] in
  let jab = [| floor (cjb -. cja) ; ceil (cjb -. cja) |] in
  let pile = ref [] in
  let na = Array.length t in
  let pa = if na = 0 then 0 else Array.length t.(0) in
  let nb = Array.length piece_b.(grappe_b).m in
  let pb = if nb = 0 then 0 else Array.length piece_b.(grappe_b).m.(0) in
  for k=0 to -1 + Array.length iab do (*Pour chaque déplacement vertical...*)
    for l=0 to -1 + Array.length jab do (*Pour chaque déplacement horizontal...*)
      for i=0 to na - 1 do
	for j=0 to pa - 1 do (*Pour chaque point de a transformé...*)
	  if t.(i).(j) (*il y a un point sur la grappe a*)
	    && (i + iab.(k) < 0 || i + iab.(k) >= nb || j + jab.(l) < 0 || j + jab.(l) >= pb || not piece_b.(grappe_b).m.(i + iab.(k)).(j + jab.(l))) then
	      (* Il n'y a pas de point correspondant sur la grappe b *)
	    incr distance
	done ;
      done ;
      for i = -iab.(k) to nb - 1 - iab.(k) do
	for j = -jab.(k) to pb - 1 - jab.(k) do (*pour chaque point de b... *)
	  if piece_b.(grappe_b).m.(i+iab.(k)).(j+jab.(k)) (*s'il y a un point sur la grappe b*)
	    && (i < 0 || i >= na || j < 0 || j >= pa || not t.(i).(j)) then
	    (*et qu'il n'y a pas de point correspondant sur a*)
	    incr distance
	done ;
      done ;
      pile := ({a = grappe_a ; b = grappe_b ; iso = iso ; iab = iab.(k) ; jab = jab.(k) ; distance = !distance})::(!pile)
    done ;
  done ;
  List.fold_left (fun acc corr -> if corr.distance <= acc.distance then corr else acc) (List.hd !pile) !pile
;;
  
let corres grappe_a grappe_b piece_a piece_b =
  let resultats = Array.map (fun iso -> corres_iso grappe_a grappe_b iso piece_a piece_b) isometries_utilisables in
  Array.fold_left (fun acc corr -> if corr.distance <= acc.distance then corr else acc) resultats.(0) resultats
;;

let init_matrix n p f = Array.init n (fun i -> Array.init p (f i)) ;;

let compute_match piece_a piece_b =
  init_matrix (Array.length piece_a) (Array.length piece_b) (fun i j -> corres i j piece_a piece_b)
;;

let bijection matrice_corres =
  let repartition = assigner (Array.map (Array.map (fun c -> c.distance)) matrice_corres) in
  Array.mapi (fun i j -> matrice_corres.(i).(j)) repartition
;;

let decrire_correspondance flux c piece_a piece_b =
  output_string flux "Correspondance entre \n" ;
  dessin_grappe flux piece_a.(c.a) ;
  output_string flux "Via : \n" ;
  dessin_matrice flux c.iso (string_of_int) ;
  output_string flux "Soit : \n" ;
  dessin_grappe flux (grappe_of_m (transformee piece_a.(c.a).m c.iso)) ;
  output_string flux "Avec : \n" ;
  dessin_grappe flux piece_b.(c.b) ;
  output_string flux ("En décalant la première de "^(string_of_int c.iab)^" lignes et "^(string_of_int c.jab)^" colonnes donne une distance de "^(string_of_int c.distance^".\n")) ;
  flush flux
;;

let decrire_match flux bij piece_a piece_b =
  Array.iter (fun c -> decrire_correspondance flux c piece_a piece_b) bij ;
  flush flux
;;

if false then
  begin
    let fichier = open_out "test_correspondances" in
    let a = ref [|grappe_of_m (creer_patate_connexe 4 8 20)|] in
    let b = ref [|grappe_of_m (creer_patate_connexe 7 5 20)|] in
    dessin_piece fichier !a ;
    dessin_piece fichier !b ;
    a := division_plus_grande_aire !a ;
    b := division_plus_grande_aire !b ;
    dessin_piece fichier !a ;
    dessin_piece fichier !b ;
    decrire_match fichier (bijection (compute_match !a !b)) !a !b ;
    close_out fichier ;
  end ;;
