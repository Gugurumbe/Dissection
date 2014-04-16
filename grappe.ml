(*On interprète, pour l'instant*)
(*#load "matrices.cmo" ;;*)

open Matrices ;;

type grappe =
  {
    m : bool matrice ;
    mutable aire : int ;
    mutable centre : (float * float)
  } ;;

type piece = grappe array ;;

exception Grappe_vide ;;

let centre m =
  let si = ref 0 in
  let sj = ref 0 in
  let n = ref 0 in
  for i=0 to -1 + Array.length m do
    for j=0 to -1 + Array.length m.(i) do
      if m.(i).(j) then
	begin
	  si := !si + i ;
	  sj := !sj + j ;
	  incr n
	end
    done
  done ;
  if !n = 0 then raise Grappe_vide 
  else
    ((float_of_int !si) /. (float_of_int !n), (float_of_int !sj) /. (float_of_int !n))
;;

let aire m =
  Array.fold_left (Array.fold_left (fun acc b -> if b then acc + 1 else acc)) 0 m
;;

let decouper matrice_a ia ib ja jb =
  let carre a = a * a in
  Array.init (Array.length matrice_a) (fun i ->
    Array.init (Array.length matrice_a.(i)) (fun j ->
      if matrice_a.(i).(j) then
	begin
	  let da = carre (i-ia) + carre (j-ja) in
	  let db = carre (i-ib) + carre (j-jb) in
	  if da > db then 
	    begin
	      matrice_a.(i).(j) <- false ;
	      true
	    end
	  else false
	end
      else false
    ))
;;

exception Grappe_point ;;

let decoupage_auto matrice_a = 
  let n = Array.length matrice_a in
  let p = if n = 0 then 0 else Array.length matrice_a.(0) in
  let aire = aire matrice_a in
  if aire <= 1 then raise Grappe_point 
  else
    begin
      let roulette () =
	let r1 = Random.int aire in
	let r2 = ref (Random.int aire) in
	if r1 = !r2 then
	  (r1, (!r2 + 1) mod aire)
	else
	  (r1, !r2)
      in
      let ith numero =
	let pos = ref 0 in
	let resultat = ref (0, 0) in
	let i = ref 0 in
	let j = ref 0 in
	while !i < n do
	  j := 0 ;
	  while !j < p do
	    begin
	      if matrice_a.(!i).(!j) && !pos = numero then
		begin
		  resultat := (!i, !j) ;
		  i := n ;
		  j := p ;
		end 
	      else if matrice_a.(!i).(!j) then incr pos
	      else ()
	    end ;
	    incr j
	  done ;
	  incr i
	done ;
	!resultat
      in
      let (r1, r2) = roulette () in
      let (i1, j1) = ith r1 in
      let (i2, j2) = ith r2 in
      decouper matrice_a i1 i2 j1 j2
    end
;;

let grappe_of_m matrice =
  {
    m = matrice ;
    aire = aire matrice ;
    centre = centre matrice
  }
;;

let diviser a =
  let b = grappe_of_m (decoupage_auto a.m) in
  a.centre <- centre a.m ;
  a.aire <- aire a.m ;
  b
;;

let gagner a i j =
  if not (a.m.(i).(j)) then a.aire <- a.aire + 1 ;
  a.m.(i).(j) <- true
;;

let perdre a i j =
  if a.m.(i).(j) then a.aire <- a.aire - 1 ;
  a.m.(i).(j) <- false
;;
  
let centrer a = a.centre <- centre a.m ;;
let calculer_aire a = a.aire <- aire a.m ;;
let division_piece p i =
  Array.init (1 + Array.length p) (fun k -> if k=0 then diviser p.(i) else p.(k-1))
;;

let division_plus_grande_aire p =
  let i_max = ref 0 in
  for i=1 to -1 + Array.length p do
    if p.(i).aire > p.(!i_max).aire then i_max := i
  done ;
  division_piece p !i_max
;;

let dessin_grappe flux grappe =
  let (ci, cj) = grappe.centre in
  let carte = Array.map (Array.map (fun b -> if b then "x" else " ")) grappe.m in
  let set_centre i j =
    if i >= 0 && i < Array.length carte && j >= 0 && j < Array.length carte then
      carte.(i).(j) <- if grappe.m.(i).(j) then "O" else "o"
  in
  let ci = int_of_float ci in
  let cj = int_of_float cj in
  set_centre ci cj ;
  set_centre (ci + 1) cj ;
  set_centre ci (cj + 1) ;
  set_centre (ci + 1) (cj + 1) ;
  dessin_matrice flux carte (fun s -> s)
;;

let dessin_piece flux piece =
  let n = Array.length piece.(0).m in
  let p = Array.length piece.(0).m.(0) in
  let table = Array.make_matrix n p (-1) in
  for k=0 to -1 + Array.length piece do
    for i=0 to -1 + Array.length piece.(k).m do
      for j=0 to -1 + Array.length piece.(k).m.(i) do
	if piece.(k).m.(i).(j) then table.(i).(j) <- k
      done 
    done
  done ;
  dessin_matrice flux table (fun i -> if i >= 0 then string_of_int i else "")
;;


let creer_patate_connexe n p aire =
  let push queue objet = Queue.push objet queue in
  let matrice = Array.make_matrix n p false in
  let i_graine = Random.int n in
  let j_graine = Random.int p in
  let prio = Queue.create () in
  let alternatif = Queue.create () in
  let voisins (i, j) =
    let possibilites = [(i+1, j) ; (i, j+1) ; (i-1, j) ; (i, j-1)] in
    let valide (i, j) =
      i >= 0 && i < n && j >= 0 && j < p && not (matrice.(i).(j))
    in
    List.filter (valide) possibilites
  in
  let occupe = ref 0 in
  push prio (i_graine, j_graine) ;
  let inscrire (i, j) =
    if matrice.(i).(j) then ()
    else
      begin
	matrice.(i).(j) <- true ;
	incr occupe ;
	List.iter (push (if Random.bool () then prio else alternatif)) (voisins (i, j))
      end
  in
  while !occupe < n * p && !occupe < aire do
    inscrire (Queue.take (if Queue.is_empty prio then alternatif else prio)) ;
  done ;
  matrice
;;

let fichier_decoupe = open_out "decoupage" ;;
let piece = ref [|grappe_of_m (creer_patate_connexe 6 6 25)|] ;;
dessin_piece fichier_decoupe !piece ;;
for i=1 to 24 do
  output_string fichier_decoupe "Dessin de la première grappe : \n" ;
  dessin_grappe fichier_decoupe (!piece).(0) ;
  output_string fichier_decoupe "Dessin de la pièce : \n" ;
  piece := division_plus_grande_aire !piece ;
  dessin_piece fichier_decoupe !piece ;
done ;;
close_out fichier_decoupe ;;
