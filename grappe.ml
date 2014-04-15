(*On interprète, pour l'instant*)
#load "matrices.cmo" ;;

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

let decoupage_auto matrice_a = j'en suis là
