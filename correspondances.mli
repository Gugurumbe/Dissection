type corres =
  {
    a : int ;
    b : int ;
    iso : Matrices.isometrie ;
    iab : int ; (*Déplacement après rotation*)
    jab : int ;
    distance : int
  }
(**Une correspondance entre les grappes a de la piece a et b de la piece b, une fois qu'on a appliqué l'isométrie iso puis effectué les déplacements iab et jab, a une certaine erreur.**)

val corres : int -> int -> Grappe.piece -> Grappe.piece -> corres
(**Établit la meilleure correspondance entre les deux grappes a.(ia) et b.(ib), en testant toutes les isométries et en alignant les centres. Commes ceux-ci n'ont pas beaucoup de chances d'être entiers, on aligne le centre de l'une avec les quatre points les plus proches du centre de l'autre.**)

val compute_match : Grappe.piece -> Grappe.piece -> corres array array
(**Établit toutes les correspondances possibles entre les éléments de la première pièce et les éléments de la deuxième.**)

val bijection : corres array array -> corres array
(**Résout le problème d'assignement.**)

val decrire_correspondance : out_channel -> corres -> Grappe.piece -> Grappe.piece -> unit
(**Décrit la correspondance dans le canal spécifié.**)

val decrire_match : out_channel -> corres array -> Grappe.piece -> Grappe.piece -> unit
(**Décrit toutes les correspondances retenues.**)
