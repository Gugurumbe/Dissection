(**Ce fichier a deux objectifs : **)
(** -> lister les isométries utilisables **)
(** -> transformer une matrice selon une isométrie **)
(**Pour de plus amples détails, consulter matrices.ml.**)

type isometrie = int array array 
type 'objet matrice = 'objet array array
val isometries_utilisables : isometrie array 
(**En théorie, ce sont des réels. En pratique, on ne s'autorise que les quarts de tours et les rotations autour d'un des axes du repère.**)
val dessin_matrice : out_channel -> 'objet matrice -> ('objet -> string) -> unit
(**Dessine la matrice donnée en utilisant une certaine fonction pour obtenir le contenu. Appelle flush sur le canal.**)
val apres_transformation : 'objet matrice -> isometrie -> (int -> int -> int -> int -> unit) -> unit
(**apres_transformation matrice isometrie (fun i j i_ j_ -> ()) appelle la fonction sur chaque couple (point initial, point transformé par l'isométrie) de la matrice. **)
val taille_transformee : 'objet matrice -> isometrie -> int * int
(**Retourne les dimensions de la matrice transformée**)
val transformee : 'objet matrice -> isometrie -> 'objet matrice
(**Retourne la matrice transformée par une certaine isométrie.**)
