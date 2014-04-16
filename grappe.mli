(**Ce fichier a un unique objectif : savoir découper une pièce en ajoutant une grappe. Attention : aucune grappe vide n'est créée, tant qu'il y a moins de découpages que de points dans la pièce. Autrement, une exception est levée. Cela garantit la terminaison globale du programme.**)

type grappe =
  {
    m : bool Matrices.matrice ;
    mutable aire : int ;
    mutable centre : float * float
  }
(**L'aire est facilement tenue à jour. Le centre non : il faut parcourir tous les points de la matrice, ceci est effectué à chaque découpage. Le centre n'est pas tenu à jour lorsqu'on gagne ou qu'on perd un point (les transformations sont supposées fixées). **)

type piece = grappe array

exception Grappe_vide
exception Grappe_point
(**Levée lorsqu'on essaye de diviser une grappe à 1 élément.**)
val grappe_of_m : bool Matrices.matrice -> grappe
val gagner : grappe -> int -> int -> unit
val perdre : grappe -> int -> int -> unit
val centrer : grappe -> unit
val calculer_aire : grappe -> unit
val division_plus_grande_aire : piece -> piece
(**Divise la plus grosse grappe en 2 et retourne la nouvelle pièce.**)
val dessin_grappe : out_channel -> grappe -> unit
(**Dessine la grappe dans le canal donné. Caractères utilisés : **)
(**    -> "x" : le point appartient à la grappe.**)
(**    -> " " : le point n'appartient pas à la grappe.**)
(**    -> "O" : le point appartient à la grappe et est voisin du centre.**)
(**    -> "o" : le point n'appartient pas à la grappe mais est voisin du centre.**)
val dessin_piece : out_channel -> piece -> unit
(**Dessine la pièce dans le canal donné. On ne dessine pas chaque grappe, mais on inscrit le numéro de la grappe concernée.**)
val creer_patate_connexe : int -> int -> int -> bool Matrices.matrice
(**                         n      p     aire **)
(**Crée une patate aléatoire connexe d'aire aire dans une matrice n*p**)
