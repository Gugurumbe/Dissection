(**Le but ultime de ce fichier est de résoudre un problème d'assignement en minimisant des poids. Il utilise la méthode TMA qui fonctionne en O(n²) (O(n²*ln(n) en réalité : il faut établir les ordres de préférence.)**)

val assigner : int array array -> int array
(**Retourne un tableau t tel que t.(i) est l'indice de l'objet allant avec i.**)
(**Attention : l'algorithme ne donne pas la même solution avec la matrice transposée. Dans notre cas, la matrice sera symétrique (à des erreurs d'arrondi près).**)
