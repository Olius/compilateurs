Le travail a été effectué en utilisant le language de programmation Haskell. 

Pour compiler le programme, il faut avoir le compilateur GHC qui est trouvable à l'adresse : https://www.haskell.org/downloads/ 

La compilation est ensuite effectuée en executant la commande : 

`ghc arbreDeDerivation.hs`


---------------

L'arbre de dérivation est construit implicitement par l'appel aux différentes fonctions `func?` (qui correspondent à un noeud non-terminal). 

Ces fonctions retournent dès qu'elles arrivent à un noeud terminal. La valeur retournée est la chaine de charactères qu'il reste à parser. 

