type 'a ntree = Tr of 'a * 'a ntree list

type path_formula = AE of string | EA of string

(*
AE(p) verificata in t se ogni ramo di t contiene almeno un nodo etichettato da un insieme che contiene p
EA(p) verificata in t se almeno un ramo di t contiene tutti un nodo etichettato da un insieme che contiene p
 *)