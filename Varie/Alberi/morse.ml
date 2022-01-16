type morse_tree = Leaf of char
                | Node of char * morse_tree * morse_tree

type segnale = Linea | Punto | Pausa | Errore
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
let decode tree segnale = 
  let rec aux t lst = match (t,lst) with
      (Leaf a, []) | (Node (a,_,_),[]) -> a
    | (Node (_,left,_), Punto::rest) -> aux left rest
    | (Node (_,_,right), Linea::rest) -> aux right rest
    | _ -> '*' 

  in
  if segnale = [Pausa] then ' '
  else if segnale = [Errore] then '*'
  else aux tree segnale

let rec path_to x = function
    Empty -> raise Not_found
  | Tr(y, Empty, Empty) -> 
    if y = x then [x]
    else raise Not_found
  | Tr(y, t1, t2) -> y:: ( try path_to x t1
                           with Not_found -> path_to x t2)

let rec path p = function
    Empty -> raise Not_found
  | Tr(y, Empty, Empty) -> 
    if not (p y) then [y]
    else raise Not_found
  | Tr(y, t1, t2) -> if p y then raise Not_found
    else y:: ( try path p t1
               with Not_found -> path p t2)

let rec path_coprente lista = function
    Empty -> raise Not_found
  | Tr(y, Empty, Empty) -> 
    if lista = [] then [y]
    else raise Not_found
  | Tr(y, t1, t2) -> let nuovalista = List.filter ((<>) y) lista in   
    y:: ( try path_coprente nuovalista t1
          with Not_found -> path_coprente nuovalista t2)