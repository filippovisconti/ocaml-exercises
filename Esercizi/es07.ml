type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione =  Gira | Avanti of int
(* gira : direzione -> direzione *)
let gira = function
    Su -> Destra
  | Giu -> Sinistra
  | Destra -> Giu
  | Sinistra -> Su
(* avanti : posizione -> int -> posizione *)
let avanti (x,y,dir) n =
  match dir with
    Su -> (x,y+n,dir)
  | Giu -> (x,y-n,dir)
  | Destra -> (x+n,y,dir)
  | Sinistra -> (x-n,y,dir)
(* sposta : posizione -> azione -> posizione *)
let sposta (x,y,dir) act =
  match act with
    Gira -> (x,y,gira dir)
  (* le coordinate non cambiano,
     la direzione gira di 90 gradi in senso orario *)
  | Avanti n -> avanti (x,y,dir) n

let rec esegui posizione = function
    [] -> posizione
  | x::rest -> esegui (sposta posizione x) rest


type nat = Zero | Succ of nat

let rec int_of_nat = function
    Zero -> 0
  | Succ n -> succ(int_of_nat n)

(* somma : nat -> nat -> nat *)
let rec somma n m =
  match n with
    Zero -> m
  | Succ k -> Succ(somma k m)

let rec prodotto n m = match n with
    Zero -> Zero
  | Succ k -> somma (somma Zero m) (prodotto k m)


type chiave = Aperta | Chiusa
type cassaforte = chiave list

(* giraPrima: cassaforte -> cassaforte, che riporta la configurazione che si ottiene girando la prima chiave *)
let giraPrima = function
    [] -> []
  | Aperta::rest -> Chiusa::rest
  | Chiusa::rest -> Aperta::rest

(* giraDopoChiusa: cassaforte -> cassaforte, che riporta la configurazione che si ottiene girando la chiave che segue la prima chiusa (e solleva un’eccezione se non è possibile eseguire l’operazione). *)

let rec giraDopoChiusa = function
    [] -> failwith "f$ck"
  | Chiusa::Chiusa::rest -> Chiusa::Aperta::rest
  | Chiusa::Aperta::rest -> Chiusa::Chiusa::rest
  | Aperta::rest -> Aperta::giraDopoChiusa rest
  | _ -> failwith "f$ck"

let successori clist = (giraPrima clist) @ (try giraDopoChiusa clist with _ -> [])

type obj = Miss | Cann | Barca
type situazione = obj list * obj list
type azioneBarca =
    From_left of obj list
  | From_right of obj list

(* safe: situazione -> bool, che determina se una situazione e’ sicura (nessun missionario viene mangiato) *)

let count riva = 
  let rec aux (x,y) = function
      [] -> (x,y)
    | Miss::rest -> aux (x+1,y) rest
    | Cann::rest -> aux (x,y+1) rest
    | el::rest -> aux (x,y) rest
  in aux (0,0) riva 

let conteggio situazione = match situazione with
    ([],[]) -> ((0,0),(0,0))
  | (left, []) -> (count left,(0,0))
  | ([], right) -> ((0,0),count right)
  | (left, right) -> (count left, count right)

let safe situazione = let ((m1,c1), (m2,c2)) = conteggio situazione in m1>=c1 && m2>=c2


let applica act (left, right) = match act with
    From_left lst -> if List.mem Barca left 
    then 
    else failwith "no barca"
  | From_right lst -> 
    if List.mem Barca right 
    then
    else failwith "no barca"



  (*
    int_of_nat (prodotto (Succ (Succ (Succ Zero))) (Succ (Succ Zero)));; == 6
    int_of_nat (prodotto (Succ (Succ (Succ (Succ Zero)))) (Succ (Succ Zero)));; == 8
    int_of_nat (prodotto (Succ (Succ (Succ (Succ Zero)))) Zero);; == 0
  #use "es07.ml";;
  eval $(opam env)
*)