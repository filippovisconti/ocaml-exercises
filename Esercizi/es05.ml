exception Not_found
let rec sumof = function
    [] -> 0
  | x::rest -> x + sumof rest
let subset_search set n = 
  let rec aux solution altri = 
    let somma = sumof solution in 
    if somma > n then raise Not_found
    else if somma = n then solution
    else match altri with
        [] -> raise Not_found (* non si possono aggiungere altri elementi *)
      | x :: rest -> 
        try aux (x::solution) rest 
        with Not_found -> aux solution rest
  in aux [] set

let in_labirinto dim (r,c) = r>=0 && r < dim && c >= 0 &&c < dim

(* filter_vicini int -> (int * int) list -> (int * int) list *)
let rec filter_vicini dim = function
    [] -> []
  | casella::rest ->
    if in_labirinto dim casella
    then casella::filter_vicini dim rest
    else filter_vicini dim rest

(* sottoproblema: data una casella e la struttura di un labirinto, 
   trovare le caselle interne al labirinto ad essa accessibili.  
   Se la casella non e` nel 
   labirinto, sollevare un'eccezione *)
(* vicini : int -> int * int -> (int * int) list *)
(* vicini dim casella = lista delle caselle interne al labirinto
                        vicine a casella *)
let vicini dim (r,c) =
  if in_labirinto dim (r,c)
  then filter_vicini dim [(r,c+1);(r+1,c+1);(r-1,c+1)]
  else raise Not_found

(* Per usare la lista dei vicini, gia` "filtrata"
   eliminando le caselle che non sono nella matrice, dobbiamo
   generalizzare l'espressione:
   	    try cerca_da (r,c+1) 
   	    with NotFound ->
   	      try cerca_da (r+1,c+1) 
   	      with NotFound -> 
   		 cerca_da (r-1,c+1)
   in modo che la ricerca possa proseguire a partire da una
   qualsiasi delle caselle in una lista data

   cerca_da si applica a una casella, non puo` applicarsi a una
   lista di caselle.  Dovremmo definire una funzione analoga a
   cerca_da che pero` si applichi a liste di caselle e riportare
       casella::cerca_da_lista (vicini dim casella)

   cerca_da_lista deve richiamare cerca_da per ogni elemento della 
   lista.  Quindi:
       cerca_da       usa   cerca_da_lista
       cerca_da_lista usa   cerca_da
   cerca_da_lista lst = un cammino fino all'uscita a partire da una
                        qualsiasi casella in lst
*)


(* loop: elementi di lista interni al labirinto, rovesciata @ result *)
type casella = int * int
let filter_viciniIt dim list = 
  let rec loop lst res =
    match list with
      [] -> res
    | x::rest -> if in_labirinto dim x
      then loop rest (x::res)
      else loop rest res
  in List.rev (loop list []) 


let path2 dim mostri ingresso uscita = 
  let rec cerca_da casella = 
    if List.mem casella mostri 
    then raise Not_found
    else
    if casella = uscita
    then [casella]
    else casella::cerca_da_lista (vicini dim casella)
  and cerca_da_lista = function
      [] -> raise Not_found
    | c::rest ->
      try cerca_da c
      with Not_found -> cerca_da_lista rest
  in cerca_da ingresso
let rec filter p = function
    [] -> []
  | x::rest ->
    if p x then x::filter p rest
    else filter p rest

(* map f [x1; x2; x3] = [f x1; f x2; fx3] *)
let rec map f = function 
    [] -> []
  | x::rest -> (f x) :: (map f rest)


exception NotSameLength
let combine list1 list2 =
  if (List.length list1) != (List.length list2) then raise NotSameLength
  else let rec loop lst1 lst2 res = match lst1 with
        [] -> res
      | x::rest -> loop rest (List.tl lst2) ((x, (List.hd lst2))::res)
    in List.rev (loop list1 list2 [])

let split2 list = 
  let rec loop list res = match list with  
      [] ->res
    | (x,y)::rest -> loop rest ((fst res)@[x], (snd res)@[y])
  in loop list ([],[])

let rec cancella k = function
    [] -> []
  | (x,y)::rest ->
    if x = k then cancella k rest 
    else (x,y)::(cancella k rest)

let rec isPresent k = function
    [] -> false
  | x::rest -> x=k || isPresent k rest

let rec eliminaEl el list =
  if not (isPresent el list) then list
  else let rec loop el list res = match list with
        [] -> res
      |x::rest -> if x=el then loop el rest res else loop el rest (x::res)
    in loop el list []

let union set1 set2 =
  let rec loop list1 list2 res = match list1 with
      [] -> res@list2
    |x::rest -> loop rest (eliminaEl x list2) (x::res)
  in loop set1 set2 []

let intersect set1 set2 =
  let rec loop l1 l2 res = match l1 with
      [] -> List.rev res
    |x::rest -> if ((isPresent x l1) && (isPresent x l2)) then loop rest l2 (x::res)
      else loop rest l2 res
  in loop set1 set2 []

let setdiff set1 set2 =
  let rec loop l1 l2 res = match l1 with
      [] -> List.rev res
    |x::rest -> if (isPresent x l2) then loop rest l2 res
      else loop rest l2 (x::res)
  in loop set1 set2 []

let rec subset sub1 sub2 = match sub1 with
    [] -> true
  | x::rest -> (isPresent x sub2) && (subset rest sub2)

let explode string =
  let rec loop i res = 
    if i < (String.length string) 
    then loop (i+1) ((string.[i])::res) 
    else List.rev res
  in loop 0 []

let rec implode = function
    [] -> ""
  | x::rest -> (Char.escaped x) ^ implode rest

let rec makelist start finish =
  if start > finish then []
  else start::(makelist (start + 1) finish)

let rec affianca elemento = function
    [] -> []
  | x::rest -> (elemento, x)::(affianca elemento rest) 
let intpairs x =
  let rec loop i =
    if i > x then []
    else (affianca i (makelist 1 3))@(loop (i+1))
  in loop 1

let rec take n lista = match lista with
    [] -> []
  | x::rest -> 
    if n <= 0 then []
    else x :: take (n-1) rest;;

let rec trips lst = 
  if (List.length lst) < 3 then [] else
    (take 3 lst)::(trips (List.tl lst))

let rec choose k lst = 
  if (List.length lst) < k then [] else
    (take k lst)::(choose k (List.tl lst))


let rec strike test guess = match test with
    [] -> 0
  | x::rest -> if (isPresent x guess) then 1 + strike guess rest
    else strike guess rest

let getPosizione x list = 
  let rec loop i list = match list with
      [] -> -1
    | y::rest -> if x = y then i else loop (i+1) rest
  in loop 0 list


let ball test guess= 
  let rec loop test i = match test with
      [] -> 0
    | x::rest -> if ( i = (getPosizione x guess)) then 1 + loop rest (i+1) 
      else loop rest (i+1)
  in loop test 0

let strike_ball test guess = 
  let ballValue = (ball test guess) in ((strike test guess) - ballValue, ballValue)
(*
#use "es05.ml";;
[[1;2;3];[3;5;8];[1;4;9]];;
subset [1;2;3;4;5;6;7;8];;
subset [1;2;3] [1;2;3;4];;
eval $(opam env)
*)