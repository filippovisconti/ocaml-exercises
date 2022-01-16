(* IMPLEMENTAZIONE PARZIALE DELLA PROF
   (* merge sort *)
   (* definizione di relazione minore *)

   let minore (_,x) (_,y) = x<y;;

   let rec split list= match list with
    [] -> ([],[])
   | [x] -> ([x],[])
   |  x::y::rest -> let (xs,ys) = split rest
    in (x::xs,y::ys);;

   (* merge: 'a list -> 'a list -> 'a list*)
   (* merge: ( 'a * b ) list -> ( 'a * b ) list -> ( 'a * b ) list*)
   let rec merge xs ys = match (xs,ys) with
    ([],_) -> ys
   | (_,[]) -> xs
   | x::xs,y::ys -> if minore x y then x::merge xs (y::ys)
    else y::merge (x::xs) ys;; 

   let rec sort lista = match lista with
    [] -> []
   | [k] -> [k]
   | lista -> let (x,y) = split lista 
    in merge (sort x) (sort y);;

   let rec take n lista = match lista with
    [] -> []
   | x::rest -> 
    if n <= 0 then []
    else x :: take (n-1) rest;;*)

(* Algoritmo:
   Sia C l’insieme (lista) che si vuole ordinare
   Si divide l’insieme C in due parti pressappoco uguali. 
   Ciascuno dei due sottoinsiemi viene ordinato, ricorsivamente. 
   Si applica l’algoritmo di fusione alle liste risultanti dalle chiamate ricorsive
   Casi base: lista vuota o di un solo elemento
   Caso ricorsivo: per ordinare lst di lunghezza > 1:
   Si suddivide la lista in due parti di dimensioni pressappoco uguali (±1): siano xs e ys le due liste ottenute
   Il risultato è la fusione di (mergesort xs) e (mergesort ys):
   merge (mergesort xs) (mergesort ys) *)

let rec take n lst = 
  if n <= 0 then []
  else match lst with
      [] -> [] 
    | x::rest -> x::(take (n-1) rest) 

let rec drop n lst = 
  if n <= 0 then lst
  else match lst with
      [] -> [] 
    | x::rest -> drop (n-1) rest 

let split lst = 
  let l = List.length lst in 
  if l > 1 then (take (l/2) lst, drop (l/2) lst) else (lst, [])

let rec merge xs ys = match (xs,ys) with
    ([],_) -> ys
  | (_,[]) -> xs
  | (x::rex, y::rey) -> if x < y then x :: (merge rex ys) else y :: (merge xs rey)
let rec mergesort lst = match lst with
    [] -> []
  | [x] -> [x]
  | lst -> let (xs,ys) = split lst in
    merge (mergesort xs) (mergesort ys)

let rec insert k = function  
    [] -> [k]
  | x::rest -> if x < k then x::(insert k rest) else k::x::rest

let insertionsort list = 
  let rec loop list res = match list with
      [] -> res
    | x::rest -> loop rest (insert x res)
  in loop list []



  (*
#use "mergesort.ml";;
[[1;2;3];[3;5;8];[1;4;9]];;
subset [1;2;3;4;5;6;7;8];;
subset [1;2;3] [1;2;3;4];;
eval $(opam env)
*)