(* enumera: applicata a una lista lst=[x0;x1;x2;...;xk], riporti la lista di coppie [(0,x0);(1,x1);(2,x2);...;(k,xk)]. *)

let enumera lst =
  let rec loop lst i res = match lst with
      [] -> List.rev res
    | x::rest -> loop rest (i+1)  ((i,x)::res)
  in loop lst 0 []

let enumera_rec lst = 
  let rec aux lst i = match lst with
      [] -> []
    | x::rest -> (i,x)::aux rest (i+1)
  in aux lst 0

let alternate lst = 
  let rec aux lst i = match lst with
      [] -> []
    | x::rest -> if (i mod 2 <> 0) then (x::aux rest (i+1)) else aux rest (i +1)
  in aux lst 0

let max lst = match lst with
    [] -> failwith "Empty list"
  | x::rest -> let rec aux lst temp = match lst with   
        [] -> temp
      | x::rest -> if (x > temp) then aux rest x else aux rest temp
    in aux rest x
let min lst = match lst with
    [] -> failwith "Empty list"
  | x::rest ->
    let rec aux lst temp = match lst with   
        [] -> temp
      | x::rest -> if (x < temp) then aux rest x else aux rest temp
    in aux rest x
let mindeimax (y::resto)  = 
  let rec aux lst temp = match lst with
      [] -> temp
    | x::rest -> let m = (max x) in 
      if m < temp then aux rest m else aux rest temp
  in aux resto (max y)

let rec nondec lst =
  match lst with
    [] -> true
  | [_] -> true
  | x::y::rest -> x <= y && nondec (y::rest)

let pairwith y lst = List.map (function x -> (y,x)) lst 
let verifica_matrice n mat = List.exists (function x -> List.for_all (function k -> k < n) x) mat

let rec isPresent x = function
    [] -> false
  | y::rest ->  x = y || isPresent x rest
let setdiff set1 set2 = List.filter (function x -> not (isPresent x set2)) set1

let rec tutte_liste_con n x y = match n with  
    0 -> [[]]
  | n -> let pippo = tutte_liste_con (n-1) x y in 
    List.map (function k-> x::k) pippo @ List.map (function j -> y::j) pippo

let rec interleave x lst = match lst with
    [] -> [[x]]
  | y::rest -> (x::y::rest)::List.map (function k -> List.cons x k) (interleave x rest)

let rec permut = function
    [] -> [[]]
  | x::rest -> List.flatten(List.map (interleave x) (permut rest))


let rec find x = function
    [] -> raise Not_found
  | y::rest -> if x = y then (fst (find x rest), rest) else (fst(find x rest), [])



(* #use "ripasso.ml";;

   nondec [1;2;3;4;5;6;7;4;5;6];;
   nondec [1;2;3;4;5;6;7;4;9;8;10];;
   nondec [1;2;3;4;5;6;7];;
   nondec [1;2];;
   nondec [1];;
   nondec [];;

   setdiff [1;2;3;4;5;6;7;4;9;8;10] [1;2;3;4;5;6;7];;
   permut [1;2;3;4;5;6;7];;

   tutte_liste_con 3 0 1;;
*)