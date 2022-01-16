let rec find p lst = match lst with
    [] -> raise Not_found
  | x::rest -> if p x then x else find p rest

let find_applicata lst = find (function x-> x*x<30) lst

let takewhile p lst = 
  let rec loop list res = match list with
      [] -> res
    | x::rest -> if p x then loop rest (x::res)
      else res
  in List.rev (loop lst [])

let rec takewhile_rec p = function
    [] -> []
  | x::rest -> if p x then x::takewhile_rec p rest else []
let dropwhile p lst = 
  let rec loop list = match list with
      [] -> []
    | x::rest -> if p x then loop rest 
      else x::rest
  in loop lst

let partition p lst = 
  let rec loop list ((yes,no) as res) = match list with
      [] -> res
    | x::rest -> if p x then loop rest (x::yes, no)
      else loop rest (yes, x::no)
  in loop lst ([],[])

let pairwith x lst = List.map (function el -> (x,el)) lst

let verifica_matrice n matrx = 
  List.exists (function x -> List.for_all (function x -> x < n) x) matrx

let rec isNotPresent k = function
    [] -> true
  | x::rest -> x <> k && isNotPresent k rest
let setdiff lst1 lst2 = 
  List.filter (function x -> isNotPresent x lst2) lst1 

let rec isPresent k = function
    [] -> false
  | x::rest -> x = k || isPresent k rest
let subset sub set = 
  List.for_all (function x -> isPresent x set) sub

let duplica lst = List.map (function x -> x*2) lst

let mapcons x list = 
  List.map (function k -> (fst k, x::(snd k))) list

let rec tutte_liste_con n x y = match n with
  | 0 -> [[]] 
  | n -> let pippo = (tutte_liste_con (n-1) x y) in 
    (List.map (fun k-> (x::k)) pippo) @ ( List.map (fun k-> (y::k)) pippo)


let rec interleave x lst = match lst with
    [] -> [[x]]
  | y::rest -> let pippo = interleave x rest in 
    (x::y::rest)::(List.map (List.cons y) pippo) 


let rec permut lst = match lst with
    [] -> [[]]
  | x::rest -> let pippo = permut rest in 
    List.flatten(List.map (interleave x) pippo) 


let labirinto = (5,
                 [((1,0),"oro"); ((3,1),"oro"); ((4,3),"oro");
                  ((0,1),"argento"); ((2,4),"argento"); 
                  ((0,2),"mostro"); ((1,1),"mostro"); ((1,3),"mostro"); ((2,3),"mostro");
                  ((3,0),"mostro"); ((4,2),"mostro")])
let r1 = 1
let v1 = "oro"
let r2 = 5
let in_riga lab riga valore = 
  let listarighe = snd (lab) in 
  List.exists (function ((x,y),z) -> x = riga && z=valore) listarighe

let trova_colonna lab riga valore = 
  let listarighe = snd (lab) in 
  let  ((_,y),_) = List.find (function ((x,y),z) -> x = riga && z=valore) listarighe in y


let upto n m = 
  let rec loop n m result =
    if (n > m) then result 
    else n::(loop (n+1) (m) result)
  in loop n m []


let in_tutte lab valore = 
  let elencorighe = upto 0 ((fst lab)-1) in 
  List.for_all (
    function x -> in_riga lab x valore
  ) elencorighe

let rec isPresent el list = match list with
    [] -> false
  | x::rest -> x = el || isPresent el rest

let rec drop n list =
  if n = 0 then list else
  if n >= (List.length list) then []
  else drop (n-1) (List.tl list)

let rec take n lista = match lista with
    [] -> []
  | x::rest -> 
    if n <= 0 then []
    else x :: take (n-1) rest;;

let trova x lst = 
  let rec trovaIndice x lst i = match lst with
      [] ->  raise Not_found
    | y::rest -> if x = y then i else trovaIndice x rest (i+1)
  in trovaIndice x lst 0

let find x lst = 
  let n = trova x lst in 
  (take (n-1) lst, drop (n+1) lst)

let find_rec x lst = 
  let rec loop lst (a,b) = match lst with
      [] -> raise Not_found
    | y::rest -> if x=y then (a,rest) else loop rest (a@[y],b)
  in loop lst ([],[])

let spezza x lst =
  let (a,b) = find_rec x lst in 
  find_rec x b

let rec remove x = function
    [] -> []
  | y::rest -> if x = y then rest 
    else y::(remove x rest)
let prendi p lst =
  let el = List.find p lst in
  (el, remove el lst)

let rec powerset = function
    [] -> [[]]
  | x::rest -> let pippo = powerset rest in (pippo) @ (List.map (List.cons x) pippo)

let rec cartprod setA setB = match setA with
    [] -> []
  | x::rest -> let pippo = cartprod rest setB in pippo @ (List.map (function y-> (x,y)) setB)

(*
takewhile (function n -> n mod 2 = 0) [0;2;4;6;7;8;9;10];;
takewhile_rec (function n -> n mod 2 = 0) [0;2;4;6;7;8;9;10];;
dropwhile (function n -> n mod 2 = 0) [0;2;4;6;7;8;9;10];;
partition (function n -> n mod 2 = 0) [0;2;4;6;7;8;9;10];;
verifica_matrice 1 [
    [1;2;3;4];
    [2;3;4;6];
    [7;8;9;10]
];;
subset [2;3;4;6;9] [1;2;3;4;5;6;7;4];;
mapcons 6 [
    (1,[1]);
    (2,[1;2]);
    (3,[2;3;4])
];;
tutte_liste_con 3 0 1;;
in_riga labirinto r1 v1;;
trova_colonna labirinto 3 v1;;
in_tutte labirinto "mostro";;
find 4 [1;2;3;4;5;6;7;4];;
find 9 [1;2;3;4;5;6;7;4];;
find_rec 4 [1;2;3;4;5;6;7;4];;
find_rec 9 [1;2;3;4;5;6;7;4];;
spezza 3 [1;2;3;4;5;6;3;7;8;9;3;10];;
prendi (function x -> x > 10) [3; 20; 7; 11; 8; 30; 20];;

cartprod [1;2;3;4] [7;8;9;10];;
#use "es06.ml";;
eval $(opam env)
*)