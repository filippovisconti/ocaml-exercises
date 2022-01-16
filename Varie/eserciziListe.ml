(*  upto: int -> int -> int list *)

let upto n m = 
  let rec loop n m result =
    if (n > m) then result 
    else n::(loop (n+1) (m) result)
  in loop n m []


(* flatten: ’a list list -> ’a list *)
(* porcata da rivedere, due rev da togliere
*)
let flatten listOfLists = 

  let rec loop list result = match list with
      [] -> result
    | hd::tl -> loop tl result@(List.rev hd)
  in List.rev (loop listOfLists [])

(* conta : ’a -> ’a list -> int *)
(* contatutti : ’a list -> ’a list -> (’a * int) list *)
let conta n lista =
  let rec loop n lista res = match lista with
      [] -> res
    |h::t -> if (h = n) then loop n t (res + 1) else loop n t res
  in loop n lista 0

let contatutti elementi flatLista = 

  let rec loop elements listona result = 
    match elements with
      [] -> result
    | h::t -> loop t listona ((h, conta h listona)::result)
  in List.rev (loop elementi flatLista [])

(*
    #use "eserciziListe.ml";;
    flatten [[1;2;3];[4;5];[6];[1;2;2];[4;4;4;4];[6]]
    contatutti (upto 1 10) (flatten [[1;2;3];[4;5];[6];[1;2;2];[4;4;4;4];[6]]);;
*)

let rec ricerca k listassoc = 
  match listassoc with 
    [] -> failwith "ricerca"
  | (x,y) :: rest -> 
    if x=k then y
    else ricerca k rest

let rec mem e set =
  match set with
    [] -> false
  | x :: rest -> e = x || mem e rest


let rec isPresent el list = match list with
    [] -> false
  | x::rest -> x = el || isPresent el rest

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