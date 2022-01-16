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

let replace list pos a  = List.mapi (fun i x -> if i = pos then a else x) list
let switch list first second =
  let firstvalue = List.nth list first in 
  let secondvalue = List.nth list second in 
  let listamezza = replace list second firstvalue in 
  replace listamezza first secondvalue



(* Partition(A, p, r)

    x = A[r]
    i = p-1

    for (j=p to r-1)
        if (A[j] ≤ x)
            i = i+1
            exchange A[i] with A[j]

    exchange A[i+1] with A[r]
    return i+1 *)


let partition list p r = 
  let pivot = List.nth list r in 
  let k = p-1 in

  let rec loop list j i = 
    if (j < (r-1)) then 
      if (List.nth list j) <= pivot then
        let ii = i+1 in 
        loop (switch list ii j) (j + 1) ii 
      else loop list (j+1) i
    else (i+1,list)
  in loop list p k 

(* QuickSort(A, p, r)

    if (p < r)
        q {(q, list)} = Partition(A, p, r)
        QuickSort(A, p, q-1)
        QuickSort(A, q+1, r) *)

(* p = 0, r = list.length *)
let rec quicksort lst p r = 
  if p < r then
    let (q, list) = partition lst p r in
    let list1 = quicksort list p (q-1)
    in let list2 = quicksort list1 (q + 1) r 
    in list2
  else lst

let startquick list = 
  quicksort list 0 (List.length list -1)

(* #use "quicksort.ml";; 
   startquick [3;6;2;7;8;9;1;2;4;88;11;33;5];;
   startquick [1;2;3;4;5;6;7;8;9];;
   startquick [9;8;7;6;5;4;3;2;1];;
*)