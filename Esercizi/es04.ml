(* loop: res + lenght of list *)
let lenght list =
  let rec loop list res = match list with
      [] -> res
    |x::rest -> loop rest (1+res)
  in loop list 0

(* loop: res + sum of list items*)
let sumof list =
  let rec loop list res = match list with
      [] -> res
    |x::rest -> loop rest (x+res)
  in loop list 0

exception EmptyList
let maxlist list = 
  if (lenght list) = 0 then raise EmptyList
  else 
    let rec loop list max = match list with
        [] -> max
      |x::rest -> if x > max then loop rest x 
        else loop rest max
    in loop list (List.hd list)
let rec drop n list =
  if n = 0 then list else
  if n >= (List.length list) then []
  else drop (n-1) (List.tl list)

let append l1 l2 = 
  if (List.length l2) = 0 then l1 else
    let revlist1 = List.rev l1 in
    let rec loop list1 list2 = match list2 with
        [] -> list1
      | x::rest -> loop (x::list1) rest
    in List.rev (loop revlist1 l2)

let reverse list1 = 
  let rec loop list res = match list with
      [] -> res
    | x::rest -> loop rest (x::res)
  in loop list1 []

exception InvalidIndex

let nth n l = 
  if n < 0 || n >= List.length l then raise InvalidIndex
  else let rec loop i list = 
         if i = 0 then List.hd list else loop (i-1) (List.tl list)
    in loop n l

let remove x lst =
  let rec loop x list res = match list with
      [] -> res
    | y::rest -> if x = y then loop x rest res else loop x rest (y::res)
  in List.rev (loop x lst [])


let rec copy n x =
  if n > 0 then (x::(copy (n-1) x)) else []

let nondec lst= 
  let rec loop lst prec = match lst with
      [] -> true
    | x::rest -> x >= prec && loop rest x
  in loop (List.tl lst) (List.hd lst)

let rec pairwith y = function
    [] -> []
  | x::rest -> (y,x)::(pairwith y rest)

let rec duplica xs = match xs with
    [] -> []
  | x::rest -> x::x::(duplica rest)

let enumera lst =
  let rec loop i list res = match list with
      [] -> res
    | x::rest -> loop (i+1) rest ((i,x)::res)
  in List.rev (loop 0 lst [])

let position x lst =
  let rec loop i list = match list with
      [] -> raise Not_found
    | y::rest -> if y = x then i else loop (i+1) rest
  in loop 0 lst

let alternate lst =
  let rec loop i list res = match list with
      [] -> res 
    | x::rest -> if (i mod 2) = 1 then loop (i+1) rest (x::res) else loop (i+1) rest res
  in List.rev (loop 0 lst [])


let find_max list =
  let rec loop list temp = 
    match list with
      []->temp
    | x::rest -> if x > temp then loop list x
      else loop rest temp 
  in loop (List.tl list) (List.hd list) 

let find_min list =
  let rec loop list temp = 
    match list with
      []->temp
    |x::rest -> if x < temp then loop list x
      else loop rest temp 
  in loop (List.tl list) (List.hd list) 

let min_dei_max lst = 
  let rec loop list res = match list with
      [] -> res 
    | x::rest -> loop rest ((find_max x)::res)
  in find_min (loop lst [])

let rec take n lista = match lista with
    [] -> []
  | x::rest -> 
    if n <= 0 then []
    else x :: take (n-1) rest;;
let split2 lst = 
  let n = ((List.length lst) / 2) in
  (take n lst, drop n lst)

let esegui () =
  begin
    print_newline ()
  end

  (*
   #use "es04.ml";;
   [[1;2;3];[3;5;8];[1;4;9]];;
   [1;2;3;4]
   [5;6;7;8]

   eval $(opam env)
   *)