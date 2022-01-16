type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
let rec tree_exists p = function
    Empty -> false
  | Tr(x,t1,t2) -> p x || tree_exists p t1 || tree_exists p t2 

let rec raccogli = function
    Empty -> []
  | Tr(x,t1,t2) -> x::(raccogli t1 @ raccogli t2)

let raccogli_it t = 
  let rec aux result = function
    | Empty -> result
    | Tr(x,t1,t2) -> aux (aux (x::result) t1) t2
  in aux [] t

let rec foglie = function
  | Empty -> []
  | Tr(x,Empty,Empty) -> [x] 
  | Tr(x,t1,t2) -> (foglie t1) @ (foglie t2)

let rec nodi_con_un_figlio = function
  | Empty | Tr(_,Empty,Empty) -> []
  | Tr(x,Empty,t) | Tr(x,t,Empty) -> x::(nodi_con_un_figlio t)
  | Tr(x,t1,t2) -> (nodi_con_un_figlio t1) @ (nodi_con_un_figlio t2)

let rec ramo x = function
  | Empty -> failwith "Foglia non trovata"
  | Tr(y,Empty,Empty) ->  if x = y then [x] else  failwith "ramo toppato"
  | Tr(y,t1,t2) ->  y:: try ramo x t1 with _ -> ramo x t2 

let rec add y = function    
    [] -> [(y,1)]
  | (x,n)::rest -> if x = y then (x,n+1)::rest
    else (x,n)::add y rest
let count t =
  let rec aux result = function
    | Empty -> result
    | Tr(a,t1,t2) -> aux (aux (add a result) t1) t2
  in aux [] t

let rec foglie_in_lista lista = function 
    Empty -> true
  | Tr(a, Empty, Empty) -> List.mem a lista 
  | Tr(a, t1, t2) -> foglie_in_lista lista t1 && foglie_in_lista lista t2

let rec foglia_costo = function
    Empty -> failwith "albero vuoto"
  | Tr(x, Empty, Empty) -> (x,x)
  | Tr(x, t, Empty) | Tr(x, Empty, t)-> let (f,c) = foglia_costo t in (f,c+x) 
  | Tr(x, t1, t2) -> let (f,c) = foglia_costo t1 in 
    let (f1,c1) = foglia_costo t2 in
    if c > c1 then (f,c+x) else (f1, c1+x)
and fclist = function   
    [] -> failwith "non accade"
  | [t] -> foglia_costo t
  | t::rest -> max (foglia_costo t) (fclist rest)

type 'a option = None | Some of 'a

let (<<) a = function
    None -> true
  | Some b -> a<b

let (>>) a = function
    None -> true
  | Some b -> a>b

let abr_check t = 
  let rec abr_util minv maxv = function
      Empty -> true
    | Tr((x,_), left, right) ->  
      x >> minv && x << maxv && 
      abr_util minv (Some x) left &&
      abr_util (Some x) maxv right
  in abr_util None None t

let rec take n lista = match lista with
    [] -> []
  | x::rest -> 
    if n <= 0 then []
    else x :: take (n-1) rest;;

let rec drop n list =
  if n = 0 then list else
  if n >= (List.length list) then []
  else drop (n-1) (List.tl list)

let split2 lst = 
  let n = ((List.length lst) / 2) in
  (take n lst, drop n lst)
let rec balinorder lst = 
  if lst = [] then Empty else
    let (left,right) = split2 lst in  
    Tr(List.hd right, balinorder left, balinorder (List.tl right))

let rec balpreorder (x::rest) = 
  if rest = [] then Empty else
    let (left, right) = split2 rest in
    Tr(x, balpreorder left, balpreorder right)

(* mapping t1 t1 = lista di coopie (x,y) tali che t1 ha un nodo etichettato da x e t2 ha un noto etichettato da x. Fallisce se t1 e t2 hanno strutture diverse *)
let rec mapping t1 t2 = match (t1, t2) with 
    (Empty, Empty) -> []
  | (Tr(x, v1, v2), Tr(y, u1,u2)) -> (x,y)::(mapping v1 u1)@(mapping v2 u2)
  | _ -> failwith "f@ck"

let rec isAfunction = function 
    [] -> true
  | (x,y)::rest -> (List.for_all (function (xx,z) -> xx<>x || z = y) 
      rest) && isAfunction rest

let isAmapping t1 t2 = 
  try isAfunction (mapping t1 t2) 
  with _ -> false