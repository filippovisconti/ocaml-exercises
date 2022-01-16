type 'a tree = Empty
             | Tr of 'a * 'a tree * 'a tree

let is_empty = function
    Empty -> true
  | _ -> false

exception EmptyTree
let root = function
    Empty -> raise EmptyTree
  | Tr(x,_,_) -> x

let is_leaf = function
    Tr(_,Empty,Empty) -> true
  | _ -> false

let leaf x =
  Tr(x,Empty,Empty)

let left = function
    Empty ->
    raise EmptyTree
  | Tr(_,t,_) -> t

let right = function
    Empty ->
    raise EmptyTree
  | Tr(_,_,t) -> t

let rec size = function
    Empty -> 0
  | Tr(_,t1,t2) -> 1 + size t1 + size t2;;

(* ----------------------------------------------- *)


let rec add (y,n) = function 
    [] -> [(y,n)]
  | (x,k)::rest -> if x = y then (x,k+n)::rest else (x,k)::(add (y,n) rest)
let rec addTogether l1 = function
    [] -> l1
  | (x,k)::rest -> addTogether (add (x,k) l1) rest
let rec count = function 
    Empty -> []
  | Tr(x,left,right) -> add x (addTogether (count left) (count right))

let count_it t = 
  let rec aux res = function
      Empty -> res
    | Tr(x,left,right) -> aux (aux (add x res) left) right 

  in aux [] t

(* ----------------------------------------------- *)
let rec reflect = function  
    Empty->Empty
  | Tr(x,left,right) -> Tr(x, reflect right, reflect left)

let fulltree n = 
  if n = 0 then Empty else
    let rec aux h k x = if k = h then Empty else Tr(x, aux h (k+1) (2*x),aux h (k+1) (2*x +1))

    in Tr(1, aux n 1 2, aux n 1 3)

let rec height = function
    Empty -> 0
  | Tr(_,l,r) -> 1 + max (height l) (height r) 

let balanced = function
    Empty -> true
  | Tr(_,l,r) -> abs (height l - height r) <= 1

let rec preorder = function
    Empty -> []
  | Tr(x,l,r) -> [x]@(preorder l)@(preorder r)

let rec postorder = function
    Empty -> []
  | Tr(x,l,r) -> (postorder l)@(postorder r)@[x]

let rec inorder = function
    Empty -> []
  | Tr(x,l,r) -> (inorder l)@[x]@(inorder r)

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

let rec balpreorder = function
    [] -> Empty
  | x::rest -> let (left, right) = split2 rest in Tr (x, balpreorder left, balpreorder right)

let rec balinorder = function
    [] -> Empty
  | lst -> let (left, right) = split2 lst in Tr (List.hd right, balpreorder left, balpreorder (List.tl right))

let rec listaDiFoglie =  function
    Empty -> []
  | Tr(x,Empty,Empty) -> [x]
  | Tr(x,l,r) -> listaDiFoglie l @ listaDiFoglie r


let rec numFoglie =  function
    Empty -> 0
  | Tr(x,Empty,Empty) -> 1
  | Tr(x,l,r) -> numFoglie l + numFoglie r

let foglie_in_lista lst tr =
  List.for_all (function x -> List.mem x lst) (listaDiFoglie tr)


let rec segui_bool lst t = match t with
    Empty -> failwith "f&ck" 
  | Tr(_,l,r) -> match lst with
      [] -> root t
    | true::rest -> segui_bool rest l
    | false::rest -> segui_bool rest r


let foglia_costo t = 
  let rec aux res = function
      Empty -> (0,0)
    | Tr(x,Empty,Empty) -> (x,res +x)
    | Tr(x,l,r) -> max (aux (res+x) l) (aux (res+x) r)

  in aux 0 t


let foglie_costi t =  
  let rec aux res = function
      Empty -> [(0,0)]
    | Tr(x,Empty,Empty) -> [(x,res +x)]
    | Tr(x,l,r) -> (aux (res+x) l) @ (aux (res+x) r)

  in aux 0 t

(* ----------------------------------------------- *)
type expr =
    Jolly
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let rec pm pattern expr = match (pattern, expr) with
    (Jolly,_) -> true
  | (Int k, Int n) -> k=n
  | (Var k, Var n) -> k=n
  | (Sum (p1,p2), Sum (e1,e2)) -> pm p1 e1 && pm p2 e2
  | (Diff (p1,p2), Diff (e1,e2)) -> pm p1 e1 && pm p2 e2
  | (Mult (p1,p2), Mult (e1,e2)) -> pm p1 e1 && pm p2 e2
  | (Div (p1,p2), Div (e1,e2)) -> pm p1 e1 && pm p2 e2
  | _ -> false

(* ----------------------------------------------- *)

let rec max_common_subtree t1 t2 = match (t1,t2) with
    (Empty, Empty) | (Tr(_,_, _), Empty) | (Empty, Tr(_,_, _)) -> Empty

  | (Tr(x,Empty, Empty), Tr(y,Empty, Empty))  -> 
    if x = y then Tr(x, Empty, Empty) else Tr("@", Empty, Empty)

  | (Tr(x,l, r), Tr(y,Empty, Empty)) | (Tr(x,Empty, Empty), Tr(y,l, r)) -> 
    if x = y then Tr(x,Empty, Empty) else Tr("@", Empty, Empty)

  | (Tr(x,l1, r1), Tr(y,l2, r2)) -> 
    if x = y then Tr(x, max_common_subtree l1 l2, max_common_subtree r1 r2) else Tr("@", Empty, Empty)


let rec stessa_struttura t1 t2 = match (t1,t2) with
    (Empty, Empty) -> true
  | (Tr(x,Empty, Empty), Tr(y,Empty, Empty)) -> true
  | (Tr(x,l1, r1), Tr(y,l2, r2)) -> stessa_struttura l1 l2 && stessa_struttura r1 r2
  | _ -> false

let rec lista_mapping t1 t2 = match (t1,t2) with
    (Empty, Empty) -> []
  | (Tr(x,l1, r1), Tr(y,l2, r2)) -> [(x,y)] @ lista_mapping l1 l2 @ lista_mapping r1 r2
  | _ -> failwith "f&ck"

let rec esiste_mapping = function
    [] -> true
  | (x,y)::rest -> List.for_all (function (xx,yy) -> x <> xx || y == yy) rest && esiste_mapping rest

let isAmapping t1 t2 = try esiste_mapping (lista_mapping t1 t2) with _ -> false

let rec path p = function
    Empty -> []
  |Tr(x,l,r) -> if p x then failwith "f&ck" else x:: try path p l with _ -> path p r

type 'a sostituzione = ('a * 'a tree) list

let rec scambiafoglia (k,kt) = function
    Empty -> Empty
  |Tr(_,
      (Tr(x,Empty,Empty) as t1),
      (Tr(y,Empty,Empty) as t2)) 
    as t-> 
    if k = x && k = y then Tr(x,kt,kt) 
    else if k = x then Tr(x,kt,t2) 
    else if k = y then Tr(x,t1,kt) 
    else t
  | Tr(x,l,r) -> Tr(x, scambiafoglia (k,kt) l, scambiafoglia (k,kt) r)



let rec applica subst t = match subst with
    [] -> t
  | (x,(Tr(_,_,_) as xt))::rest -> applica rest (scambiafoglia (x,xt) t)
  | k::rest -> failwith "noalbero"
(*)
  let listaCamminiAFoglie t = 
  let rec aux res = function
      Empty -> res
    | Tr(x,l,r) -> let m = List.map (function k -> List.append k [x] ) res in 
      (aux (m) l @ aux (m) r)
  in aux [] t

  let path_coprente t lst = 
  let rec aux = function
      [] -> failwith "f&ck"
    | l::rest -> if (List.for_all (function k -> List.mem k l) lst) then l else aux rest
  in aux (listaCamminiAFoglie t)*)
(*
let diff lst lst2 = (* lst - lst2 *)
  let rec loop list res = match list with
      [] -> res
    | y::rest -> if (List.mem y lst2) then loop rest res else loop rest (y::res)
  in List.rev (loop lst [])
let rec contieneTutti test = function
    [] -> []
  | x::rest -> if List.mem x test then x::(contieneTutti test rest) else failwith "non li contiene tutti"
let rec path_coprente t lst vis = match t with
    Empty -> failwith "f&ck su empty"
  | Tr(x, Empty, Empty) -> [x]
  | Tr(x,l,r) -> (* controllo che il cammino contenga tutta la lista *)
    try contieneTutti (x::(path_coprente r lst (x::vis))) (diff lst vis) 
    with _ -> contieneTutti (x::(path_coprente l lst (x::vis))) (diff lst vis) *)
let rec remove k = function
    [] -> []
  | x::rest -> if x = k then remove k rest else x::remove k rest

let rec path_coprente t lst = match t with
    Empty -> failwith "eeee"
  | Tr(x, Empty, Empty) -> if lst = [] || List.hd lst = x then [x] else failwith "ffff"
  | Tr(x,l,r) -> x::if List.mem x lst then try path_coprente l (remove x lst) with _ -> path_coprente r (remove x lst)
    else try path_coprente l lst with _ -> path_coprente r lst


(* --Colori-- *)
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

let rec colore n = function
  | [] -> raise Not_found
  | (x,y) :: rest -> if List.mem n y then x else colore n rest


(* --ABR-- *)

let rec abr_check = function 
    Empty -> failwith "empty"
  | Tr((x,y),Empty, Empty) -> true
  | Tr((x,y),l,r) -> x > fst (root l) && x < fst( root r) && abr_check l && abr_check r

let rec abr_search k = function 
    Empty -> failwith "empty"
  | Tr((x,y),l,r) -> if x = k then y else try abr_search k l with _ -> abr_search k r

let rec abr_update (k,v)  = function 
    Empty -> Empty
  | Tr((x,y),l,r) -> if k = x then  Tr((k,v), l,r) else Tr((x,y), (abr_update (k,v) l), (abr_update (k,v) r))

let rec abr_min = function 
    Empty -> failwith "emptyTree"
  | Tr(x, Empty, r) -> (x)
  | Tr(x,l,r) -> abr_min l

let rec abr_eliminaMin = function
    Empty -> failwith "emptyTree"
  | Tr(x, Empty, r) -> r
  | Tr(x,l,r) -> Tr(x, abr_eliminaMin l, r)
let abr_delmin t = match t with 
    Empty -> failwith "emptyTree"
  | Tr(x, Empty, r) -> (x, r)
  | Tr(x,l,r) -> (abr_min t, abr_eliminaMin t)

let rec abr_delete k = function
    Empty -> failwith "emptyTree"
  | Tr(x, Empty, r) as t -> if x = k then r else abr_delete k t
  | Tr(x, l, Empty) as t -> if x = k then l else abr_delete k t
  | Tr(x,l,r) as t -> if x = k then Tr(abr_min r, l, abr_eliminaMin r) else abr_delete k t

let rec abr_insert k = function
    Empty -> Empty
  (*| Tr(x, Empty , Empty) -> 
    if k <= x then Tr(x,Tr(k, Empty, Empty), Empty) 
    else Tr(x,Empty, Tr(k, Empty, Empty))*)
  | Tr(x,l,r) ->  if k <= x then 
      try 
        if root l >= k then Tr(x, abr_insert k l,r)
        else Tr(x,Tr(k, l, Empty), r) 
      with _ -> 
        Tr(x,Tr(k, Empty, Empty), r) 
    else 
      try 
        if root r < k then Tr(x, l, abr_insert k r)
        else Tr(x,l, Tr(k, Empty, r)) 
      with _ -> 
        Tr(x,l, Tr(k, Empty, Empty))


let tree_sort lst = 
  let rec aux t = function
      [] -> t
    | x::rest -> aux (abr_insert x t) rest
  in let t = Tr(List.hd lst, Empty, Empty) in inorder (aux t (List.tl lst))


(* --Luglio 2019-- *)

(* nramo: int -> int tree -> int list *)
let rec nramo k = function
    Empty -> failwith "f&ck"
  | Tr(x, Empty, Empty) -> if k = x then [x] else failwith "f&ck"
  | Tr(x, t1,t2) -> if x > k then failwith "sballo"
    else x::try nramo (k-x) t1 with _ -> nramo (k-x) t2


let rec rami = function
    Empty -> failwith "e"
  | Tr(x,Empty, Empty) -> [[x]]
  | Tr(x,l,r) -> List.map (List.cons x) (rami l @rami r)

let cons x lst = x::lst
let rec aggTesta k = function
    [] -> []
  | x::rest -> cons k x::aggTesta k rest

let rec rami = function
    Empty -> failwith "e"
  | Tr(x,Empty, Empty) -> [[x]]
  | Tr(x,l,r) -> List.map (function k -> x::k) (rami l @rami r)

let rec accoppiaNodi t1 t2 = match (t1,t2) with
    (Empty,Empty) -> failwith "empty"
  | (Tr(x,Empty,Empty),Tr(y,Empty,Empty)) -> [(x,y)]
  | (Tr(x,l,r),Tr(y,l2,r2)) -> [(x,y)] @ (accoppiaNodi l l2) @ (accoppiaNodi r r2)
  | _ -> failwith "not same structure"

let rec isAfunction = function 
    [] -> true
  | (x,y) :: rest -> (try List.assoc x rest = y  with _ -> true) && isAfunction rest
let esiste_mapping t1 t2 = let l = accoppiaNodi t1 t2 in  isAfunction (l)

let rec mcs t1 t2 = match (t1,t2) with
    (Empty, Empty) -> Empty
  | (Tr(x,l,r),Tr(y,l2,r2)) -> if x=y then Tr(x,mcs l l2, mcs r r2) else Tr("@", Empty, Empty)
  | _ -> Tr("@", Empty, Empty)

let rec same_structure t1 t2 = match (t1,t2) with
    (Empty,Empty) -> true
  | (Tr(_,l,r),Tr(_,l2,r2)) -> same_structure l l2 && same_structure r r2
  | _ -> false

let rec path_to2 k coas = function
    Empty -> failwith "empty"
  | Tr(x,Empty,Empty) -> if x = k then [x] else failwith "not foglia giusta"
  | Tr(x,l,r) ->  x::
                  try if (colore x coas) <> (colore (root l) coas) then 
                      path_to2 k coas l 
                    else failwith "non alterni"
                  with _ -> 
                    if (colore x coas) <> (colore (root r) coas) then 
                      path_to2 k coas r 
                    else failwith "non alterni2"
(* ----------------------TEST DATA------------------------- *)
let oneToFive = [1;2;3;4;5];;

let balTree = fulltree 4;;

let boolL1 = [true;false;false];; (* 11 *)
let boolL2 = [false;true];; (* 6 *)
let boolL3 = [true;false;false;true;false;false];; (* fail *)

let strTr1 = Tr ("1",
                 Tr ("2", Tr ("4", Tr ("8", Empty, Empty), Tr ("9", Empty, Empty)),
                     Tr ("5", Tr ("10", Empty, Empty), Tr ("11", Empty, Empty))),
                 Tr ("3", Tr ("6", Tr ("12", Empty, Empty), Tr ("13", Empty, Empty)),
                     Tr ("7", Tr ("14", Empty, Empty), Tr ("15", Empty, Empty))))
let strTr2 = Tr ("1",
                 Tr ("2", Tr ("4", Tr ("8", Empty, Empty), Tr ("9", Empty, Empty)),
                     Tr ("5", Tr ("10", Empty, Empty), Tr ("11", Empty, Empty))),
                 Tr ("3", Tr ("6", Empty, Empty),Empty))


(*
  #use "Esercizi/es08.ml";;
  eval $(opam env)
    preorder (fulltree 4);;
    postorder (fulltree 4);;
    inorder (fulltree 4);;
  height (fulltree 8);;
  balanced (fulltree 4);;


  balpreorder l;;  
  balinorder l;;

  preorder (balpreorder l);;
  inorder  (balinorder l);;

segui_bool boolL1 balTree;;
segui_bool boolL2 balTree;;
segui_bool boolL3 balTree;;

foglia_costo balTree;; == 26
foglie_costi balTree;; 
path_coprente balTree [3;6];;

listaCamminiAFoglie balTree;;
tree_sort [4;2;6;9;1;10;3;7];;

same_structure balTree balTree;;
same_structure strTr1 strTr2;;

*)