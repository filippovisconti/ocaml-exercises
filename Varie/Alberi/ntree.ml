type expr = 
    Int of int
  | Var of string
  | Diff of expr*expr
  | Div of expr*expr
  | Sum of expr list
  | Mult of expr list

type ambiente = (string*int) list

let rec eval env = function
    Int n -> n
  | Var x -> (try List.assoc x env with Not_found -> failwith "eval")
  | Diff (e1,e2) -> (eval env e1) - (eval env e2)
  | Div (e1,e2) -> (eval env e1) / (eval env e2)
  | _ -> failwith " Fuck this "

type 'a ntree = Tr of 'a * 'a ntree list
let leaf x = Tr(x,[])

let rec size (Tr(x,tlist)) = match tlist with
    [] -> 1
  | t::rest -> size t + size(Tr(x,rest))

let rec foglie_in_lista lista (Tr(x,tlist)) = 
  if tlist = [] then List.mem x lista 
  else List.for_all (foglie_in_lista lista) tlist 

let rec foglie_in_lista2 lista (Tr(x,tlist)) = 
  if tlist = [] then List.mem x lista 
  else flist lista tlist 
and flist lista = function
    [] -> true
  | t::rest -> foglie_in_lista2 lista t && flist lista rest

(* Altezza di un albero *)
(* maxl: 'a list -> 'a 
   maxl lst = elemento massimo di lst *)
let rec maxl = function
    [x] -> x
  | x::xs -> max x (maxl xs)
  | _ -> failwith "maxl"

let rec branching_factor (Tr(_,tlist)) = match tlist with
    [] -> 0
  | _ -> max (List.length tlist) (maxl (List.map branching_factor tlist))

let rec branching_factor2 (Tr(_,tlist)) = match tlist with  
    [] -> 0
  | _ -> max (List.length tlist) (0)

let maxx (a,b) (c,d) = if b>d then (a,b) else (c,d)
(*let rec foglia_costo (Tr(x,tlist)) = match tlist with 
    [] -> (x,x)
  | _ -> let (y,c) = fclist tlist in (y,c+x) 
  and fclist = function   
    [] -> failwith "non accade"
  | [t] -> foglia_costo t
  | t::rest -> maxx (foglia_costo t) (fclist rest)*)

let rec mapping (Tr(x, tlist)) (Tr(y, tlist2)) = 
  (x,y) :: maplist (tlist,tlist2) 
and maplist = function
    ([],[]) -> []
  |(x::rest1, y::rest2) -> (mapping x y) @ (maplist (rest1,rest2))
  | _ -> failwith "f@ck"


let rec mapping2 (Tr(x, tlist)) (Tr(y, tlist2)) = 
  match (tlist,tlist2) with
    ([],[]) -> [(x,y)]
  | (t1::rest1, t2::rest2) -> 
    (mapping t1 t2) @ mapping (Tr(x,rest1)) (Tr(y,rest2)) 
  | _ -> failwith "fuuuck"

let rec nodiAlbero (Tr(x, tlist)) = 
  match tlist with 
    [] -> [x]
  | t::rest -> (nodiAlbero t) @ (nodiAlbero (Tr(x, rest)))

let rec nramo k = function
  | Tr(x, []) -> if k = x then [x] else failwith "f&ck"
  | Tr(x, tlist) -> if x > k then failwith "sballo"
    else x::nramolist (k-x) tlist
and nramolist k = function
    [] -> failwith "f&ck"
  | x::rest -> try nramo k x with _ -> nramolist k rest

let rec nramo k (Tr(x, tlist)) = match tlist with
  | [] -> if k = x then [x] else failwith "f&ck"
  | [t] -> x::nramo (k-x) t
  | t::rest -> try  x::nramo (k-x) t with _ -> nramo (k) (Tr(x,rest))