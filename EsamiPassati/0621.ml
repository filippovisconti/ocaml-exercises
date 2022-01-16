type 'a graph = ('a * 'a) list

let grafo = [(1,2);(1,4);(4,4);(2,4);(3,2);(4,3);(4,5);(5,6);(6,3);(3,6)]

let successori nodo (grafo:'a graph) = List.map snd (List.filter (function (x,y) -> x = nodo) grafo)
let depth_limited (g) start goal depth = 
  let rec from_node depth n=
    if depth < 0 then failwith "end" 
    else if n = goal then [n] else
      n::from_list (depth - 1) (successori n g)
  and from_list depth =function 
      [] -> failwith "Nope"
    | x::rest -> try from_node depth x with _ -> from_list depth rest
  in from_node depth start   

let path g start goal maxdepth = 
  let rec aux max = 
    try depth_limited g start goal max 
    with _ -> aux (max+1)
  in aux 0 

(* 
depth_limited grafo 1 6 1;;
depth_limited grafo 1 6 2;;
depth_limited grafo 1 6 3;;
depth_limited grafo 1 6 4;;
depth_limited grafo 1 6 5;;
depth_limited grafo 1 6 6;;
path grafo 1 6 5;;
#use "EsamiPassati/0621.ml";;
*)
