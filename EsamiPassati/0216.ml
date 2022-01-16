type 'a action_graph = ('a * string * 'a) list

let successori node (graph:'a action_graph) = List.map (function (_,y,z) -> (y,z)) (List.filter (function (x,_,_) -> x = node) graph)


let grafo = [(1,"a",2); (1,"b",3); (1,"c",4); (2,"a",6); (3,"b",5);
             (3,"c",5); (4,"b",1); (4,"c",6); (5,"c",4);  (5,"a",5);
             (5,"b",5); (6,"b",5)]
let setadd x lst = if List.mem x lst then lst else x::lst

let rec nodes (g:'a action_graph) = match g with
    [] -> []
  | (x,_,z)::rest -> setadd z (setadd x (nodes rest))

(* tutti gli archi uscenti da node hanno nomi distinti se puntano a nomi distinti *)
let rec check g = match g with
  | [] -> true
  | (x,y,z)::rest -> let rec help lst = match lst with
        [] -> true
      | (a,b,c)::rest -> (y <> b || (a=x && c=z)) && help rest 
    in help rest && check rest

let move g start goal = 
  if start = goal then [] else 
    let rec fromnode visited (node) =
      if List.mem node visited then failwith "loop" else
      if node = goal then [] else
        fromlist (node::visited) (successori node g)
    and fromlist visited = function
        [] -> failwith "Nope"
      | (a,x)::rest -> try a::fromnode visited x with _ -> fromlist visited rest 
    in fromnode [] start 

(* 
#use "EsamiPassati/0216.ml";;
*)
