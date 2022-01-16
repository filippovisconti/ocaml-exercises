type 'a graph = ('a * 'a) list

let g = [(1,2);(1,3);(3,4);(4,3);(4,2);(2,5);(5,5)]

let pl1 = [None;None;None;None;None]
let pl2 = [Some 1;None;Some 4;Some 3;None]
let pl3 = [None;None;Some 3]
let pl4 = [None;None;None;None]
let pl5 = [Some 1;None;Some 4;Some 3]
let successori nodo (grafo:'a graph) = List.map snd (List.filter (function (x,y) -> x = nodo) grafo)


let whichpath g plist start goal = 
  let rec from_node plist visited node = 
    if plist = [] then
      if List.mem node visited then failwith "loop"
      else if node = goal then [goal] 
      else node::from_list plist (node::visited) (successori node g) 
    else if node = goal then failwith "not enough nodi" else
      (match List.hd plist with
       | Some x -> (if node = x then node::from_list (List.tl plist) (node::visited) (successori node g) 
                    else failwith "not right node")
       | None -> node::from_list (List.tl plist) (node::visited) (successori node g))
  and from_list plist visited = function
      [] -> failwith "Nope"
    | x::rest -> try from_node plist visited x with _ -> from_list plist visited rest
  in from_node plist [] start

(* 
whichpath g pl1 1 5;;
whichpath g pl2 1 5;;
whichpath g pl3 1 4;;
whichpath g pl4 1 4;;
whichpath g pl5 1 5;;
#use "EsamiPassati/0217.ml";;
*)
