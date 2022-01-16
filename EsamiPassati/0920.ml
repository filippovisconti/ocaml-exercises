type 'a graph = ('a * 'a) list

let rec successori g node = List.map snd (List.filter (function (x,y) -> x = node) g)
let percorso g start tappa target = 
  let rec from_node visited node tbool = 
    if List.mem node visited then failwith "Loop"
    else if node = target then if tbool || node = tappa then [node] else failwith "No tappa"
    else node::from_list (node::visited) (if node = tappa then true else tbool) (successori g node)
  and from_list visited tbool = function
      [] -> failwith "nope"
    | x::rest -> try from_node visited x tbool with _ -> from_list visited tbool rest  
  in from_node [] start false  