type ('a, 'b) pseudo = ('a list * 'b) list

let plist:('a, 'b) pseudo = [([1;2;3;4],"piccolo");([5;6;7;8],"medio"); ([9;10;11;12],"grande")]

let dimlist=["grande"; "piccolo"]

let g1 = [(1,100);(1,5);(1,3); (3,1);(100,10);(3,10);(5,10)]
let g2 = [(1,100);(1,5);(5,1);(100,10);(5,10)]

let rec dim x = function
    [] -> failwith "no dim found"
  | (lst,d)::rest -> if List.mem x lst then d else dim x rest

type 'a graph = ('a * 'a) list

let successori node graph = List.map snd (List.filter (function (x,y) -> x = node) graph)

let path plist dimlist g start goal = 
  let rec fromnode visited node = 
    (if List.mem node visited then failwith "loop"
     else (if List.mem (dim node plist) dimlist then (if node = goal then [goal] 
                                                      else node::fromlist (node::visited) (successori node g))
           else failwith "not dim"))
  and fromlist visited = function     
      [] -> failwith "nope"
    | x::rest -> try fromnode visited x with _ -> fromlist visited rest
  in fromnode [] start

(* 
path plist dimlist g1 1 10;;
path plist dimlist g2 1 10;;
#use "EsamiPassati/0716.ml";;
*)