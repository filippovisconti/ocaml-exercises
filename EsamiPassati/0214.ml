type 'a graph = ('a * 'a) list

let rec vicini node = function  
    [] -> []
  | (x,y)::rest -> vicini node rest @ (if x = node then [y] 
                                       else if y = node then [x] 
                                       else [])

let path g p k start = 
  let rec fromnode visited k node = 
    if List.mem node visited then failwith "loop"
    else if k = 0 then [] 
    else if p node then node::fromlist (node::visited) (k-1) (vicini node g)
    else failwith "No p"
  and fromlist visited k = function
      [] -> failwith "nope"
    | x::rest -> try fromnode visited k x with _ -> fromlist visited k rest 
  in fromnode [] k start


(* 
#use "EsamiPassati/0214.ml";;
*)