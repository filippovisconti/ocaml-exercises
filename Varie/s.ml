type 'a graph = ('a * 'a * int) list

let successori n (g:'a graph) = List.map (function (x,y,p) -> (y,p)) (List.filter (function (x,_,_) -> x = n) g)

let path g start goal =     
  let rec fromnode visited node pesi = 
    if List.mem node visited then failwith "loop"
    else if node = goal then [node]
    else node::fromlist (node::visited) pesi (successori node g) 
  and fromlist visited pesi = function 
      [] -> failwith "nope"
    | (y,p)::rest -> (try 
                        (if List.mem p pesi then failwith "no peso" 
                         else fromnode visited y (p::pesi)) 
                      with _ -> fromlist visited pesi rest)
  in fromnode [] start []

(* 
#use "EsamiPassati/s.ml";;
*)