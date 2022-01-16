type 'a ntree = Tr of 'a * 'a ntree list

let rec pesi(Tr(x,tl)) = match tl with 
    [] -> [x]
  | _ -> List.map (function y -> x+y) (pesilist tl)
and pesilist = function 
    [] -> []
  | x::rest -> pesi x @ pesilist rest

