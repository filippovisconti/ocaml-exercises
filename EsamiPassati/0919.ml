type 'a tree = Empty
             | Tr of 'a * 'a tree * 'a tree

let rec labels = function
    Empty -> []
  | Tr(x, Empty, Empty) -> [x]
  | Tr(x, t1, t2) -> [x] @ labels t1 @ labels t2

let rec discendenti n = function
    Empty -> []
  | Tr(x, t1, t2) -> if x=n then labels (Tr(x,t1,t2)) else discendenti n t1 @ discendenti n t2
