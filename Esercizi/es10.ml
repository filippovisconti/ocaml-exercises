(* grafo rappresentato tramite lista di archi *)
type 'a graph = ('a * 'a) list

let rec setadd x set = 
  if List.mem x set then set else x::set


(* grafo orientato -> successori 
   Ho una lista di archi, la scorro e prendo tutti i nodi di arrivo a partire dal nodo dato *)
let successori (grafo:'a graph) nodo = 
  List.map snd (List.filter (function (x,_) -> x = nodo ) grafo)

(* grafo non orientato -> vicini *)
let rec vicini nodo (grafo:'a graph) = match grafo with
    [] -> []
  | (x,y)::rest -> 
    if x = nodo then y::vicini nodo rest
    else if y = nodo then x::vicini nodo rest 
    else vicini nodo rest

(* lista di tutti i nodi del grafo *)
let rec nodes = function
    [] -> []
  | (x,y)::rest -> setadd x (setadd y (nodes rest))

let depth_first_collect graph start = 
  let rec search visited = function
      [] -> visited
    | x::rest ->
      if List.mem x visited then search visited rest
      else search (x::visited) (successori graph x @ rest)

  in search [] [start]

let g:'a graph = [(1, 2); (1, 3); (1, 4); (2, 6); (3, 5); (4, 6); (6, 5); (6, 7); (5, 4)]

let test_connessi2 graph n m = List.mem m (depth_first_collect graph n)

let esiste_ciclo graph n = 
  let rec search visited = function
      [] -> false
    | x::rest -> if x = n then List.mem x visited 
      else if List.mem x visited then search visited rest 
      else search (x::visited) (successori graph x @ rest)
  in search [] [n]


type 'a graph2 = 'a list * ('a * 'a) list

(* per grafi non orientati *)
let raggiungibile graph n m =
  let rec aux visited = function
      [] -> raise Not_found
    | x::rest -> if List.mem x visited then aux visited rest
      else x = m || aux (x::visited) (vicini x graph @ rest) 
  in aux [] [n]

let grafo_connesso (nodi, archi) =
  let rec aux nodi = match nodi with
      [] -> true
    |[x] -> true
    | x::y::rest -> raggiungibile archi x y && aux (y::rest)
  in aux nodi


let test_connessi g a b =
  let rec aux visited = function 
      [] -> false
    | x::rest -> x = b || aux (x::visited) (successori g a @ rest)
  in aux [] [a]

let rec rimNoRep k = function
    [] -> []
  | x::rest -> if x = k then rest else x::rimNoRep k rest

(* lst contiene m *)
let cammino (_,archi) lst n m = 
  let rec from_node node = function
      [] -> failwith "nope"
    | [x] -> 
      if x = m && node = x then [x] 
      else failwith "nope"
    | lst -> 
      if List.mem node lst 
      then 
        node :: from_list (rimNoRep node lst) (successori archi node)
      else failwith "non in lst"
  and from_list lst = function
      [] -> failwith "noope"
    | x::rest -> 
      try from_node x lst 
      with _ -> from_list lst rest
  in n::from_list lst (successori archi n)

let hamiltoniano ((x::rest,_) as g) = cammino g (x::rest) x x

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

let rec colore x = function
    [] -> failwith "nope"
  | (c,lst)::rest -> if List.mem x lst then c else colore x rest

let colori_alterni graph colass start goal = 
  let rec from_node visited node = 
    if List.mem node visited then failwith "loop"
    else if node = goal then [node] else
      node::from_list (node::visited) (colore node colass) (successori graph node)
  and from_list visited oldCol = function
      [] -> failwith "no fuck"
    | x::rest -> 
      try 
        if (colore x colass) <> oldCol then from_node visited x 
        else failwith "same color"
      with _ -> from_list visited oldCol rest
  in from_node [] start

let tc g n m =
  let rec aux visited = function
      [] -> false
    | x::rest -> if List.mem x visited then aux visited rest 
      else x = m || aux (x::visited) (successori g x @ rest)
  in aux [] [n]
let rec connessi_in_glist b c = function
    [] -> false
  | x::rest -> test_connessi x b c || test_connessi x c b ||  connessi_in_glist b c rest

(* start non è in lst *)
let cammino_con_nodi graph start lst =
  let rec from_node visited lst node = 
    if List.mem node visited then failwith "loop"
    else if lst = [] then []
    else node:: from_list visited (if List.mem node lst then (rimNoRep node lst) else lst) (successori graph node)
  and from_list visited lst = function
      [] -> raise Not_found
    | x::rest -> try from_node visited lst x with _ -> from_list visited lst rest
  in from_node [] lst start

type chiave = Aperta | Chiusa
type cassaforte = chiave list

let giraPrima = function
    [] -> []
  | Aperta::rest -> Chiusa::rest
  | Chiusa::rest -> Aperta::rest

let rec giraDopoChiusa = function
    [] -> failwith "f$ck"
  | Chiusa::Chiusa::rest -> Chiusa::Aperta::rest
  | Chiusa::Aperta::rest -> Chiusa::Chiusa::rest
  | Aperta::rest -> Aperta::giraDopoChiusa rest
  | _ -> failwith "f$ck"

let rec nodi n = 
  if n = 0 then [[]] else
    let call = nodi (n-1) in 
    List.map (List.cons Aperta) call @ List.map (List.cons Chiusa) call

let successoriCassaforte clist = [(giraPrima clist)] @ (try [giraDopoChiusa clist] with _ -> [] )
let archi n = let l = nodi n in List.map (function x -> (x, successoriCassaforte x)) l

let rec start n = if n = 0 then [] else Chiusa :: start (n-1)
let aperta cf = List.for_all (function x -> x = Aperta) cf 

let apriVis n = 
  let start = start n in 
  let rec from_node visited n =
    if List.mem n visited then failwith "loop"
    else if aperta n then [n]
    else n::from_list (n::visited) (successoriCassaforte n)

  and from_list visited = function
      [] -> failwith "Nope"
    | x::rest -> try from_node visited x with _ -> from_list visited rest
  in from_node [] start

let depth_first_collect g start = 
  let rec aux visited = function
      [] -> visited
    | x::rest -> (if List.mem x visited then aux visited rest 
                  else aux (x::visited) (successori g x @ rest))
  in aux [] [start]

let breadth_first_collect g start = 
  let rec aux visited = function
      [] -> visited
    | x::rest -> (if List.mem x visited then aux visited rest 
                  else aux (x::visited) (rest @ successori g x ))
  in aux [] [start]

let rec listfrom a = function
    [] -> []
  | x::rest -> if x = a then a::rest else listfrom a rest

let gl:'a graph = [(1,2);(2,3);(3,4);(4,5);(5,6);(6,3)]
let g2:'a graph = [(1,2);(2,3);(3,4);(4,5);(5,6)]
let g3:'a graph = [(1,3);(2,6);(3,4);(3,5);(3,6);(4,2);(4,5);(5,4);(6,5)]

let ciclo2 g start = 
  let rec fromnode visited node = 
    if List.mem node visited then listfrom node (List.rev (node::visited)) else
      fromlist (node::visited) (successori g node)
  and fromlist visited = function
      [] -> failwith "nope"
    | x::rest -> try fromnode visited x with _ -> fromlist visited rest
  in fromnode [] start

let pari x = (x mod 2 = 0)

let path_n_p g p n start = 
  let rec fromnode visited n node = 
    if List.mem node visited then failwith "loop"
    else if n = 0 then [] 
    else node:: fromlist (node::visited) (if p node then (n-1) else n) (successori g node)
  and fromlist visited n = function
      [] -> failwith "nope"
    | x::rest -> try fromnode visited n x with _ -> fromlist visited n rest
  in fromnode [] n start

let rec listfrom a = function
    [] -> []
  | x::rest -> if x = a then a::rest else listfrom a rest

let ciclo3 g start =   
  let rec fromnode visited node = 
    if List.mem node visited then  listfrom node (List.rev (node::visited))
    else fromlist (node::visited) (successori g node)
  and fromlist visited = function
      [] -> failwith "nope"
    | x::rest -> try fromnode visited x with _ -> fromlist visited rest
  in fromnode [] start

(* 
test_connessi g 1 6;;
esiste_ciclo g 4;;
ciclo g 4;;
ciclo2 gl 1;;
ciclo3 gl 1;;
ciclo2 g2 1;;
ciclo3 g2 1;;
path_n_p g3 pari 2 1;;
#use "Esercizi/es10.ml";; 
*)