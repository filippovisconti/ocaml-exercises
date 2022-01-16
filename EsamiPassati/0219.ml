(* Sia data la seguente definizione di tipo: 
   type color = Rosso | Verde | Neutro
   Per rappresentare i colori di un insieme di elementi (di tipo α) utilizziamo una lista associativa di tipo (α × color) list, 
   contenente coppie (e,c) 
   dove c può essere soltanto Rosso o Verde: 
   per convenzione, gli elementi a cui non è associato alcun valore nella lista hanno colore Neutro. Scrivere una funzione
   conta_colori: 
   (α × color) list → α list → (color × int) list
   tale che conta_colori cols lista riporti la lista con 3 coppie [(Rosso,n);(Verde,m); (Neutro,k)] dove n è il numero di elementi rossi in lista, m il numero di quelli verdi e k il numero di quelli neutri. 
   Ad esempio, se cols è la lista [(2,Rosso); (3,Verde); (4,Verde); (6,Verde); (7,Rosso)], allora conta_colori cols [1;2;3;4;5;6;7;8;9;10] riporterà la lista [(Rosso, 2); (Verde, 3); (Neutro, 5)].
*)

type color = Rosso | Verde | Neutro
let cols = [(2,Rosso); (3,Verde); (4,Verde); (6,Verde); (7,Rosso)]
let lista = [1;2;3;4;5;6;7;8;9;10]

let rec conta_colori cols = function 
    [] -> [(Rosso,0);(Verde,0); (Neutro,0)]
  | x::rest -> 
    let tmp = try
        List.assoc x cols 
      with _ -> Neutro in 
    let [(Rosso,n);(Verde,m); (Neutro,k)] = conta_colori cols rest in
    match tmp  with
      Rosso -> [(Rosso,n+1);(Verde,m); (Neutro,k)]
    | Verde -> [(Rosso,n);(Verde,m+1); (Neutro,k)]
    | Neutro ->[(Rosso,n);(Verde,m); (Neutro,k+1)]

(* trova: 'a*color list -> 'a -> color
    restituisce il colore associato alla chiave passata
*)
let rec trova1 cols y = match cols with
    [] -> Neutro
  | (x,col)::rest -> if x = y then col 
    else trova1 rest y

let trova2 cols y = try
    List.assoc y cols
  with _ -> Neutro

let findOrFail (cols:('a * color) list) x = List.assoc x cols

let conta_colori_it cols lista = 
  (* 'a list -> int ->  int ->  int -> (color*int) list
      loop: valore di [(Rosso, r+x);(Verde, v+y); (Neutro,n+z)] dove x,y,z = num di elementi rosso,verde,neutro
  *)
  let rec loop lista r v n = 
    match lista with   
      [] -> [(Rosso, r);(Verde, v); (Neutro,n)]
    | x::rest -> match trova2 cols x with
        Rosso  -> loop rest (r+1) v n
      | Verde  -> loop rest r (v+1) n
      | Neutro -> loop rest r v (n+1)
  in loop lista 0 0 0

type 'a graph = ('a * 'a) list

let rec vicini node = function
    [] -> []
  | (x,y)::rest -> (if x = node then y::vicini node rest 
                    else if y = node then x::vicini node rest 
                    else vicini node rest) 

let path (g: 'a graph) (colors:('a * color) list) lst start = 
  let rec from_node visited lst node = 
    if List.mem node visited then failwith "loop"
    else if lst = [] then []
    else node:: (from_list (node::visited) 
                   (if List.hd lst = findOrFail colors node then List.tl lst 
                    else lst) 
                   (vicini node g) )
  and from_list visited lst = function
      [] -> failwith "nop3"
    | x::rest -> try from_node visited lst x with _ -> from_list visited lst rest 
  in from_node [] lst start



(* #use "EsamiPassati/0219.ml";;
*)
