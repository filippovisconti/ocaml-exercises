type 'a ntree = Tr of 'a * 'a ntree list

(*
let rec creaAlbero n = 
  let rec postorder (Tr(t,tlist)) = match tlist with
      [] -> []
    | tree -> postlist tree @ [t]
  and postlist = function
      [] -> [] 
    | t::rest -> postorder t @ postlist rest*)

let rec inorder (Tr(t,tlist)) = match tlist with
    [] -> [t]
  | tree::rest -> inorder tree @ [t] @ inlist rest
and inlist = function 
    [] -> []
  | x::rest -> inorder x @ inlist rest

let rec foglie_in_lista lst (Tr(t,tlist)) = match tlist with
    [] -> List.mem t lst
  | tree -> flist lst tree
and flist lst = function 
    [] -> true
  | x::rest -> foglie_in_lista lst x && flist lst rest

(* applicata a un albero, riporta il numero delle foglie *)
let rec num_di_foglie (Tr(t,tlist)) = match tlist with
    [] -> 1
  | tlist -> numlist tlist
(* 'a ntree list -> int | restituisce la somma dei numeri delle foglie degli alberi contenuti nella lista passata *)
and numlist = function
    [] -> 0
  | x::rest -> num_di_foglie x + numlist rest
let rec discendenti n ((Tr(x,tlist)) as t) = 
  if x = n then inorder t else dislist n tlist
and dislist n = function
    [] -> []
  | t::rest -> discendenti n t @ (dislist n rest)

let rec path_non_pred p (Tr(x,tlist)) = 
  if p x then failwith "f&ck" 
  else if tlist = [] then [x] 
  else x::ramolist p tlist

and ramolist p = function
    [] -> failwith "b!5c$"
  | t::rest -> try path_non_pred p t with _ -> ramolist p rest

let rec same_structure (Tr(_,tl1)) (Tr(_,tl2)) = samelst (tl1, tl2) 
and samelst = function
    ([],[]) -> true
  | (t1::re1,t2::re2) -> same_structure t1 t2 && samelst (re1,re2)
  | _ -> false

let rec same_structure2 (Tr(_,tl1)) (Tr(_,tl2)) = 
  try List.for_all (function (t,u) -> same_structure2 t u) (List.combine tl1 tl2)
  with _ -> false

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
let root (Tr(t,tl)) = t

let rec n_ramo k (Tr(t,tl)) = match tl with
    [] -> if k = t then [t] else failwith "noope"
  | _ -> t::nramolist (k-t) tl
and nramolist k = function
    [] -> failwith "NoMoreTrees"
  | t::trest -> try n_ramo k t with _ -> nramolist k trest

let rec labels (Tr(t,tl)) = match tl with
    [] -> [t]
  | _-> t::labelist tl 
and labelist = function
    [] -> []
  | x::rest -> labels x @ labelist rest 

let rec discendenti x (Tr(t,tl)) =  
  if x = t then labels (Tr(t,tl)) 
  else dislist x tl
and dislist k = function
    [] -> []
  | x::rest -> discendenti k x @ dislist k rest

let max_coppia (x, y) (z, w) =
  if y > w then (x, y) else (z, w)

let rec foglia_costo (Tr(t,tl)) = match tl with 
    [] -> (t,t)
  | _ -> let (f, c) = fclist tl in (f, c+t)
and fclist = function
    [] -> failwith ""
  | [x] -> foglia_costo x
  | x::rest -> max_coppia (fclist rest) (foglia_costo x)
let rec tutte_foglie_costi (Tr(t,tl)) = match tl with
    [] -> [(t,t)]
  | _ -> List.map (function (x,y) -> (x,y+t)) (tfclist tl)
and tfclist = function
    [] -> failwith " "
  | [x] -> tutte_foglie_costi x 
  | x::rest -> tutte_foglie_costi x @ tfclist rest


let rec isPrimo num k = if (k == 0 || k == 1) then true else if num mod k == 0 && num <> k then false else isPrimo num (k-1) 
let verificaPrimo n = isPrimo n n
let rec ramo_di_primi (Tr(t,tl)) = 
  if verificaPrimo t then t::rdplist tl else failwith "nop2"
and rdplist = function
    [] -> []
  | x::rest -> try ramo_di_primi x with _ -> rdplist rest 

let rec listaGuida lst (Tr(t,tl)) = match tl with 
    [] -> failwith "nope"
  | _ -> listaGuida (List.tl lst) (List.nth tl (List.hd lst))

let rec remove x = function
    [] -> []
  | y::rest -> if x = y then remove x rest else y::remove x rest

let rec ramo_da_lista (Tr(t,tl)) lst k = match tl with
    [] -> if lst = [k] && t = k then [t] else raise Not_found 
  | _ -> if List.mem t lst then t::rdlist (remove t lst) k tl else failwith "nopeE"
and rdlist lst k = function
    [] -> failwith "nopeI"
  | x::rest ->try ramo_da_lista x lst k with _ -> rdlist lst k rest

let rec rami (Tr(t,tl)) = match tl with
    [] -> [[t]]
  | _ -> List.map (List.cons t) (ramilist tl)
and ramilist = function
    [] -> []
  | x::rest -> rami x @ ramilist rest


let rec path_non_pred p (Tr(t,tl)) = if p t then failwith "nop1" else
    t::pnplist p tl
and pnplist p = function
    [] -> []
  | x::rest -> try path_non_pred p x with _ -> pnplist p rest 


let leaf x = Tr(x,[])
let albero = Tr(1,[Tr(2,[Tr(3,[leaf 4;
                               leaf 5]);
                         Tr(6,[leaf 7]);
                         leaf 8]);
                   leaf 9;
                   Tr(10,[Tr(11,[leaf 12;
                                 leaf 13;
                                 leaf 14]);
                          leaf 15;
                          Tr(16,[leaf 17;
                                 Tr(18,[leaf 19;
                                        leaf 20])])])])

let rec aggiungiSenzaRep (x,y) lst = if List.mem (x,y) lst then lst else (x,y)::lst
let rec union lst1 = function
    [] -> lst1
  | x::rest -> union (aggiungiSenzaRep x lst1) rest

let rec preorder (Tr(t,tl)) = match tl with
    [] -> [t]
  | _ -> [t] @ prelist tl
and prelist = function
    [] -> []
  | x::rest -> preorder x @ prelist rest

let rec postorder (Tr(t,tl)) = match tl with
    [] -> [t]
  | _ -> postlist tl @ [t]

and postlist = function
    [] -> []
  | x::rest -> postorder x @ postlist rest

let rec num_di_foglie2 (Tr(t,tl)) = match tl with
    [] -> 1
  | _ -> ndflist tl
and ndflist = function
    [] -> 0
  | x::rest -> num_di_foglie2 x + ndflist rest

let rec foglia_costo2 (Tr(t,tl)) = match tl with
    [] -> (t,t)
  | _ -> let (f,c) = focolist tl in (f,c+t)
and focolist = function
    [] -> (0,0)
  | x::rest -> max_coppia (foglia_costo2 x) (focolist rest)

let rec same_structure3 t1 t2 = match (t1,t2) with
    (Tr(_,tl1),Tr(_,tl2)) -> smlist (tl1,tl2)
and smlist = function
    ([],[]) -> true
  |(a::r1,b::r2) -> same_structure3 a b && smlist (r1,r2)
  | _ -> false

let rec tutte_foglie_costi2 (Tr(t,tl)) = match tl with
    [] -> [(t,t)]
  | _ -> List.map (function (f,c) -> (f,c+t)) (tfclist tl)
and tfclist = function
    [] -> []
  | x::rest -> tutte_foglie_costi2 x @ tfclist rest
let rec path_non_pred2 p (Tr(t,tl)) = 
  if p t then failwith "nope" else t::if tl = [] then [] else pnplist2 p tl
and pnplist2 p = function
    [] -> failwith "nope2"
  | x::rest -> try path_non_pred2 p x with _ -> pnplist2 p rest 



(* 
#use "Esercizi/es09.ml";;
 ramo_da_lista albero [1;2;3;4] 4;;
 tutte_foglie_costi2 albero;;
 tutte_foglie_costi albero;;
 path_non_pred2 (function x -> true) albero ;;
 path_non_pred (function x -> x > 30) albero ;;
  path_non_pred (function x -> x mod 2 = 0) albero ;;
  *)