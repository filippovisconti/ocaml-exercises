(* attraversamento di un labirinto, da una casella di entrata a una casella di uscita, senza passare per caselle che contengono un mostro, e raccogliendo tutti gli oggetti che si trovano nelle altre caselle (se ce ne sono) *)

type content = Mostro | Obj of string
type 'a contents = ('a * content list) list

(** vicini in un grafo non orientato **)
(*  vicini : 'a -> 'a graph -> 'a list  *)
(* vicini x g = lista dei vicini di x in g (non orientato) *)
let rec vicini nodo = function 
    [] -> []
  | (x,y)::rest ->
    if x = nodo then y::vicini nodo rest
    else if y = nodo then x::vicini nodo rest
    else vicini nodo rest

let contents =
  [(1,[Obj "oro"]); (2,[Mostro]); (4,[Obj "computer";Obj "penna"]);
   (7,[Mostro; Obj "libro"])]

(* un labirinto e' un: 'a graph * 'a contents *)
(*  grafo rappresentato come lista di archi*)
type 'a graph = ('a * 'a) list
type 'a labirinto = 'a graph * 'a contents

let content x contenuti = 
  try List.assoc x contenuti 
  with  Not_found -> []
let has_monster x contenuti = 
  List.mem Mostro (content x contenuti)

let path ((grafo,contenuti): 'a labirinto) ingresso uscita = 
  let rec cerca_da visited casella = 
    if List.mem casella visited || has_monster casella contenuti
    then raise Not_found
    else
    if casella = uscita then [casella] 
    else casella :: cerca_da_una_tra (casella::visited) (vicini casella grafo)

  and cerca_da_una_tra visited = function
      [] -> raise Not_found
    | x::rest -> try cerca_da visited x
      with Not_found -> cerca_da_una_tra visited rest
  in cerca_da [] ingresso


let path ((grafo,contenuti): 'a labirinto) ingresso uscita = 
  let rec cerca_da visited casella = 
    if List.mem casella visited || has_monster casella contenuti
    then raise Not_found
    else
    if casella = uscita then ([casella], content casella contenuti )
    else let (cammino, oggetti) = cerca_da_una_tra (casella::visited) (vicini casella grafo) in (casella :: cammino, (content casella contenuti) @ oggetti)

  and cerca_da_una_tra visited = function
      [] -> raise Not_found
    | x::rest -> try cerca_da visited x
      with Not_found -> cerca_da_una_tra visited rest
  in cerca_da [] ingresso
let path ((grafo,contenuti): 'a labirinto) ingresso uscita = 
  let rec cerca_da visited raccolti casella = 
    if List.mem casella visited || has_monster casella contenuti
    then raise Not_found
    else
    if casella = uscita then (List.rev (casella::visited), content casella contenuti @ raccolti)
    else cerca_da_una_tra (casella::visited) ((vicini casella grafo) @raccolti) (casella, (content casella contenuti))
  and cerca_da_una_tra visited = function
      [] -> raise Not_found
    | x::rest -> try cerca_da visited x
      with Not_found -> cerca_da_una_tra visited rest
  in cerca_da [] ingresso


(* -------------------------------------- *)

