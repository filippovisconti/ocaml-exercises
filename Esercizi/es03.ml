let oraValida (h, m) = ((h>=0) && (h<=23) && (m>=0) && (m<=59))

exception OraNonValida
let somma_ore ((h1,m1) as ora1) ((h2,m2) as ora2) =
  if not (oraValida ora1 && oraValida ora2) then raise OraNonValida
  else ((h1+h2+ ((m1+m2))/60 ) mod 24, (m1+m2) mod 60)

let find_max list =
  let rec loop list temp = 
    match list with
      []->temp
    |x::rest -> if x > temp then loop list x
      else loop rest temp 
  in loop (List.tl list) (List.hd list) 

let find_min list =
  let rec loop list temp = 
    match list with
      []->temp
    |x::rest -> if x < temp then loop list x
      else loop rest temp 
  in loop (List.tl list) (List.hd list) 
let read_max () =
  let rec loop list =
    try 
      let s = read_int ()
      in loop (s::list)
    with _ -> list
  in find_max(loop []) 

let print_max_min list = 
  (find_max list,find_min list)


let read_max_min () =

  let rec loop list =
    try 
      let s = read_int ()
      in loop (s::list)
    with _ -> list
  in print_max_min(loop []) 

let rec numero_somma () =
  let s = read_line() in
  if s="."  (* terminato? *)
  then (0,0) (* nessun numero letto, somma 0 *)
  else (* s rappresenta un int, leggi gli altri numeri *)
    let (tot,somma) = numero_somma()
    in (tot+1,somma+(int_of_string s))

let rec sonoTuttiMinori n list = match list with
    [] -> true
  | x::rest -> (not (x > n)) && (sonoTuttiMinori n rest)


let tutti_minori n =
  let rec loop list = 
    try 
      let s = read_int ()
      in loop (s::list)
    with _ -> list
  in sonoTuttiMinori n (loop [])

let conta n lista =
  let rec loop n lista res = match lista with
      [] -> res
    |h::t -> if (h = n) then loop n t (res + 1) else loop n t res
  in loop n lista 0

let occorre n = 
  let rec loop list = 
    try 
      let s = read_int ()
      in loop (s::list)
    with _ -> list
  in conta n (loop [])

exception EmptyString
let num_di_stringhe () =
  let rec loop list = 
    try 
      let s = read_line ()
      in if s = "" then raise EmptyString else loop (s::list)
    with _ -> list
  in List.length (loop [])

let find_max_length list =
  let rec loop list temp res = 
    match list with
      []->res
    |x::rest -> let l = (String.length x) in
      if l > temp then loop list l x
      else loop rest temp res
  in loop (List.tl list) (String.length (List.hd list)) (List.hd list) 

let stringa_max () = 
  let rec loop list = 
    try 
      let s = read_line ()
      in if s = "" then raise EmptyString else loop (s::list)
    with _ -> list
  in find_max_length (loop [])

let rec sumbetween n m = 
  if n > m then 0
  else n + sumbetween (n+1) m

let sumto m = sumbetween 0 m

let rec power n k = 
  if k = 0 then 1
  else n*(power n (k-1))

let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else fib (n-1) + fib (n-2)

let maxstring s =
  let rec loop string lenght i max char = 
    if string="" then raise EmptyString else
    if i >= lenght then char
    else  
      let temp  = Char.code (string.[i]) 
      in if temp > max 
      then loop string lenght (i+1) temp string.[i]
      else loop string lenght (i+1) max char
  in loop s (String.length s) 0 (Char.code (s.[0])) (s.[0])

let esegui () =
  begin
    print_char (maxstring "abcde");
    print_newline ();
    print_char (maxstring "fgh");
    print_newline ()
  end
  (*
   #use "es03.ml";;
   *)