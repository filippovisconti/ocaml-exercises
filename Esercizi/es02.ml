(* ultime cifre: int -> ( int * int )*)
(* riporta le ultime due cifre di un intero *)

let ultime_cifre n =
    try let numero = abs n 
        in let riduci numero =  
            if numero > 10 then (((numero mod 100 - numero mod 10)/10), numero mod 10)
            else (0, numero mod 10)
        in riduci numero
    with _-> (0,0)

(* bello: int -> bool *) 
let bello n =
    let ultime = ultime_cifre n
    in let numBello m = 
        m == 0 || m == 3 || m == 7
    in if n >= 10 then (numBello (snd ultime) && not (numBello (fst ultime)))
        else numBello (snd ultime)
    
(* data: int * string -> bool *)
let data (d, m) =
    (*let goodData d inf sup =
        d >= inf and d <= sup
    in *)match m with
      "febbraio" -> (d > 0) && (d < 28)
    | "novembre" | "aprile"| "giugno"| "settembre" -> (d > 0 && d < 30)
    | "gennaio"| "marzo"| "maggio"| "luglio"| "agosto"| "ottobre"| "dicembre" -> (d > 0 && d < 31)
    | _ -> false
