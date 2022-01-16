(*  elimina: string -> (string x string) list -> (string x string) list
    tale che elimina s pairlist = lista che si ottinee da pairlist eliminando le coppie costitute da due stringhe la cui concatenazione è uguale a s
*)

let rec elimina s = function
    [] -> []
  | (x,y)::rest -> if (x^y) = s then elimina s rest
    else (x,y)::(elimina s rest)

let eliminait s list = 
  (*loop: (string x string) list -> (string x string) list -> (string x string) list
    loop: res@elimina s list, res concat a list a cui sono state rimosse le occorrenze delle coppie di stringhe la cui concatenazione è uguale a s
  *)
  let rec loop lst res = match lst with
      [] -> res
    | (x,y)::rest -> if (x^y) = s then loop rest res 
      else loop rest ((x,y)::res)
  in loop list []

let eliminafilter s lista =
  List.filter (function (x,y) -> x^y <> s ) lista
let s = "pippo"
let list = [("as","er");("dfr","gb");("pi","ppo");("ds","e33r");("","pippo");("aec","rd")]

(* #use "autoval.ml";; *)