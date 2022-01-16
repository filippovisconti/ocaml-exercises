type metro = (int * int * string) list

let m:metro = [(1,2,"A"); (2,3,"A"); (3,1,"A"); (2,4,"B"); (4,5,"B"); (4,6,"C"); (6,3,"C"); (5,7,"D"); (6,7,"D")]
let setadd x lst= if List.mem x lst then lst else x::lst 
let line (metro:metro) l =
  let rec aux metro res = match metro with
      [] -> res
    | (a,b,ln)::rest -> if ln = l then
        setadd a (setadd b res)
      else aux rest res
  in aux metro []

let rec vicini staz (m:metro) = match m with
    [] -> []
  | (a,b,ln)::rest -> (if a = staz then setadd (b,ln) (vicini staz rest)
                       else if b = staz then setadd (a,ln) (vicini staz rest)
                       else (vicini staz rest))

let raggiungi (m:metro) (maxc:int) (start:int) (goal:int) = 
  let rec from_node vis n cn ln = 
    if List.mem n vis then failwith "looop"
    else if cn < 0 then failwith "troppi cambi" 
    else if n = goal then [goal]
    else n::from_list vis cn ln (vicini n m)
  and from_list vis cn ln= function
      [] -> failwith "nope"
    | (x,l)::rest -> try from_node vis x (if l = ln then cn else (cn-1)) ln with _ -> from_list vis cn ln rest
  in from_node [] start (maxc +1) ""


(* #use "EsamiPassati/0220.ml";;
*)