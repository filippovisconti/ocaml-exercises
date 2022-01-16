(*let rec sb(lower, upper) = 
if lower > upper then 0
else sb(lower+1, upper) + lower ;;*)

(*let times m n :int = m * n;;

print_int (times 3 4);;*)

let y = [1;2;3;4];;

let rec listLenght l = 
    begin match l with
        | [] -> 0
        | h::t -> 1 + (listLenght t)
    end;;
print_int (listLenght y);;