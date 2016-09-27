(* https://projecteuler.net/problem=1 *)

let divisible_by_three x =
        x mod 3 = 0 ;;

let divisible_by_five x =
        x mod 5 = 0 ;;

let divisible_by_three_or_five x = 
        divisible_by_three x || divisible_by_five x ;;

let rec range a b =
        if a >= b then [] else a :: range (a+1) b ;;

let rec sum cond l =
        match l with
        | [] -> 0
        | hd :: tl -> if cond hd then hd + sum cond tl else sum cond tl
;;

print_int (sum divisible_by_three_or_five (range 1 1000)) ;;
