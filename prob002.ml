(* https://projecteuler.net/problem=2 *)

let fibonacci_max = 4_000_000 ;;

let rec fib a b max = 
        if (a+b) < max then a :: (fib b (a+b) max) else [a;b] ;;

let rec print_list l =
        match l with
        | [] -> ()
        | hd :: tl -> 
                   print_int(hd); print_string(" ");
                   print_list(tl) ;;
let is_even a = 
        a mod 2 = 0 ;;

let rec sum cond l =
        match l with
        | [] -> 0
        | hd :: tl -> if cond hd then hd + sum cond tl else sum cond tl
;;

print_int (sum is_even (fib 1 2 fibonacci_max) ) ;;
