(* https://projecteuler.net/problem=2 *)

open Core.Std ;;

let fibonacci_max = 4_000_000 ;;

let rec fib a b max = 
        if (a+b) < max then a :: (fib b (a+b) max) else [a;b] ;;

let is_even a = 
        a mod 2 = 0 ;;

let sum = List.reduce ~f:(+) (List.filter ~f:(is_even) (fib 1 2 fibonacci_max)) ;;

match sum with
| None -> printf "None\n"
| Some x -> printf "%d\n" x
;;
