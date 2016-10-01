(* https://projecteuler.net/problem=1 *)

open Core.Std ;;

let divisible_by_three_or_five x = 
        x mod 3 = 0 || x mod 5 = 0 ;;

let nums = List.filter ~f:(divisible_by_three_or_five) (List.range 1 1000) ;;

let sum = List.reduce ~f:(+) nums ;;

match sum with
| None -> printf "None\n"
| Some x -> printf "%d\n" x
;;
